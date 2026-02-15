use crate::minijq::ast::Expr;
use crate::minijq::eval::{RuntimeTypeError, eval};
use crate::minijq::generator::generate_value;
use crate::minijq::types::{ObjectShape, Type};
use crate::minijq::typing::{TypeScheme, infer_expr_scheme, infer_expr_type};
use rand::Rng;
use serde_json::Value;
use std::fmt::Write;

#[derive(Clone, Debug)]
pub struct ReconstructionConfig {
    pub max_iterations: usize,
    pub samples_per_iteration: usize,
    pub max_value_depth: usize,
    pub rejection_probe_samples: usize,
    pub required_success_rounds: usize,
}

impl Default for ReconstructionConfig {
    fn default() -> Self {
        ReconstructionConfig {
            max_iterations: 50,
            samples_per_iteration: 32,
            max_value_depth: 4,
            rejection_probe_samples: 12,
            required_success_rounds: 2,
        }
    }
}

impl ReconstructionConfig {
    pub fn strong() -> Self {
        ReconstructionConfig {
            max_iterations: 300,
            samples_per_iteration: 96,
            max_value_depth: 12,
            rejection_probe_samples: 36,
            required_success_rounds: 6,
        }
    }
}

#[derive(Clone, Debug)]
pub struct IterationTrace {
    pub iteration: usize,
    pub candidate_before: Type,
    pub candidate_after: Type,
    pub tested_samples: usize,
    pub inferred_output: Type,
    pub subtracted_type: Option<Type>,
    pub subset_marked: Option<Type>,
    pub probe_samples: usize,
    pub probe_failures: usize,
    pub failure: Option<RuntimeTypeError>,
    pub failure_input: Option<Value>,
}

#[derive(Clone, Debug)]
pub struct ReconstructionResult {
    pub inferred_scheme: TypeScheme,
    pub final_input_type: Type,
    pub subset_types: Vec<Type>,
    pub converged: bool,
    pub traces: Vec<IterationTrace>,
}

impl ReconstructionResult {
    pub fn annotated_input_type(&self) -> String {
        annotate_with_subsets(&self.final_input_type, &self.subset_types)
    }

    pub fn pretty_trace(&self) -> String {
        let mut out = String::new();
        let _ = writeln!(
            out,
            "Scheme: {} -> {}",
            self.inferred_scheme.input, self.inferred_scheme.output
        );
        let _ = writeln!(out, "Final input type: {}", self.annotated_input_type());
        let _ = writeln!(out, "Converged: {}", self.converged);

        if self.traces.is_empty() {
            let _ = writeln!(out, "No iterations were executed.");
            return out;
        }

        for trace in &self.traces {
            let _ = writeln!(out, "Iteration {}:", trace.iteration);
            let _ = writeln!(out, "  Candidate before: {}", trace.candidate_before);
            let _ = writeln!(out, "  Inferred output : {}", trace.inferred_output);
            let _ = writeln!(out, "  Tested samples  : {}", trace.tested_samples);

            match (
                &trace.failure,
                &trace.subtracted_type,
                &trace.subset_marked,
                &trace.failure_input,
            ) {
                (Some(err), Some(subtracted), _, Some(input)) => {
                    let _ = writeln!(out, "  Refinement      : yes");
                    let _ = writeln!(out, "  Counterexample  : {}", compact_json(input));
                    let _ = writeln!(out, "  Runtime error   : {}", err);
                    let _ = writeln!(
                        out,
                        "  Probe failures  : {}/{}",
                        trace.probe_failures, trace.probe_samples
                    );
                    let _ = writeln!(out, "  Subtracted type : {}", subtracted);
                    let _ = writeln!(out, "  Candidate after : {}", trace.candidate_after);
                }
                (Some(err), _, Some(subset), Some(input)) => {
                    let _ = writeln!(out, "  Refinement      : subset");
                    let _ = writeln!(out, "  Counterexample  : {}", compact_json(input));
                    let _ = writeln!(out, "  Runtime error   : {}", err);
                    let _ = writeln!(
                        out,
                        "  Probe failures  : {}/{}",
                        trace.probe_failures, trace.probe_samples
                    );
                    let _ = writeln!(out, "  Marked subset   : Subset<{}>", subset);
                    let _ = writeln!(out, "  Candidate after : {}", trace.candidate_after);
                }
                _ => {
                    let _ = writeln!(out, "  Refinement      : no");
                    let _ = writeln!(out, "  Candidate after : {}", trace.candidate_after);
                }
            }
        }

        out
    }
}

pub fn reconstruct_input_type(
    program: &Expr,
    config: &ReconstructionConfig,
    rng: &mut impl Rng,
) -> ReconstructionResult {
    let scheme = infer_expr_scheme(program);
    reconstruct_input_type_impl(program, scheme, config, rng, true)
}

pub fn reconstruct_input_type_with_scheme(
    program: &Expr,
    scheme: TypeScheme,
    config: &ReconstructionConfig,
    rng: &mut impl Rng,
) -> ReconstructionResult {
    reconstruct_input_type_impl(program, scheme, config, rng, true)
}

pub fn reconstruct_input_type_unbiased(
    program: &Expr,
    config: &ReconstructionConfig,
    rng: &mut impl Rng,
) -> ReconstructionResult {
    let scheme = infer_expr_scheme(program);
    reconstruct_input_type_impl(program, scheme, config, rng, false)
}

pub fn reconstruct_input_type_unbiased_with_scheme(
    program: &Expr,
    scheme: TypeScheme,
    config: &ReconstructionConfig,
    rng: &mut impl Rng,
) -> ReconstructionResult {
    reconstruct_input_type_impl(program, scheme, config, rng, false)
}

fn reconstruct_input_type_impl(
    program: &Expr,
    scheme: TypeScheme,
    config: &ReconstructionConfig,
    rng: &mut impl Rng,
    seed_from_scheme: bool,
) -> ReconstructionResult {
    let mut candidate = if seed_from_scheme {
        Type::Any
            .intersect(&scheme.input.erase_generics())
            .normalize()
    } else {
        Type::Any
    };

    let mut traces = Vec::new();
    let mut subset_types = Vec::new();
    let mut stable_rounds = 0;
    let mut converged = false;

    for iteration in 0..config.max_iterations {
        let inferred_output = infer_expr_type(program, &candidate);
        let before = candidate.clone();
        let mut tested_samples = 0;
        let mut failure: Option<RuntimeTypeError> = None;
        let mut failure_input: Option<Value> = None;

        for _ in 0..config.samples_per_iteration {
            let Some(input) = generate_value(&candidate, rng, config.max_value_depth) else {
                break;
            };
            tested_samples += 1;

            if let Err(err) = eval(program, &input) {
                failure = Some(err);
                failure_input = Some(input);
                break;
            }
        }

        if let Some(err) = failure.clone() {
            let rejected_region = if err.actual == err.input_type {
                if preserve_precise_failure_region(&err) {
                    err.input_type.clone()
                } else {
                    broaden_to_root_kind(&err.input_type)
                }
            } else {
                err.input_type.clone()
            };
            let (probe_samples, probe_failures) = probe_rejection_region(
                program,
                &rejected_region,
                config.rejection_probe_samples,
                config.max_value_depth,
                rng,
            );
            let total_probe_samples = 1 + probe_samples;
            let total_probe_failures = 1 + probe_failures;
            let all_probes_failed = total_probe_samples == total_probe_failures;

            let mut next = candidate.clone();
            let mut subtracted_type = None;
            let mut subset_marked = None;
            let mut changed = false;

            if all_probes_failed {
                let narrowed = candidate.subtract(&rejected_region).normalize();
                if narrowed != candidate {
                    next = narrowed;
                    subtracted_type = Some(rejected_region.clone());
                    changed = true;
                } else if record_subset_type(&mut subset_types, rejected_region.clone()) {
                    subset_marked = Some(rejected_region.clone());
                    changed = true;
                } else {
                    subset_marked = Some(rejected_region.clone());
                }
            } else {
                if record_subset_type(&mut subset_types, rejected_region.clone()) {
                    changed = true;
                }
                subset_marked = Some(rejected_region.clone());
            }

            traces.push(IterationTrace {
                iteration,
                candidate_before: before,
                candidate_after: next.clone(),
                tested_samples,
                inferred_output,
                subtracted_type,
                subset_marked,
                probe_samples: total_probe_samples,
                probe_failures: total_probe_failures,
                failure: Some(err),
                failure_input,
            });

            if next != candidate {
                candidate = next;
                stable_rounds = 0;
            } else if changed {
                stable_rounds = 0;
            } else {
                stable_rounds += 1;
            }

            if candidate.is_never() {
                converged = true;
                break;
            }
            if stable_rounds >= config.required_success_rounds {
                converged = true;
                break;
            }
        } else {
            traces.push(IterationTrace {
                iteration,
                candidate_before: before.clone(),
                candidate_after: before,
                tested_samples,
                inferred_output,
                subtracted_type: None,
                subset_marked: None,
                probe_samples: 0,
                probe_failures: 0,
                failure: None,
                failure_input: None,
            });

            if tested_samples == 0 {
                converged = true;
                break;
            }

            stable_rounds += 1;
            if stable_rounds >= config.required_success_rounds {
                converged = true;
                break;
            }
        }
    }

    ReconstructionResult {
        inferred_scheme: scheme,
        final_input_type: candidate,
        subset_types,
        converged,
        traces,
    }
}

fn compact_json(value: &Value) -> String {
    serde_json::to_string(value).unwrap_or_else(|_| value.to_string())
}

fn annotate_with_subsets(ty: &Type, subset_types: &[Type]) -> String {
    if subset_types.is_empty() {
        return ty.to_string();
    }

    let subset_types: Vec<Type> = subset_types
        .iter()
        .map(|ty| ty.clone().normalize())
        .collect();

    match ty.clone().normalize() {
        Type::Union(items) => {
            let parts = items
                .into_iter()
                .map(|item| {
                    if subset_types.iter().any(|subset| subset == &item) {
                        format!("Subset<{}>", item)
                    } else {
                        item.to_string()
                    }
                })
                .collect::<Vec<_>>();
            parts.join(" | ")
        }
        other => {
            if subset_types.iter().any(|subset| subset == &other) {
                format!("Subset<{}>", other)
            } else {
                other.to_string()
            }
        }
    }
}

fn record_subset_type(subset_types: &mut Vec<Type>, ty: Type) -> bool {
    let normalized = ty.normalize();
    if subset_types.iter().any(|existing| existing == &normalized) {
        false
    } else {
        subset_types.push(normalized);
        true
    }
}

fn probe_rejection_region(
    program: &Expr,
    rejected_region: &Type,
    samples: usize,
    max_value_depth: usize,
    rng: &mut impl Rng,
) -> (usize, usize) {
    let mut tested = 0;
    let mut failures = 0;

    for _ in 0..samples {
        let Some(input) = generate_value(rejected_region, rng, max_value_depth) else {
            break;
        };
        tested += 1;
        if eval(program, &input).is_err() {
            failures += 1;
        }
    }

    (tested, failures)
}

fn broaden_to_root_kind(ty: &Type) -> Type {
    match ty {
        Type::Tuple(items) => {
            if items.is_empty() {
                Type::Array(Box::new(Type::Any))
            } else {
                Type::NonEmptyArray(Box::new(Type::Any))
            }
        }
        Type::Array(_) => Type::Array(Box::new(Type::Any)),
        Type::NonEmptyArray(_) => Type::NonEmptyArray(Box::new(Type::Any)),
        Type::Object(_) => Type::Object(ObjectShape::open_any()),
        Type::Union(items) => Type::union(items.iter().map(broaden_to_root_kind)),
        Type::BoolLiteral(_) => Type::Bool,
        Type::NumberLiteral(_) => Type::Number,
        Type::StringLiteral(_) => Type::String,
        Type::Generic(_) => Type::Any,
        other => other.clone(),
    }
}

fn preserve_precise_failure_region(err: &RuntimeTypeError) -> bool {
    matches!(
        (&err.expected, &err.input_type),
        (Type::NonEmptyArray(_), Type::Tuple(items)) if items.is_empty()
    )
}

#[cfg(test)]
mod tests {
    use super::{ReconstructionConfig, reconstruct_input_type, reconstruct_input_type_unbiased};
    use crate::minijq::ast::Expr;
    use crate::minijq::types::{ObjectShape, Type};
    use crate::minijq::typing::Builtin;
    use rand::SeedableRng;
    use rand::rngs::StdRng;
    use serde_json::json;
    use std::collections::BTreeSet;

    #[test]
    fn reconstructs_number_for_add_self() {
        let expr = Expr::add(Expr::identity(), Expr::identity());
        let mut rng = StdRng::seed_from_u64(5);
        let config = ReconstructionConfig {
            max_iterations: 20,
            samples_per_iteration: 8,
            max_value_depth: 3,
            rejection_probe_samples: 8,
            required_success_rounds: 2,
        };

        let result = reconstruct_input_type(&expr, &config, &mut rng);
        assert!(result.converged);
        assert_eq!(result.final_input_type, Type::Number);
    }

    #[test]
    fn reconstructs_nonempty_array_for_first() {
        let expr = Expr::builtin(Builtin::First, Expr::identity());
        let mut rng = StdRng::seed_from_u64(9);
        let config = ReconstructionConfig {
            max_iterations: 20,
            samples_per_iteration: 8,
            max_value_depth: 3,
            rejection_probe_samples: 8,
            required_success_rounds: 2,
        };

        let result = reconstruct_input_type(&expr, &config, &mut rng);
        assert!(result.converged);
        assert_eq!(
            result.final_input_type,
            Type::NonEmptyArray(Box::new(Type::Any))
        );
    }

    #[test]
    fn reconstructs_length_domain() {
        let expr = Expr::builtin(Builtin::Length, Expr::identity());
        let mut rng = StdRng::seed_from_u64(17);
        let config = ReconstructionConfig {
            max_iterations: 30,
            samples_per_iteration: 8,
            max_value_depth: 3,
            rejection_probe_samples: 8,
            required_success_rounds: 2,
        };

        let result = reconstruct_input_type(&expr, &config, &mut rng);
        assert!(result.converged);
        assert_eq!(
            result.final_input_type,
            Type::union(vec![
                Type::String,
                Type::Array(Box::new(Type::Any)),
                Type::Object(ObjectShape::open_any()),
            ])
        );
    }

    #[test]
    fn reconstructs_string_for_startswith() {
        let expr = Expr::builtin(Builtin::StartsWith, Expr::literal(json!("pre")));
        let mut rng = StdRng::seed_from_u64(42);
        let config = ReconstructionConfig {
            max_iterations: 30,
            samples_per_iteration: 8,
            max_value_depth: 3,
            rejection_probe_samples: 8,
            required_success_rounds: 2,
        };

        let result = reconstruct_input_type(&expr, &config, &mut rng);
        assert!(result.converged);
        assert_eq!(result.final_input_type, Type::String);
    }

    #[test]
    fn reconstructs_object_or_array_for_has_string_key() {
        let expr = Expr::builtin(Builtin::Has, Expr::literal(json!("id")));
        let mut rng = StdRng::seed_from_u64(43);
        let config = ReconstructionConfig {
            max_iterations: 30,
            samples_per_iteration: 8,
            max_value_depth: 3,
            rejection_probe_samples: 8,
            required_success_rounds: 2,
        };

        let result = reconstruct_input_type(&expr, &config, &mut rng);
        assert!(result.converged);
        assert_eq!(
            result.final_input_type,
            Type::union(vec![
                Type::Object(ObjectShape::open_any()),
                Type::Array(Box::new(Type::Any)),
            ])
        );
    }

    #[test]
    fn reconstructs_number_for_abs() {
        let expr = Expr::builtin(Builtin::Abs, Expr::identity());
        let mut rng = StdRng::seed_from_u64(44);
        let config = ReconstructionConfig {
            max_iterations: 30,
            samples_per_iteration: 8,
            max_value_depth: 3,
            rejection_probe_samples: 8,
            required_success_rounds: 2,
        };

        let result = reconstruct_input_type(&expr, &config, &mut rng);
        assert!(result.converged);
        assert_eq!(result.final_input_type, Type::Number);
    }

    #[test]
    fn optional_access_keeps_any_input() {
        let expr = Expr::optional(Expr::field(Expr::identity(), "x"));
        let mut rng = StdRng::seed_from_u64(45);
        let config = ReconstructionConfig {
            max_iterations: 30,
            samples_per_iteration: 8,
            max_value_depth: 3,
            rejection_probe_samples: 8,
            required_success_rounds: 2,
        };

        let result = reconstruct_input_type(&expr, &config, &mut rng);
        assert!(result.converged);
        assert_eq!(result.final_input_type, Type::Any);
    }

    #[test]
    fn reconstructs_nonempty_array_for_min() {
        let expr = Expr::builtin(Builtin::Min, Expr::identity());
        let mut rng = StdRng::seed_from_u64(46);
        let config = ReconstructionConfig {
            max_iterations: 30,
            samples_per_iteration: 8,
            max_value_depth: 3,
            rejection_probe_samples: 8,
            required_success_rounds: 2,
        };

        let result = reconstruct_input_type(&expr, &config, &mut rng);
        assert!(result.converged);
        assert_eq!(
            result.final_input_type,
            Type::NonEmptyArray(Box::new(Type::Any))
        );
    }

    #[test]
    fn reconstructs_string_for_split() {
        let expr = Expr::builtin(Builtin::Split, Expr::literal(json!(",")));
        let mut rng = StdRng::seed_from_u64(47);
        let config = ReconstructionConfig {
            max_iterations: 30,
            samples_per_iteration: 8,
            max_value_depth: 3,
            rejection_probe_samples: 8,
            required_success_rounds: 2,
        };

        let result = reconstruct_input_type(&expr, &config, &mut rng);
        assert!(result.converged);
        assert_eq!(result.final_input_type, Type::String);
    }

    #[test]
    fn marks_subset_for_tonumber_strings() {
        let expr = Expr::builtin(Builtin::ToNumber, Expr::identity());
        let mut rng = StdRng::seed_from_u64(96);
        let config = ReconstructionConfig {
            max_iterations: 30,
            samples_per_iteration: 8,
            max_value_depth: 3,
            rejection_probe_samples: 8,
            required_success_rounds: 2,
        };

        let result = reconstruct_input_type(&expr, &config, &mut rng);
        assert!(result.converged);
        assert_eq!(
            result.final_input_type,
            Type::union(vec![Type::Number, Type::String])
        );
        assert_eq!(result.annotated_input_type(), "Number | Subset<String>");
        assert!(result.subset_types.iter().any(|ty| ty == &Type::String));
    }

    #[test]
    fn traces_include_counterexample_inputs() {
        let expr = Expr::builtin(
            Builtin::ToNumber,
            Expr::builtin(Builtin::ToString, Expr::identity()),
        );
        let mut rng = StdRng::seed_from_u64(99);
        let config = ReconstructionConfig {
            max_iterations: 30,
            samples_per_iteration: 8,
            max_value_depth: 3,
            rejection_probe_samples: 8,
            required_success_rounds: 2,
        };

        let result = reconstruct_input_type(&expr, &config, &mut rng);
        let refinement = result
            .traces
            .iter()
            .find(|trace| trace.failure.is_some())
            .expect("expected at least one failing iteration");
        assert!(
            refinement.failure_input.is_some(),
            "failing iteration should record input"
        );

        let formatted = result.pretty_trace();
        assert!(formatted.contains("Counterexample"));
        assert!(formatted.contains("Subtracted type") || formatted.contains("Marked subset"));
    }

    #[test]
    fn unbiased_start_matches_seeded_results_on_core_programs() {
        let config = ReconstructionConfig {
            max_iterations: 120,
            samples_per_iteration: 64,
            max_value_depth: 4,
            rejection_probe_samples: 16,
            required_success_rounds: 3,
        };

        let cases = vec![
            ("add_self", Expr::add(Expr::identity(), Expr::identity())),
            ("length", Expr::builtin(Builtin::Length, Expr::identity())),
            (
                "startswith",
                Expr::builtin(Builtin::StartsWith, Expr::literal(json!("pre"))),
            ),
            ("first", Expr::builtin(Builtin::First, Expr::identity())),
            (
                "strict_tonumber",
                Expr::builtin(Builtin::ToNumber, Expr::identity()),
            ),
        ];

        for (i, (name, expr)) in cases.into_iter().enumerate() {
            let seed = 50_000 + i as u64;
            let mut seeded_rng = StdRng::seed_from_u64(seed);
            let mut unbiased_rng = StdRng::seed_from_u64(seed);

            let seeded = reconstruct_input_type(&expr, &config, &mut seeded_rng);
            let unbiased = reconstruct_input_type_unbiased(&expr, &config, &mut unbiased_rng);

            assert!(
                seeded.converged,
                "seeded reconstruction did not converge for {name}"
            );
            assert!(
                unbiased.converged,
                "unbiased reconstruction did not converge for {name}"
            );
            assert_eq!(
                seeded.final_input_type, unbiased.final_input_type,
                "final input type mismatch for {name}"
            );

            let seeded_subsets: BTreeSet<String> = seeded
                .subset_types
                .iter()
                .map(ToString::to_string)
                .collect();
            let unbiased_subsets: BTreeSet<String> = unbiased
                .subset_types
                .iter()
                .map(ToString::to_string)
                .collect();
            assert_eq!(
                seeded_subsets, unbiased_subsets,
                "subset markers mismatch for {name}"
            );
        }
    }
}
