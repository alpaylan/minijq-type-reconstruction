use crate::minijq::ast::Expr;
use crate::minijq::eval::{eval, RuntimeTypeError};
use crate::minijq::generator::generate_value;
use crate::minijq::types::{ObjectShape, Type};
use crate::minijq::typing::{infer_expr_scheme, infer_expr_type, TypeScheme};
use rand::Rng;

#[derive(Clone, Debug)]
pub struct ReconstructionConfig {
    pub max_iterations: usize,
    pub samples_per_iteration: usize,
    pub max_value_depth: usize,
    pub required_success_rounds: usize,
}

impl Default for ReconstructionConfig {
    fn default() -> Self {
        ReconstructionConfig {
            max_iterations: 50,
            samples_per_iteration: 32,
            max_value_depth: 4,
            required_success_rounds: 2,
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
    pub failure: Option<RuntimeTypeError>,
}

#[derive(Clone, Debug)]
pub struct ReconstructionResult {
    pub inferred_scheme: TypeScheme,
    pub final_input_type: Type,
    pub converged: bool,
    pub traces: Vec<IterationTrace>,
}

pub fn reconstruct_input_type(
    program: &Expr,
    config: &ReconstructionConfig,
    rng: &mut impl Rng,
) -> ReconstructionResult {
    let scheme = infer_expr_scheme(program);
    let mut candidate = Type::Any
        .intersect(&scheme.input.erase_generics())
        .normalize();

    let mut traces = Vec::new();
    let mut stable_rounds = 0;
    let mut converged = false;

    for iteration in 0..config.max_iterations {
        let inferred_output = infer_expr_type(program, &candidate);
        let before = candidate.clone();
        let mut tested_samples = 0;
        let mut failure: Option<RuntimeTypeError> = None;

        for _ in 0..config.samples_per_iteration {
            let Some(input) = generate_value(&candidate, rng, config.max_value_depth) else {
                break;
            };
            tested_samples += 1;

            if let Err(err) = eval(program, &input) {
                failure = Some(err);
                break;
            }
        }

        if let Some(err) = failure.clone() {
            let to_subtract = if err.actual == err.input_type {
                broaden_to_root_kind(&err.input_type)
            } else {
                err.input_type.clone()
            };
            let next = candidate.subtract(&to_subtract).normalize();
            traces.push(IterationTrace {
                iteration,
                candidate_before: before,
                candidate_after: next.clone(),
                tested_samples,
                inferred_output,
                subtracted_type: Some(to_subtract),
                failure: Some(err),
            });

            if next == candidate {
                break;
            }
            candidate = next;
            stable_rounds = 0;

            if candidate.is_never() {
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
                failure: None,
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
        converged,
        traces,
    }
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
        Type::Generic(_) => Type::Any,
        other => other.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::{reconstruct_input_type, ReconstructionConfig};
    use crate::minijq::ast::Expr;
    use crate::minijq::typing::Builtin;
    use crate::minijq::types::{ObjectShape, Type};
    use serde_json::json;
    use rand::rngs::StdRng;
    use rand::SeedableRng;

    #[test]
    fn reconstructs_number_for_add_self() {
        let expr = Expr::add(Expr::identity(), Expr::identity());
        let mut rng = StdRng::seed_from_u64(5);
        let config = ReconstructionConfig {
            max_iterations: 20,
            samples_per_iteration: 8,
            max_value_depth: 3,
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
            required_success_rounds: 2,
        };

        let result = reconstruct_input_type(&expr, &config, &mut rng);
        assert!(result.converged);
        assert_eq!(result.final_input_type, Type::NonEmptyArray(Box::new(Type::Any)));
    }

    #[test]
    fn reconstructs_length_domain() {
        let expr = Expr::builtin(Builtin::Length, Expr::identity());
        let mut rng = StdRng::seed_from_u64(17);
        let config = ReconstructionConfig {
            max_iterations: 30,
            samples_per_iteration: 8,
            max_value_depth: 3,
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
            required_success_rounds: 2,
        };

        let result = reconstruct_input_type(&expr, &config, &mut rng);
        assert!(result.converged);
        assert_eq!(result.final_input_type, Type::NonEmptyArray(Box::new(Type::Any)));
    }

    #[test]
    fn reconstructs_string_for_split() {
        let expr = Expr::builtin(Builtin::Split, Expr::literal(json!(",")));
        let mut rng = StdRng::seed_from_u64(47);
        let config = ReconstructionConfig {
            max_iterations: 30,
            samples_per_iteration: 8,
            max_value_depth: 3,
            required_success_rounds: 2,
        };

        let result = reconstruct_input_type(&expr, &config, &mut rng);
        assert!(result.converged);
        assert_eq!(result.final_input_type, Type::String);
    }
}
