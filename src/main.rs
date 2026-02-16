use rand::SeedableRng;
use rand::rngs::StdRng;
use serde_json::Value;
use std::collections::BTreeMap;
use std::env;
use std::fmt::Write;
use std::fs;
use std::panic::{self, AssertUnwindSafe};
use std::path::{Path, PathBuf};
use typereconstruction::minijq::{
    CachedReconstruction, Expr, ReconstructionConfig, ReconstructionResult, Type, TypeCache,
    TypeScheme, all_examples, eval, infer_expr_scheme, infer_expr_type, infer_predicate_refinement,
    parse_definitions, parse_expr, reconstruct_input_type_unbiased_with_scheme,
    reconstruct_input_type_with_scheme, validate_input_against_reconstruction,
};

const DEFAULT_TYPE_CACHE_PATH: &str = ".minijq.typecache.mjqi";

#[derive(Clone, Debug)]
struct CliOptions {
    trace: bool,
    unbiased: bool,
    only_program: Option<String>,
    analyze_file: Option<String>,
    validate_filter: Option<String>,
    validate_input_json: Option<String>,
    validate_input_file: Option<String>,
    type_cache_path: Option<String>,
    refresh_type_cache: bool,
}

impl Default for CliOptions {
    fn default() -> Self {
        CliOptions {
            trace: false,
            unbiased: false,
            only_program: None,
            analyze_file: None,
            validate_filter: None,
            validate_input_json: None,
            validate_input_file: None,
            type_cache_path: Some(DEFAULT_TYPE_CACHE_PATH.to_string()),
            refresh_type_cache: false,
        }
    }
}

fn parse_cli_options() -> CliOptions {
    let mut options = CliOptions::default();
    let mut args = env::args().skip(1);

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--trace" => {
                options.trace = true;
            }
            "--unbiased" => {
                options.unbiased = true;
            }
            "--program" => {
                let Some(name) = args.next() else {
                    eprintln!("missing value for --program");
                    std::process::exit(2);
                };
                options.only_program = Some(name);
            }
            "--analyze-file" => {
                let Some(path) = args.next() else {
                    eprintln!("missing value for --analyze-file");
                    std::process::exit(2);
                };
                options.analyze_file = Some(path);
            }
            "--validate-filter" => {
                let Some(filter) = args.next() else {
                    eprintln!("missing value for --validate-filter");
                    std::process::exit(2);
                };
                options.validate_filter = Some(filter);
            }
            "--validate-input" => {
                let Some(input) = args.next() else {
                    eprintln!("missing value for --validate-input");
                    std::process::exit(2);
                };
                options.validate_input_json = Some(input);
            }
            "--validate-input-file" => {
                let Some(path) = args.next() else {
                    eprintln!("missing value for --validate-input-file");
                    std::process::exit(2);
                };
                options.validate_input_file = Some(path);
            }
            "--type-cache" => {
                let Some(path) = args.next() else {
                    eprintln!("missing value for --type-cache");
                    std::process::exit(2);
                };
                options.type_cache_path = Some(path);
            }
            "--no-type-cache" => {
                options.type_cache_path = None;
            }
            "--refresh-type-cache" => {
                options.refresh_type_cache = true;
            }
            other => {
                eprintln!("unknown argument: {other}");
                eprintln!(
                    "usage: cargo run -- [--trace] [--unbiased] [--program NAME] [--analyze-file PATH] [--validate-filter FILTER --validate-input JSON|--validate-input-file PATH] [--type-cache PATH | --no-type-cache] [--refresh-type-cache]"
                );
                std::process::exit(2);
            }
        }
    }

    if options.analyze_file.is_some() && options.only_program.is_some() {
        eprintln!("--program cannot be combined with --analyze-file");
        std::process::exit(2);
    }
    if options.validate_filter.is_some() {
        if options.only_program.is_some() || options.analyze_file.is_some() {
            eprintln!("--validate-filter cannot be combined with --program or --analyze-file");
            std::process::exit(2);
        }
        if options.validate_input_json.is_some() && options.validate_input_file.is_some() {
            eprintln!("use only one of --validate-input and --validate-input-file");
            std::process::exit(2);
        }
        if options.validate_input_json.is_none() && options.validate_input_file.is_none() {
            eprintln!("--validate-filter requires --validate-input or --validate-input-file");
            std::process::exit(2);
        }
    } else if options.validate_input_json.is_some() || options.validate_input_file.is_some() {
        eprintln!("--validate-input/--validate-input-file requires --validate-filter");
        std::process::exit(2);
    }

    options
}

fn runtime_informed_scheme(final_input_type: &Type, output_type: &Type) -> TypeScheme {
    TypeScheme {
        vars: Vec::new(),
        input: final_input_type.clone(),
        output: output_type.clone(),
    }
}

fn load_cache(path: &Path) -> TypeCache {
    TypeCache::load_or_default(path).unwrap_or_else(|err| {
        eprintln!(
            "warning: failed to load type cache `{}` ({err}); starting with empty cache",
            path.display()
        );
        TypeCache::new(path)
    })
}

fn catch_unwind_silent<T>(f: impl FnOnce() -> T) -> Result<T, String> {
    let previous_hook = panic::take_hook();
    panic::set_hook(Box::new(|_| {}));
    let outcome = panic::catch_unwind(AssertUnwindSafe(f));
    panic::set_hook(previous_hook);

    outcome.map_err(|payload| {
        if let Some(text) = payload.downcast_ref::<&str>() {
            format!("runtime panic: {text}")
        } else if let Some(text) = payload.downcast_ref::<String>() {
            format!("runtime panic: {text}")
        } else {
            "runtime panic: unknown panic payload".to_string()
        }
    })
}

#[derive(Clone, Debug)]
struct UnsupportedDef {
    key: String,
    line: usize,
    reason: String,
}

#[derive(Clone, Debug)]
struct ParsedDef {
    def: typereconstruction::minijq::Definition,
    key: String,
    expr: Expr,
}

fn resolve_named_calls(
    expr: &Expr,
    zero_arg_defs: &BTreeMap<String, Expr>,
    stack: &mut Vec<String>,
) -> Result<Expr, String> {
    match expr {
        Expr::Identity => Ok(Expr::identity()),
        Expr::Literal(value) => Ok(Expr::literal(value.clone())),
        Expr::Pipe(lhs, rhs) => Ok(Expr::pipe(
            resolve_named_calls(lhs, zero_arg_defs, stack)?,
            resolve_named_calls(rhs, zero_arg_defs, stack)?,
        )),
        Expr::Binary(op, lhs, rhs) => Ok(Expr::binary(
            *op,
            resolve_named_calls(lhs, zero_arg_defs, stack)?,
            resolve_named_calls(rhs, zero_arg_defs, stack)?,
        )),
        Expr::Unary(op, inner) => Ok(Expr::unary(
            *op,
            resolve_named_calls(inner, zero_arg_defs, stack)?,
        )),
        Expr::Builtin(op, inner) => Ok(Expr::builtin(
            *op,
            resolve_named_calls(inner, zero_arg_defs, stack)?,
        )),
        Expr::Call(name, arg) => {
            let resolved_arg = resolve_named_calls(arg, zero_arg_defs, stack)?;
            let Some(callee) = zero_arg_defs.get(name) else {
                return Err(format!("unknown filter `{name}`"));
            };

            if stack.iter().any(|item| item == name) {
                let mut cycle = stack.clone();
                cycle.push(name.clone());
                return Err(format!("recursive filter cycle: {}", cycle.join(" -> ")));
            }

            stack.push(name.clone());
            let resolved_callee = resolve_named_calls(callee, zero_arg_defs, stack)?;
            stack.pop();

            if matches!(resolved_arg, Expr::Identity) {
                Ok(resolved_callee)
            } else {
                Ok(Expr::pipe(resolved_arg, resolved_callee))
            }
        }
        Expr::Field(inner, name) => Ok(Expr::field(
            resolve_named_calls(inner, zero_arg_defs, stack)?,
            name.clone(),
        )),
        Expr::Index(inner, idx) => Ok(Expr::index(
            resolve_named_calls(inner, zero_arg_defs, stack)?,
            *idx,
        )),
        Expr::Lookup(inner, key) => Ok(Expr::lookup(
            resolve_named_calls(inner, zero_arg_defs, stack)?,
            resolve_named_calls(key, zero_arg_defs, stack)?,
        )),
        Expr::Optional(inner) => Ok(Expr::optional(resolve_named_calls(
            inner,
            zero_arg_defs,
            stack,
        )?)),
        Expr::TryCatch {
            try_expr,
            catch_expr,
        } => Ok(Expr::try_catch(
            resolve_named_calls(try_expr, zero_arg_defs, stack)?,
            resolve_named_calls(catch_expr, zero_arg_defs, stack)?,
        )),
        Expr::IfElse {
            cond,
            then_branch,
            else_branch,
        } => Ok(Expr::if_else(
            resolve_named_calls(cond, zero_arg_defs, stack)?,
            resolve_named_calls(then_branch, zero_arg_defs, stack)?,
            resolve_named_calls(else_branch, zero_arg_defs, stack)?,
        )),
        Expr::ArrayLiteral(items) => Ok(Expr::array(
            items
                .iter()
                .map(|item| resolve_named_calls(item, zero_arg_defs, stack))
                .collect::<Result<Vec<_>, _>>()?,
        )),
        Expr::ObjectLiteral(fields) => Ok(Expr::object(
            fields
                .iter()
                .map(|(k, v)| {
                    resolve_named_calls(v, zero_arg_defs, stack)
                        .map(|resolved| (k.clone(), resolved))
                })
                .collect::<Result<Vec<_>, _>>()?,
        )),
    }
}

fn analyze_definition_file(
    path: &Path,
    options: &CliOptions,
    config: &ReconstructionConfig,
) -> i32 {
    let input = match fs::read_to_string(path) {
        Ok(text) => text,
        Err(err) => {
            eprintln!("failed to read `{}`: {err}", path.display());
            return 1;
        }
    };

    let definitions = match parse_definitions(&input) {
        Ok(defs) => defs,
        Err(err) => {
            eprintln!("failed to parse definitions in `{}`: {err}", path.display());
            return 1;
        }
    };

    let mut output_path = PathBuf::from(path);
    output_path.set_extension("mjqi");

    let lookup_cache = if options.refresh_type_cache {
        None
    } else {
        Some(load_cache(&output_path))
    };
    let mut write_cache = TypeCache::new(&output_path);

    let mut unsupported = Vec::new();
    let mut parsed_defs = Vec::new();
    for def in definitions {
        let key = def.cache_key();
        match parse_expr(&def.body) {
            Ok(expr) => parsed_defs.push(ParsedDef { def, key, expr }),
            Err(err) => {
                unsupported.push(UnsupportedDef {
                    key,
                    line: def.line,
                    reason: format!("parse error: {err}"),
                });
            }
        }
    }

    let zero_arg_defs: BTreeMap<String, Expr> = parsed_defs
        .iter()
        .filter(|parsed| parsed.def.params.is_none())
        .map(|parsed| (parsed.def.name.clone(), parsed.expr.clone()))
        .collect();

    let mut analyzed = 0usize;
    let mut cache_hits = 0usize;
    let mut cache_misses = 0usize;

    for (idx, parsed) in parsed_defs.into_iter().enumerate() {
        let mut stack = vec![parsed.def.name.clone()];
        let expr = match resolve_named_calls(&parsed.expr, &zero_arg_defs, &mut stack) {
            Ok(expr) => expr,
            Err(reason) => {
                unsupported.push(UnsupportedDef {
                    key: parsed.key.clone(),
                    line: parsed.def.line,
                    reason: format!("unsupported filter reference: {reason}"),
                });
                continue;
            }
        };

        let baseline_scheme = infer_expr_scheme(&expr);
        let cached = lookup_cache
            .as_ref()
            .and_then(|cache| cache.lookup(&parsed.key, &parsed.def.body));

        let (result, source): (ReconstructionResult, &str) = if let Some(cached) = cached {
            cache_hits += 1;
            let inferred_scheme =
                runtime_informed_scheme(&cached.final_input_type, &cached.output_type);
            (
                ReconstructionResult {
                    inferred_scheme,
                    final_input_type: cached.final_input_type,
                    subset_types: cached.subset_types,
                    converged: cached.converged,
                    traces: Vec::new(),
                },
                "cache-reconstruction",
            )
        } else {
            cache_misses += 1;
            let reconstructed = catch_unwind_silent(|| {
                let mut rng = StdRng::seed_from_u64(20_000 + idx as u64);
                let mut result = if options.unbiased {
                    reconstruct_input_type_unbiased_with_scheme(
                        &expr,
                        baseline_scheme.clone(),
                        config,
                        &mut rng,
                    )
                } else {
                    reconstruct_input_type_with_scheme(
                        &expr,
                        baseline_scheme.clone(),
                        config,
                        &mut rng,
                    )
                };

                let output_type = infer_expr_type(&expr, &result.final_input_type).normalize();
                result.inferred_scheme =
                    runtime_informed_scheme(&result.final_input_type, &output_type);
                result
            });

            let result = match reconstructed {
                Ok(result) => result,
                Err(reason) => {
                    unsupported.push(UnsupportedDef {
                        key: parsed.key.clone(),
                        line: parsed.def.line,
                        reason,
                    });
                    continue;
                }
            };
            (result, "reconstructed")
        };

        analyzed += 1;
        write_cache.insert(
            &parsed.key,
            &parsed.def.body,
            CachedReconstruction {
                final_input_type: result.final_input_type.clone(),
                output_type: result.inferred_scheme.output.clone(),
                subset_types: result.subset_types.clone(),
                converged: result.converged,
            },
        );

        println!("Def: {}", parsed.key);
        println!(
            "  Baseline static scheme: {} -> {}",
            baseline_scheme.input, baseline_scheme.output
        );
        println!("  Runtime-informed scheme source: {source}");
        println!(
            "  Scheme: {} -> {}",
            result.inferred_scheme.input, result.inferred_scheme.output
        );
        if let Some(refinement) = infer_predicate_refinement(&expr, &result.final_input_type)
            && refinement.has_information() {
                let note = refinement.pretty();
                println!("  Predicate refinement: {}", note);
            }
        println!("  Final input type: {}", result.annotated_input_type());
        println!("  Converged: {}", result.converged);
        if options.trace {
            println!("  Trace:");
            if result.traces.is_empty() {
                println!("    Loaded from reconstruction cache; no dynamic iterations executed.");
            } else {
                for line in result.pretty_trace().lines() {
                    println!("    {line}");
                }
            }
        }
        println!();
    }

    if let Err(err) = write_cache.save() {
        eprintln!("failed to write `{}`: {err}", output_path.display());
        return 1;
    }

    if !unsupported.is_empty() {
        let mut file = match fs::read_to_string(&output_path) {
            Ok(contents) => contents,
            Err(err) => {
                eprintln!(
                    "failed to read `{}` for post-processing: {err}",
                    output_path.display()
                );
                return 1;
            }
        };
        let _ = writeln!(file);
        if !unsupported.is_empty() {
            let _ = writeln!(
                file,
                "# unsupported definitions (not analyzable by current minijq subset):"
            );
            for item in &unsupported {
                let reason = item.reason.replace('\n', " ");
                let _ = writeln!(file, "# - {} [line {}]: {}", item.key, item.line, reason);
            }
        }
        if let Err(err) = fs::write(&output_path, file) {
            eprintln!(
                "failed to append unsupported definitions to `{}`: {err}",
                output_path.display()
            );
            return 1;
        }
    }

    println!("Wrote analysis interface: {}", output_path.display());
    println!(
        "Analysis summary: analyzed {} definition(s), {} unsupported, {} cache hit(s), {} miss(es)",
        analyzed,
        unsupported.len(),
        cache_hits,
        cache_misses
    );

    0
}

fn run_examples(options: &CliOptions, config: &ReconstructionConfig) -> i32 {
    let mut matched = false;
    let mut cache_hits = 0usize;
    let mut cache_misses = 0usize;

    let mut type_cache = options
        .type_cache_path
        .as_ref()
        .map(|path| load_cache(Path::new(path)));
    let cache_enabled = type_cache.is_some();

    for (idx, example) in all_examples().into_iter().enumerate() {
        if let Some(name) = &options.only_program
            && example.name != name {
                continue;
            }
        matched = true;

        let baseline_scheme = infer_expr_scheme(&example.expr);
        let cached_analysis = if options.refresh_type_cache {
            None
        } else {
            type_cache
                .as_ref()
                .and_then(|cache| cache.lookup(example.name, example.source))
        };

        let (result, scheme_source): (ReconstructionResult, &str) = if let Some(cached) =
            cached_analysis
        {
            if cache_enabled {
                cache_hits += 1;
            }
            let inferred_scheme =
                runtime_informed_scheme(&cached.final_input_type, &cached.output_type);
            (
                ReconstructionResult {
                    inferred_scheme,
                    final_input_type: cached.final_input_type,
                    subset_types: cached.subset_types,
                    converged: cached.converged,
                    traces: Vec::new(),
                },
                "cache-reconstruction",
            )
        } else {
            if cache_enabled {
                cache_misses += 1;
            }

            let mut rng = StdRng::seed_from_u64(1_000 + idx as u64);
            let mut result = if options.unbiased {
                reconstruct_input_type_unbiased_with_scheme(
                    &example.expr,
                    baseline_scheme.clone(),
                    config,
                    &mut rng,
                )
            } else {
                reconstruct_input_type_with_scheme(
                    &example.expr,
                    baseline_scheme.clone(),
                    config,
                    &mut rng,
                )
            };

            let output_type = infer_expr_type(&example.expr, &result.final_input_type).normalize();
            result.inferred_scheme =
                runtime_informed_scheme(&result.final_input_type, &output_type);

            if let Some(cache) = type_cache.as_mut() {
                cache.insert(
                    example.name,
                    example.source,
                    CachedReconstruction {
                        final_input_type: result.final_input_type.clone(),
                        output_type,
                        subset_types: result.subset_types.clone(),
                        converged: result.converged,
                    },
                );
            }

            let source = if options.refresh_type_cache {
                "recomputed"
            } else {
                "reconstructed"
            };
            (result, source)
        };

        println!("Program: {}", example.name);
        println!("  Desc: {}", example.description);
        println!("  Src : {}", example.source);
        println!("  Expr: {}", example.expr);
        println!(
            "  Baseline static scheme: {} -> {}",
            baseline_scheme.input, baseline_scheme.output
        );
        println!("  Runtime-informed scheme source: {scheme_source}");
        println!(
            "  Scheme: {} -> {}",
            result.inferred_scheme.input, result.inferred_scheme.output
        );
        if let Some(refinement) =
            infer_predicate_refinement(&example.expr, &result.final_input_type)
            && refinement.has_information() {
                println!("  Predicate refinement: {}", refinement.pretty());
            }
        println!("  Final input type: {}", result.annotated_input_type());
        if !result.subset_types.is_empty() {
            let subsets = result
                .subset_types
                .iter()
                .map(|ty| format!("Subset<{ty}>"))
                .collect::<Vec<_>>()
                .join(", ");
            println!("  Subset markers: {}", subsets);
        }
        println!("  Converged: {}", result.converged);

        if let Some(last) = result.traces.last() {
            match &last.failure {
                Some(err) => {
                    println!(
                        "  Last iteration {} failed after {} samples: {}",
                        last.iteration, last.tested_samples, err
                    );
                }
                None => {
                    println!(
                        "  Last iteration {} succeeded for {} samples",
                        last.iteration, last.tested_samples
                    );
                }
            }
        }

        if options.trace {
            println!("  Trace:");
            if result.traces.is_empty() {
                println!("    Loaded from reconstruction cache; no dynamic iterations executed.");
            } else {
                for line in result.pretty_trace().lines() {
                    println!("    {line}");
                }
            }
        }

        println!();
    }

    if let Some(cache) = type_cache.as_mut()
        && let Some(path) = options.type_cache_path.as_ref() {
            match cache.save_if_dirty() {
                Ok(saved) => {
                    if saved {
                        println!("Updated type cache: {path}");
                    }
                }
                Err(err) => {
                    eprintln!("warning: failed to save type cache `{path}`: {err}");
                }
            }
        }

    if cache_enabled && matched {
        println!("Type cache stats: {cache_hits} hit(s), {cache_misses} miss(es)");
    }

    if let Some(name) = &options.only_program
        && !matched {
            eprintln!("no example named `{name}`");
            return 1;
        }

    0
}

fn read_validation_input(options: &CliOptions) -> Result<Value, String> {
    if let Some(raw) = options.validate_input_json.as_ref() {
        return serde_json::from_str(raw)
            .map_err(|err| format!("invalid JSON provided to --validate-input: {err}"));
    }
    if let Some(path) = options.validate_input_file.as_ref() {
        let body = fs::read_to_string(path)
            .map_err(|err| format!("failed to read `{path}` for --validate-input-file: {err}"))?;
        return serde_json::from_str(&body)
            .map_err(|err| format!("invalid JSON in `{path}`: {err}"));
    }
    Err("missing validation input".to_string())
}

fn run_validation(options: &CliOptions, config: &ReconstructionConfig) -> i32 {
    let Some(filter) = options.validate_filter.as_ref() else {
        eprintln!("internal error: missing --validate-filter");
        return 2;
    };

    let expr = match parse_expr(filter) {
        Ok(expr) => expr,
        Err(err) => {
            eprintln!("failed to parse filter `{filter}`: {err}");
            return 2;
        }
    };
    let input = match read_validation_input(options) {
        Ok(value) => value,
        Err(err) => {
            eprintln!("{err}");
            return 2;
        }
    };

    let baseline_scheme = infer_expr_scheme(&expr);
    let cache_key = format!("validate::{filter}");
    let mut type_cache = options
        .type_cache_path
        .as_ref()
        .map(|path| load_cache(Path::new(path)));

    let cached_analysis = if options.refresh_type_cache {
        None
    } else {
        type_cache
            .as_ref()
            .and_then(|cache| cache.lookup(&cache_key, filter))
    };

    let result = if let Some(cached) = cached_analysis {
        let inferred_scheme =
            runtime_informed_scheme(&cached.final_input_type, &cached.output_type);
        ReconstructionResult {
            inferred_scheme,
            final_input_type: cached.final_input_type,
            subset_types: cached.subset_types,
            converged: cached.converged,
            traces: Vec::new(),
        }
    } else {
        let mut rng = StdRng::seed_from_u64(95_001);
        let mut reconstructed = if options.unbiased {
            reconstruct_input_type_unbiased_with_scheme(
                &expr,
                baseline_scheme.clone(),
                config,
                &mut rng,
            )
        } else {
            reconstruct_input_type_with_scheme(&expr, baseline_scheme.clone(), config, &mut rng)
        };
        let output_type = infer_expr_type(&expr, &reconstructed.final_input_type).normalize();
        reconstructed.inferred_scheme =
            runtime_informed_scheme(&reconstructed.final_input_type, &output_type);

        if let Some(cache) = type_cache.as_mut() {
            cache.insert(
                &cache_key,
                filter,
                CachedReconstruction {
                    final_input_type: reconstructed.final_input_type.clone(),
                    output_type,
                    subset_types: reconstructed.subset_types.clone(),
                    converged: reconstructed.converged,
                },
            );
            if let Some(path) = options.type_cache_path.as_ref()
                && let Err(err) = cache.save_if_dirty() {
                    eprintln!("warning: failed to save type cache `{path}`: {err}");
                }
        }
        reconstructed
    };

    println!("Validation target:");
    println!("  Filter: {filter}");
    println!(
        "  Baseline static scheme: {} -> {}",
        baseline_scheme.input, baseline_scheme.output
    );
    println!(
        "  Runtime-informed scheme: {} -> {}",
        result.inferred_scheme.input, result.inferred_scheme.output
    );
    if let Some(refinement) = infer_predicate_refinement(&expr, &result.final_input_type)
        && refinement.has_information() {
            println!("  Predicate refinement: {}", refinement.pretty());
        }
    println!(
        "  Reconstructed input domain: {}",
        result.annotated_input_type()
    );
    println!("  Converged: {}", result.converged);
    println!("  Input JSON: {}", input);

    match eval(&expr, &input) {
        Ok(output) => {
            println!("Evaluation: success");
            println!("  Output: {}", output);
            let report = validate_input_against_reconstruction(
                &input,
                &result.final_input_type,
                &result.subset_types,
                None,
            );
            println!("Scheme validator:");
            for line in report.pretty().lines() {
                println!("  {line}");
            }
            0
        }
        Err(err) => {
            println!("Evaluation: runtime type error");
            println!("  {}", err);
            let report = validate_input_against_reconstruction(
                &input,
                &result.final_input_type,
                &result.subset_types,
                Some(&err),
            );
            println!("Scheme validator:");
            for line in report.pretty().lines() {
                println!("  {line}");
            }
            if report.accepted_by_reconstruction && report.subset_hits.is_empty() {
                println!(
                    "  note: input fits current reconstructed domain; this indicates missing constraints in reconstruction."
                );
            }
            1
        }
    }
}

fn main() {
    let options = parse_cli_options();
    let config = ReconstructionConfig::strong();

    if options.validate_filter.is_some() {
        let code = run_validation(&options, &config);
        if code != 0 {
            std::process::exit(code);
        }
        return;
    }

    if let Some(path) = &options.analyze_file {
        let code = analyze_definition_file(Path::new(path), &options, &config);
        if code != 0 {
            std::process::exit(code);
        }
        return;
    }

    let code = run_examples(&options, &config);
    if code != 0 {
        std::process::exit(code);
    }
}

#[cfg(test)]
mod tests {
    use super::resolve_named_calls;
    use crate::ParsedDef;
    use std::collections::BTreeMap;
    use typereconstruction::minijq::{Expr, parse_definitions, parse_expr};

    #[test]
    fn resolve_named_calls_inlines_zero_arg_filters() {
        let src = r#"
            def isboolean: . == true or . == false;
            def booleans:  select(isboolean);
        "#;
        let defs = parse_definitions(src).expect("must parse defs");
        let parsed_defs = defs
            .into_iter()
            .map(|def| ParsedDef {
                key: def.cache_key(),
                expr: parse_expr(&def.body).expect("must parse body"),
                def,
            })
            .collect::<Vec<_>>();

        let zero_arg_defs: BTreeMap<String, Expr> = parsed_defs
            .iter()
            .filter(|parsed| parsed.def.params.is_none())
            .map(|parsed| (parsed.def.name.clone(), parsed.expr.clone()))
            .collect();

        let booleans = parsed_defs
            .iter()
            .find(|parsed| parsed.def.name == "booleans")
            .expect("booleans def");

        let mut stack = vec![booleans.def.name.clone()];
        let resolved =
            resolve_named_calls(&booleans.expr, &zero_arg_defs, &mut stack).expect("must resolve");

        assert_eq!(
            resolved.to_string(),
            "select(((. == true) or (. == false)))"
        );
    }

    #[test]
    fn resolve_named_calls_reports_missing_filter() {
        let expr = parse_expr("select(unknown_filter)").expect("must parse call");
        let mut stack = vec!["root".to_string()];
        let err = resolve_named_calls(&expr, &BTreeMap::new(), &mut stack).unwrap_err();
        assert!(err.contains("unknown filter `unknown_filter`"));
    }
}
