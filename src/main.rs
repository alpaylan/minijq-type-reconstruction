use rand::SeedableRng;
use rand::rngs::StdRng;
use std::env;
use typereconstruction::minijq::{
    CachedReconstruction, ReconstructionConfig, ReconstructionResult, Type, TypeCache, TypeScheme,
    all_examples, infer_expr_scheme, infer_expr_type, reconstruct_input_type_unbiased_with_scheme,
    reconstruct_input_type_with_scheme,
};

const DEFAULT_TYPE_CACHE_PATH: &str = ".minijq.typecache.mjqi";

#[derive(Clone, Debug)]
struct CliOptions {
    trace: bool,
    unbiased: bool,
    only_program: Option<String>,
    type_cache_path: Option<String>,
    refresh_type_cache: bool,
}

impl Default for CliOptions {
    fn default() -> Self {
        CliOptions {
            trace: false,
            unbiased: false,
            only_program: None,
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
                    "usage: cargo run -- [--trace] [--unbiased] [--program NAME] [--type-cache PATH | --no-type-cache] [--refresh-type-cache]"
                );
                std::process::exit(2);
            }
        }
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

fn main() {
    let options = parse_cli_options();
    let config = ReconstructionConfig::default();
    let mut matched = false;
    let mut cache_hits = 0usize;
    let mut cache_misses = 0usize;
    let mut type_cache = options.type_cache_path.as_ref().map(|path| {
        TypeCache::load_or_default(path).unwrap_or_else(|err| {
            eprintln!(
                "warning: failed to load type cache `{path}` ({err}); starting with empty cache"
            );
            TypeCache::new(path)
        })
    });
    let cache_enabled = type_cache.is_some();

    for (idx, example) in all_examples().into_iter().enumerate() {
        if let Some(name) = &options.only_program {
            if example.name != name {
                continue;
            }
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
                    &config,
                    &mut rng,
                )
            } else {
                reconstruct_input_type_with_scheme(
                    &example.expr,
                    baseline_scheme.clone(),
                    &config,
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

    if let Some(cache) = type_cache.as_mut() {
        if let Some(path) = options.type_cache_path.as_ref() {
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
    }

    if cache_enabled && matched {
        println!("Type cache stats: {cache_hits} hit(s), {cache_misses} miss(es)");
    }

    if let Some(name) = options.only_program {
        if !matched {
            eprintln!("no example named `{name}`");
            std::process::exit(1);
        }
    }
}
