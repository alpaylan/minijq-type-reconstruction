use rand::rngs::StdRng;
use rand::SeedableRng;
use std::env;
use typereconstruction::minijq::{
    all_examples, reconstruct_input_type, reconstruct_input_type_unbiased, ReconstructionConfig,
};

#[derive(Clone, Debug, Default)]
struct CliOptions {
    trace: bool,
    unbiased: bool,
    only_program: Option<String>,
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
            other => {
                eprintln!("unknown argument: {other}");
                eprintln!("usage: cargo run -- [--trace] [--unbiased] [--program NAME]");
                std::process::exit(2);
            }
        }
    }

    options
}

fn main() {
    let options = parse_cli_options();
    let config = ReconstructionConfig::default();
    let mut matched = false;

    for (idx, example) in all_examples().into_iter().enumerate() {
        if let Some(name) = &options.only_program {
            if example.name != name {
                continue;
            }
        }
        matched = true;

        let mut rng = StdRng::seed_from_u64(1_000 + idx as u64);
        let result = if options.unbiased {
            reconstruct_input_type_unbiased(&example.expr, &config, &mut rng)
        } else {
            reconstruct_input_type(&example.expr, &config, &mut rng)
        };

        println!("Program: {}", example.name);
        println!("  Desc: {}", example.description);
        println!("  Src : {}", example.source);
        println!("  Expr: {}", example.expr);
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
            for line in result.pretty_trace().lines() {
                println!("    {line}");
            }
        }

        println!();
    }

    if let Some(name) = options.only_program {
        if !matched {
            eprintln!("no example named `{name}`");
            std::process::exit(1);
        }
    }
}
