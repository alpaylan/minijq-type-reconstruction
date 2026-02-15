use rand::rngs::StdRng;
use rand::SeedableRng;
use typereconstruction::minijq::{all_examples, reconstruct_input_type, ReconstructionConfig};

fn main() {
    let config = ReconstructionConfig::default();

    for (idx, example) in all_examples().into_iter().enumerate() {
        let mut rng = StdRng::seed_from_u64(1_000 + idx as u64);
        let result = reconstruct_input_type(&example.expr, &config, &mut rng);

        println!("Program: {}", example.name);
        println!("  Desc: {}", example.description);
        println!("  Src : {}", example.source);
        println!("  Expr: {}", example.expr);
        println!(
            "  Scheme: {} -> {}",
            result.inferred_scheme.input, result.inferred_scheme.output
        );
        println!("  Final input type: {}", result.final_input_type);
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

        println!();
    }
}
