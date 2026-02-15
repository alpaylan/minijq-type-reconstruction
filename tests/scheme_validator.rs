use rand::SeedableRng;
use rand::rngs::StdRng;
use serde_json::json;
use typereconstruction::minijq::{
    ReconstructionConfig, ValidationIssueKind, eval, infer_expr_scheme, parse_expr,
    reconstruct_input_type_with_scheme, validate_input_against_reconstruction,
};

fn reconstruct_for_validation(
    filter: &str,
    seed: u64,
) -> typereconstruction::minijq::ReconstructionResult {
    let expr = parse_expr(filter).expect("filter should parse");
    let scheme = infer_expr_scheme(&expr);
    let config = ReconstructionConfig {
        max_iterations: 60,
        samples_per_iteration: 32,
        max_value_depth: 6,
        rejection_probe_samples: 16,
        required_success_rounds: 3,
    };
    let mut rng = StdRng::seed_from_u64(seed);
    reconstruct_input_type_with_scheme(&expr, scheme, &config, &mut rng)
}

#[test]
fn validator_points_to_nested_numeric_mismatch() {
    let filter = ".metrics.a + .metrics.b";
    let expr = parse_expr(filter).expect("filter should parse");
    let reconstructed = reconstruct_for_validation(filter, 5001);
    let input = json!({"metrics": {"a": "oops", "b": 2}});

    let runtime = eval(&expr, &input).expect_err("input should fail at runtime");
    let report = validate_input_against_reconstruction(
        &input,
        &reconstructed.final_input_type,
        &reconstructed.subset_types,
        Some(&runtime),
    );

    assert!(
        report
            .issues
            .iter()
            .any(|issue| issue.path == "$[\"metrics\"][\"a\"]"),
        "expected nested path issue, got {:?}",
        report.issues
    );
}

#[test]
fn validator_points_to_array_element_error_site() {
    let filter = "map(.id + 1)";
    let expr = parse_expr(filter).expect("filter should parse");
    let reconstructed = reconstruct_for_validation(filter, 5002);
    let input = json!([{"id": 1}, {"id": "x"}]);

    let runtime = eval(&expr, &input).expect_err("input should fail at runtime");
    let report = validate_input_against_reconstruction(
        &input,
        &reconstructed.final_input_type,
        &reconstructed.subset_types,
        Some(&runtime),
    );

    assert!(
        report
            .issues
            .iter()
            .any(|issue| issue.path == "$[1][\"id\"]"),
        "expected element path issue, got {:?}",
        report.issues
    );
}

#[test]
fn validator_reports_non_empty_requirement() {
    let filter = ".[0]";
    let expr = parse_expr(filter).expect("filter should parse");
    let reconstructed = reconstruct_for_validation(filter, 5003);
    let input = json!([]);

    let runtime = eval(&expr, &input).expect_err("empty array should fail");
    let report = validate_input_against_reconstruction(
        &input,
        &reconstructed.final_input_type,
        &reconstructed.subset_types,
        Some(&runtime),
    );

    assert!(
        report.issues.iter().any(|issue| {
            issue.path == "$" && matches!(issue.kind, ValidationIssueKind::NonEmptyRequired)
        }),
        "expected non-empty requirement, got {:?}",
        report.issues
    );
}
