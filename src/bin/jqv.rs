use rand::SeedableRng;
use rand::rngs::StdRng;
use serde_json::Value;
use std::env;
use std::fs;
use std::io::{self, Read, Write};
use std::path::Path;
use std::process::{Command, ExitCode, Stdio};
use typereconstruction::minijq::{
    CachedReconstruction, ReconstructionConfig, ReconstructionResult, TypeCache, eval,
    infer_expr_scheme, infer_expr_type, infer_predicate_refinement, parse_expr,
    reconstruct_input_type_with_scheme, validate_input_against_reconstruction,
};

const DEFAULT_TYPE_CACHE_PATH: &str = ".minijq.typecache.mjqi";

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct ParsedInvocation {
    filter_expr: Option<String>,
    filter_file: Option<String>,
    input_files: Vec<String>,
    null_input: bool,
}

fn print_usage(program: &str) {
    eprintln!("usage: {program} [jq arguments...]");
    eprintln!(
        "jqv forwards all arguments to jq and only adds diagnostics when jq exits with runtime error code 5."
    );
    eprintln!("set JQ_BIN to override the jq executable (default: jq).");
}

fn parse_jq_invocation(args: &[String]) -> ParsedInvocation {
    let mut parsed = ParsedInvocation::default();
    let mut positional = Vec::new();
    let mut i = 0usize;
    let mut after_double_dash = false;

    while i < args.len() {
        let arg = &args[i];
        if !after_double_dash && arg == "--" {
            after_double_dash = true;
            i += 1;
            continue;
        }

        if !after_double_dash && arg.starts_with('-') {
            if arg == "-n" || arg == "--null-input" {
                parsed.null_input = true;
                i += 1;
                continue;
            }

            if arg == "-f" || arg == "--from-file" {
                if let Some(path) = args.get(i + 1) {
                    parsed.filter_file = Some(path.clone());
                    i += 2;
                    continue;
                }
                i += 1;
                continue;
            }

            if let Some(path) = arg.strip_prefix("--from-file=") {
                parsed.filter_file = Some(path.to_string());
                i += 1;
                continue;
            }

            if let Some(path) = arg.strip_prefix("-f") {
                if !path.is_empty() {
                    parsed.filter_file = Some(path.to_string());
                    i += 1;
                    continue;
                }
            }

            let takes_two = matches!(
                arg.as_str(),
                "--arg" | "--argjson" | "--slurpfile" | "--rawfile" | "--argfile"
            );
            if takes_two {
                i += 3;
                continue;
            }

            let takes_one = matches!(arg.as_str(), "-L" | "--library-path");
            if takes_one {
                i += 2;
                continue;
            }

            if arg.starts_with("-L") && arg.len() > 2 {
                i += 1;
                continue;
            }

            i += 1;
            continue;
        }

        positional.push(arg.clone());
        i += 1;
    }

    if parsed.filter_file.is_none() {
        if let Some((first, rest)) = positional.split_first() {
            parsed.filter_expr = Some(first.clone());
            parsed.input_files.extend(rest.iter().cloned());
        }
    } else {
        parsed.input_files = positional;
    }

    parsed
}

fn read_stdin_all() -> io::Result<Vec<u8>> {
    let mut buf = Vec::new();
    io::stdin().read_to_end(&mut buf)?;
    Ok(buf)
}

fn run_jq_passthrough(jq_bin: &str, args: &[String], stdin_data: Option<&[u8]>) -> io::Result<i32> {
    if let Some(stdin_data) = stdin_data {
        let mut child = Command::new(jq_bin)
            .args(args)
            .stdin(Stdio::piped())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .spawn()?;
        if let Some(mut stdin) = child.stdin.take() {
            stdin.write_all(stdin_data)?;
        }
        let status = child.wait()?;
        return Ok(status.code().unwrap_or(1));
    }

    let status = Command::new(jq_bin).args(args).status()?;
    Ok(status.code().unwrap_or(1))
}

fn derive_filter_source(parsed: &ParsedInvocation) -> Result<String, String> {
    if let Some(expr) = &parsed.filter_expr {
        return Ok(expr.clone());
    }
    if let Some(path) = &parsed.filter_file {
        return fs::read_to_string(path)
            .map(|body| body.trim().to_string())
            .map_err(|err| format!("failed to read filter file `{path}`: {err}"));
    }
    Err("could not infer jq filter expression from invocation".to_string())
}

fn derive_input_json(
    parsed: &ParsedInvocation,
    stdin_data: Option<&[u8]>,
) -> Result<Value, String> {
    if parsed.null_input {
        return Ok(Value::Null);
    }

    if parsed.input_files.len() == 1 {
        let path = &parsed.input_files[0];
        let body = fs::read_to_string(path)
            .map_err(|err| format!("failed to read jq input file `{path}`: {err}"))?;
        return serde_json::from_str::<Value>(&body)
            .map_err(|err| format!("jq input file `{path}` is not a single JSON value: {err}"));
    }

    if parsed.input_files.is_empty() {
        let Some(stdin_data) = stdin_data else {
            return Err("no jq input available for validator".to_string());
        };
        let text = String::from_utf8_lossy(stdin_data);
        return serde_json::from_str::<Value>(&text)
            .map_err(|err| format!("stdin input is not a single JSON value: {err}"));
    }

    Err("validator currently supports one input JSON value (single file or stdin)".to_string())
}

fn load_cache(path: &Path) -> TypeCache {
    TypeCache::load_or_default(path).unwrap_or_else(|err| {
        eprintln!(
            "jqv warning: failed to load type cache `{}` ({err}); using empty cache",
            path.display()
        );
        TypeCache::new(path)
    })
}

fn reconstruct_for_filter(
    filter_source: &str,
    cache: &mut TypeCache,
) -> Result<ReconstructionResult, String> {
    let expr = parse_expr(filter_source)
        .map_err(|err| format!("filter parse failed for validator replay: {err}"))?;
    let baseline = infer_expr_scheme(&expr);
    let cache_key = format!("jqv::{filter_source}");

    if let Some(cached) = cache.lookup(&cache_key, filter_source) {
        let inferred_scheme = typereconstruction::minijq::TypeScheme {
            vars: Vec::new(),
            input: cached.final_input_type.clone(),
            output: cached.output_type.clone(),
        };
        return Ok(ReconstructionResult {
            inferred_scheme,
            final_input_type: cached.final_input_type,
            subset_types: cached.subset_types,
            converged: cached.converged,
            traces: Vec::new(),
        });
    }

    let config = ReconstructionConfig::default();
    let mut rng = StdRng::seed_from_u64(991_337);
    let mut reconstructed = reconstruct_input_type_with_scheme(&expr, baseline, &config, &mut rng);
    let output_type = infer_expr_type(&expr, &reconstructed.final_input_type).normalize();
    reconstructed.inferred_scheme = typereconstruction::minijq::TypeScheme {
        vars: Vec::new(),
        input: reconstructed.final_input_type.clone(),
        output: output_type.clone(),
    };

    cache.insert(
        &cache_key,
        filter_source,
        CachedReconstruction {
            final_input_type: reconstructed.final_input_type.clone(),
            output_type,
            subset_types: reconstructed.subset_types.clone(),
            converged: reconstructed.converged,
        },
    );

    Ok(reconstructed)
}

fn run_validator_diagnostics(filter_source: &str, input: &Value) -> Result<(), String> {
    let path = Path::new(DEFAULT_TYPE_CACHE_PATH);
    let mut cache = load_cache(path);
    let result = reconstruct_for_filter(filter_source, &mut cache)?;
    let _ = cache.save_if_dirty();

    let expr = parse_expr(filter_source)
        .map_err(|err| format!("filter parse failed for validator replay: {err}"))?;
    let runtime_error = eval(&expr, input).err();
    let report = validate_input_against_reconstruction(
        input,
        &result.final_input_type,
        &result.subset_types,
        runtime_error.as_ref(),
    );

    eprintln!("jqv validator:");
    eprintln!("  Filter: {}", filter_source.replace('\n', " "));
    eprintln!("  Reconstructed input domain: {}", result.final_input_type);
    eprintln!(
        "  Runtime-informed scheme: {} -> {}",
        result.inferred_scheme.input, result.inferred_scheme.output
    );
    if let Some(refinement) = infer_predicate_refinement(&expr, &result.final_input_type) {
        if refinement.has_information() {
            eprintln!("  Predicate refinement: {}", refinement.pretty());
        }
    }
    for line in report.pretty().lines() {
        eprintln!("  {line}");
    }

    if runtime_error.is_none() {
        eprintln!("  note: jq failed but minijq replay did not reproduce a runtime error.");
    }

    Ok(())
}

fn main() -> ExitCode {
    let args = env::args().skip(1).collect::<Vec<_>>();
    if args.iter().any(|arg| arg == "--jqv-help") {
        print_usage(&env::args().next().unwrap_or_else(|| "jqv".to_string()));
        return ExitCode::SUCCESS;
    }

    let jq_bin = env::var("JQ_BIN").unwrap_or_else(|_| "jq".to_string());
    let parsed = parse_jq_invocation(&args);

    let stdin_data = if parsed.input_files.is_empty() && !parsed.null_input {
        match read_stdin_all() {
            Ok(data) => Some(data),
            Err(err) => {
                eprintln!("jqv: failed to read stdin: {err}");
                return ExitCode::from(2);
            }
        }
    } else {
        None
    };

    let jq_code = match run_jq_passthrough(&jq_bin, &args, stdin_data.as_deref()) {
        Ok(code) => code,
        Err(err) => {
            eprintln!("jqv: failed to execute `{jq_bin}`: {err}");
            return ExitCode::from(127);
        }
    };

    if jq_code != 5 {
        return ExitCode::from(jq_code as u8);
    }

    let filter_source = match derive_filter_source(&parsed) {
        Ok(source) => source,
        Err(reason) => {
            eprintln!("jqv: runtime error detected; skipped validator: {reason}");
            return ExitCode::from(jq_code as u8);
        }
    };
    let input = match derive_input_json(&parsed, stdin_data.as_deref()) {
        Ok(input) => input,
        Err(reason) => {
            eprintln!("jqv: runtime error detected; skipped validator: {reason}");
            return ExitCode::from(jq_code as u8);
        }
    };

    eprintln!();
    eprintln!("--- jqv: jq runtime error detected; running validator diagnostics ---");
    if let Err(err) = run_validator_diagnostics(&filter_source, &input) {
        eprintln!("jqv: validator failed: {err}");
    }

    ExitCode::from(jq_code as u8)
}

#[cfg(test)]
mod tests {
    use super::{ParsedInvocation, parse_jq_invocation};

    #[test]
    fn parse_inline_filter_and_file() {
        let args = vec![".a + 1".to_string(), "input.json".to_string()];
        let parsed = parse_jq_invocation(&args);
        assert_eq!(
            parsed,
            ParsedInvocation {
                filter_expr: Some(".a + 1".to_string()),
                filter_file: None,
                input_files: vec!["input.json".to_string()],
                null_input: false,
            }
        );
    }

    #[test]
    fn parse_filter_file_and_null_input() {
        let args = vec!["-n".to_string(), "-f".to_string(), "prog.jq".to_string()];
        let parsed = parse_jq_invocation(&args);
        assert_eq!(parsed.filter_expr, None);
        assert_eq!(parsed.filter_file, Some("prog.jq".to_string()));
        assert!(parsed.input_files.is_empty());
        assert!(parsed.null_input);
    }

    #[test]
    fn parse_long_options_that_take_values() {
        let args = vec![
            "--arg".to_string(),
            "x".to_string(),
            "1".to_string(),
            "--argjson".to_string(),
            "y".to_string(),
            "2".to_string(),
            ". + $x + $y".to_string(),
            "input.json".to_string(),
        ];
        let parsed = parse_jq_invocation(&args);
        assert_eq!(parsed.filter_expr, Some(". + $x + $y".to_string()));
        assert_eq!(parsed.input_files, vec!["input.json".to_string()]);
    }
}
