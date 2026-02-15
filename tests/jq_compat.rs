use serde_json::{json, Value};
use std::io::Write;
use std::process::{Command, Stdio};
use typereconstruction::minijq::{eval, parse_expr};

fn jq_available() -> bool {
    Command::new("jq")
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .map(|status| status.success())
        .unwrap_or(false)
}

fn run_minijq(filter: &str, input: &Value) -> Result<Value, String> {
    let expr = parse_expr(filter).map_err(|err| format!("parse error: {err}"))?;
    eval(&expr, input).map_err(|err| err.message)
}

fn run_jq(filter: &str, input: &Value) -> Result<Value, String> {
    let mut child = Command::new("jq")
        .arg("-c")
        .arg(filter)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|err| format!("failed to spawn jq: {err}"))?;

    {
        let stdin = child
            .stdin
            .as_mut()
            .ok_or_else(|| "failed to open jq stdin".to_string())?;
        stdin
            .write_all(format!("{input}\n").as_bytes())
            .map_err(|err| format!("failed to write jq input: {err}"))?;
    }

    let output = child
        .wait_with_output()
        .map_err(|err| format!("failed to read jq output: {err}"))?;

    if !output.status.success() {
        return Err(String::from_utf8_lossy(&output.stderr).trim().to_string());
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<&str> = stdout.lines().filter(|line| !line.trim().is_empty()).collect();
    if lines.len() != 1 {
        return Err(format!(
            "expected exactly one jq output value, got {}: {}",
            lines.len(),
            stdout.trim()
        ));
    }

    serde_json::from_str(lines[0]).map_err(|err| format!("invalid jq JSON output: {err}"))
}

fn json_equiv(lhs: &Value, rhs: &Value) -> bool {
    match (lhs, rhs) {
        (Value::Null, Value::Null) => true,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::String(a), Value::String(b)) => a == b,
        (Value::Number(a), Value::Number(b)) => match (a.as_f64(), b.as_f64()) {
            (Some(x), Some(y)) => (x - y).abs() <= 1e-9,
            _ => a == b,
        },
        (Value::Array(xs), Value::Array(ys)) => {
            xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(x, y)| json_equiv(x, y))
        }
        (Value::Object(xm), Value::Object(ym)) => {
            if xm.len() != ym.len() {
                return false;
            }

            xm.iter()
                .all(|(k, xv)| ym.get(k).is_some_and(|yv| json_equiv(xv, yv)))
        }
        _ => false,
    }
}

#[test]
fn jq_compat_success_cases() {
    if !jq_available() {
        eprintln!("skipping jq compatibility tests: jq is not installed");
        return;
    }

    let cases = vec![
        (".", json!({"a": 1})),
        (". + .", json!(3)),
        ("(.n // 0) * 2", json!({"n": 4})),
        ("(.n // 0) * 2", json!({})),
        ("length", json!("abc")),
        ("length", json!([1, 2, 3])),
        (".user.name | length", json!({"user": {"name": "alice"}})),
        ("if .score > 0 then .score else 0 end", json!({"score": -2})),
        (".score >= 10 and .score < 20", json!({"score": 12})),
        ("{sum: (.a + .b), ok: (.a > .b)}", json!({"a": 3, "b": 1})),
        ("map(. + 1)", json!([1, 2, 3])),
        ("has(\"config\")", json!({"config": 1})),
        ("contains({status: \"ok\"})", json!({"status": "ok", "x": 1})),
        ("startswith(\"pre\") or endswith(\"fix\")", json!("prefix")),
        ("sort | reverse", json!([3, 1, 2])),
        ("(.price | tonumber?) // 0 | abs", json!({"price": "-4.2"})),
        ("(.price | tonumber?) // 0 | abs", json!({"price": "not-a-number"})),
        ("try (.raw | tonumber) catch 0", json!({"raw": "12.5"})),
        ("try (.raw | tonumber) catch 0", json!({"raw": "x"})),
        (".csv | split(\",\") | join(\"-\")", json!({"csv": "a,b,c"})),
        ("try (.scores | max) catch null", json!({"scores": [1, 5, 2]})),
        ("try (.scores | max) catch null", json!({"scores": []})),
        ("try (.values | add) catch null", json!({"values": [1, 2, 3]})),
        ("try (.values | add) catch null", json!({"values": [null]})),
        (".data[.key]? // \"missing\"", json!({"data": {"x": 7}, "key": "x"})),
        (".data[.key]? // \"missing\"", json!({"data": 10, "key": "x"})),
        ("if . == true or . == false then 1 else error end", json!(true)),
        ("if . == true or . == false then 1 else error end", json!(false)),
        ("try error(\"boom\") catch 0", json!(null)),
        // Ported from tjq/tests/short/test4.
        (
            ". | split(\" \") | map(split(\"\")) | reverse | map(reverse) | map(join(\"\")) | join(\" \")",
            json!("hello from tjq"),
        ),
        // Adapted from tjq/tests/short/test1 (avoids .[] stream syntax).
        (
            "[ .name, (.ingredients | length), (.ingredients | map(select(.item == \"sugar\") | .amount.quantity)) ]",
            json!({
                "name": "cake",
                "ingredients": [
                    {"item": "sugar", "amount": {"quantity": 2}},
                    {"item": "sugar", "amount": {"quantity": 3}}
                ]
            }),
        ),
    ];

    for (filter, input) in cases {
        let jq_out = run_jq(filter, &input).unwrap_or_else(|err| {
            panic!("jq failed unexpectedly\nfilter: {filter}\ninput: {input}\nerror: {err}")
        });
        let mini_out = run_minijq(filter, &input).unwrap_or_else(|err| {
            panic!("minijq failed unexpectedly\nfilter: {filter}\ninput: {input}\nerror: {err}")
        });

        assert!(
            json_equiv(&jq_out, &mini_out),
            "jq/minijq mismatch\nfilter: {filter}\ninput: {input}\njq: {jq_out}\nminijq: {mini_out}"
        );
    }
}

#[test]
fn jq_compat_shared_error_cases() {
    if !jq_available() {
        eprintln!("skipping jq compatibility tests: jq is not installed");
        return;
    }

    let cases = vec![
        (".x", json!(10)),
        ("tonumber", json!("abc")),
        ("split(\",\")", json!(5)),
        ("join(\",\")", json!([1, {"a": 1}])),
        (".[\"x\"]", json!(5)),
        ("if . == true or . == false then 1 else error end", json!(null)),
        ("error(\"boom\")", json!(null)),
    ];

    for (filter, input) in cases {
        let jq_res = run_jq(filter, &input);
        let mini_res = run_minijq(filter, &input);

        assert!(
            jq_res.is_err(),
            "jq unexpectedly succeeded\nfilter: {filter}\ninput: {input}\noutput: {:?}",
            jq_res
        );
        assert!(
            mini_res.is_err(),
            "minijq unexpectedly succeeded\nfilter: {filter}\ninput: {input}\noutput: {:?}",
            mini_res
        );
    }
}
