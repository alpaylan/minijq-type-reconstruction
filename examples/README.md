# Examples

This folder contains concrete jq/minijq filters, sample inputs, jq outputs, and validator diagnostics produced by this project.

## Layout

- `programs/*.jq`: jq programs/filters.
- `inputs/<case>/{valid,invalid}.json`: sample inputs.
- `results/<case>/jq.*`: jq stdout/stderr and exit codes.
- `results/<case>/validator.*`: scheme-validator output and exit codes from `cargo run -- --validate-filter ...`.
- `generate_results.sh`: regenerates all files in `results/`.

## Cases

### 1) `metrics_sum`

- Program: `programs/metrics_sum.jq`
- Filter: `.metrics.a + .metrics.b`
- Inputs:
  - `inputs/metrics_sum/valid.json`
  - `inputs/metrics_sum/invalid.json`
- jq results:
  - valid stdout: `results/metrics_sum/jq.valid.stdout.txt`
  - invalid stderr: `results/metrics_sum/jq.invalid.stderr.txt`
- validator diagnostics:
  - invalid report: `results/metrics_sum/validator.invalid.txt`
  - key message:
    - `$["metrics"]["a"]: expected Number, got String<"oops">`

### 2) `map_increment_id`

- Program: `programs/map_increment_id.jq`
- Filter: `map(.id + 1)`
- Inputs:
  - `inputs/map_increment_id/valid.json`
  - `inputs/map_increment_id/invalid.json`
- jq results:
  - valid stdout: `results/map_increment_id/jq.valid.stdout.txt`
  - invalid stderr: `results/map_increment_id/jq.invalid.stderr.txt`
- validator diagnostics:
  - invalid report: `results/map_increment_id/validator.invalid.txt`
  - key message:
    - `$[1]["id"]: expected Number, got String<"x">`

### 3) `split_csv`

- Program: `programs/split_csv.jq`
- Filter: `split(",")`
- Inputs:
  - `inputs/split_csv/valid.json`
  - `inputs/split_csv/invalid.json`
- jq results:
  - valid stdout: `results/split_csv/jq.valid.stdout.txt`
  - invalid stderr: `results/split_csv/jq.invalid.stderr.txt`
- validator diagnostics:
  - invalid report: `results/split_csv/validator.invalid.txt`
  - key message:
    - `$: expected String, got Number<42>`

### 4) `strict_boolean_gate`

- Program: `programs/strict_boolean_gate.jq`
- Filter: `if . == true or . == false then 1 else error end`
- Inputs:
  - `inputs/strict_boolean_gate/valid.json`
  - `inputs/strict_boolean_gate/invalid.json`
- jq results:
  - valid stdout: `results/strict_boolean_gate/jq.valid.stdout.txt`
  - invalid stderr: `results/strict_boolean_gate/jq.invalid.stderr.txt`
- validator diagnostics:
  - invalid report: `results/strict_boolean_gate/validator.invalid.txt`
  - key message:
    - `$: expected Bool, got Null`

## Regenerate

From repo root:

```bash
bash examples/generate_results.sh
```
