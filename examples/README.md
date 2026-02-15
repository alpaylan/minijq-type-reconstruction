# Examples

This folder contains concrete jq/minijq filters, sample inputs, jq outputs, and validator diagnostics produced by this project.

## Layout

- `programs/*.jq`: jq programs/filters.
- `inputs/<case>/{valid,invalid}.json`: sample inputs.
- `results/<case>/jq.*`: jq stdout/stderr and exit codes.
- `results/<case>/validator.*`: scheme-validator output and exit codes from `cargo run -- --validate-filter ...`.
- `results/<case>/overhead.txt`: jq vs jqv timing snapshot for each case.
- `results/perf_summary.md`: aggregated jq vs jqv overhead table.
- `generate_results.sh`: regenerates all files in `results/`.

## jq vs jqv Overhead Snapshot

Average wall time over 20 runs per command (steady-state, jqv warmed). Numbers are machine-dependent; regenerate locally for your environment.

| Case | jq valid (ms) | jqv valid (ms) | valid overhead | jq invalid (ms) | jqv invalid (ms) | invalid overhead |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `metrics_sum` | 9.894 | 11.593 | +17.2% | 8.982 | 11.828 | +31.7% |
| `map_increment_id` | 9.340 | 11.671 | +25.0% | 9.005 | 12.076 | +34.1% |
| `split_csv` | 9.508 | 11.607 | +22.1% | 9.023 | 11.912 | +32.0% |
| `strict_boolean_gate` | 9.071 | 11.622 | +28.1% | 8.995 | 11.735 | +30.5% |
| `large_mid_error` | 323.615 | 298.010 | -7.9% | 50.033 | 360.838 | +621.2% |

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

### 5) `large_mid_error`

- Program: `programs/large_mid_error.jq`
- Filter: `map((.id + 1) * 2) | ... | sort | reverse | sort | reverse | sort | add`
- Inputs:
  - `inputs/large_mid_error/valid.json` (50002 lines, 50000 records)
  - `inputs/large_mid_error/invalid.json` (50002 lines, one bad element in the middle at index `25000`)
- jq results:
  - valid stdout: `results/large_mid_error/jq.valid.stdout.txt`
  - invalid stderr: `results/large_mid_error/jq.invalid.stderr.txt`
- validator diagnostics:
  - invalid report: `results/large_mid_error/validator.invalid.txt`
  - key message:
    - `$[25000]["id"]: expected Number, got String<"oops">`

## Regenerate

From repo root:

```bash
bash examples/generate_results.sh
```

Use `BENCH_RUNS` to tune timing stability vs speed:

```bash
BENCH_RUNS=50 bash examples/generate_results.sh
```
