#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

if ! command -v jq >/dev/null 2>&1; then
  echo "jq is required to regenerate example results." >&2
  exit 1
fi

mkdir -p \
  examples/results/metrics_sum \
  examples/results/map_increment_id \
  examples/results/split_csv \
  examples/results/strict_boolean_gate

run_case() {
  local case_name="$1"
  local filter="$2"
  local program="examples/programs/${case_name}.jq"
  local valid_input="examples/inputs/${case_name}/valid.json"
  local invalid_input="examples/inputs/${case_name}/invalid.json"
  local out_dir="examples/results/${case_name}"

  jq -c -f "$program" "$valid_input" > "${out_dir}/jq.valid.stdout.txt" 2> "${out_dir}/jq.valid.stderr.txt"

  if jq -c -f "$program" "$invalid_input" > "${out_dir}/jq.invalid.stdout.txt" 2> "${out_dir}/jq.invalid.stderr.txt"; then
    echo 0 > "${out_dir}/jq.invalid.exitcode"
  else
    echo $? > "${out_dir}/jq.invalid.exitcode"
  fi

  if cargo run -q -- --validate-filter "$filter" --validate-input-file "$valid_input" > "${out_dir}/validator.valid.txt" 2>&1; then
    echo 0 > "${out_dir}/validator.valid.exitcode"
  else
    echo $? > "${out_dir}/validator.valid.exitcode"
  fi

  if cargo run -q -- --validate-filter "$filter" --validate-input-file "$invalid_input" > "${out_dir}/validator.invalid.txt" 2>&1; then
    echo 0 > "${out_dir}/validator.invalid.exitcode"
  else
    echo $? > "${out_dir}/validator.invalid.exitcode"
  fi
}

run_case "metrics_sum" ".metrics.a + .metrics.b"
run_case "map_increment_id" "map(.id + 1)"
run_case "split_csv" "split(\",\")"
run_case "strict_boolean_gate" "if . == true or . == false then 1 else error end"

echo "Regenerated examples/results."
