#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

if ! command -v jq >/dev/null 2>&1; then
  echo "jq is required to regenerate example results." >&2
  exit 1
fi

if ! command -v perl >/dev/null 2>&1; then
  echo "perl is required to benchmark jq vs jqv overhead." >&2
  exit 1
fi

BENCH_RUNS="${BENCH_RUNS:-20}"
JQ_BIN_PATH="$(command -v jq)"
JQV_BIN="${JQV_BIN:-$ROOT_DIR/target/release/jqv}"
MINIJQ_BIN="${MINIJQ_BIN:-minijq-type-reconstruction}"

if [[ ! -x "$JQV_BIN" ]]; then
  echo "Building jqv release binary for overhead measurements..." >&2
  cargo build -q --release --bin jqv
fi

mkdir -p \
  examples/results/metrics_sum \
  examples/results/map_increment_id \
  examples/results/split_csv \
  examples/results/strict_boolean_gate \
  examples/results/large_mid_error

measure_avg_ms() {
  local runs="$1"
  local expected="$2"
  local cmd="$3"

  perl -MTime::HiRes=time -e '
    my ($runs, $expected, $cmd) = @ARGV;
    my $total = 0.0;
    for (1 .. $runs) {
      my $start = time();
      my $status = system("bash", "-lc", $cmd);
      my $code = $status == -1 ? 127 : ($status >> 8);
      my $ok = $expected eq "any" ? 1 : $expected eq "zero" ? ($code == 0) : ($code != 0);
      if (!$ok) {
        die "benchmark exit mismatch (expected=$expected, got=$code): $cmd\n";
      }
      $total += (time() - $start);
    }
    printf "%.3f\n", ($total * 1000.0 / $runs);
  ' "$runs" "$expected" "$cmd"
}

percent_overhead() {
  local base_ms="$1"
  local new_ms="$2"
  perl -e '
    my ($base, $new) = @ARGV;
    if ($base <= 0) {
      print "n/a";
      exit 0;
    }
    printf "%+.1f%%", (($new - $base) / $base * 100.0);
  ' "$base_ms" "$new_ms"
}

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

  if cargo run -q --bin "$MINIJQ_BIN" -- --validate-filter "$filter" --validate-input-file "$valid_input" > "${out_dir}/validator.valid.txt" 2>&1; then
    echo 0 > "${out_dir}/validator.valid.exitcode"
  else
    echo $? > "${out_dir}/validator.valid.exitcode"
  fi

  if cargo run -q --bin "$MINIJQ_BIN" -- --validate-filter "$filter" --validate-input-file "$invalid_input" > "${out_dir}/validator.invalid.txt" 2>&1; then
    echo 0 > "${out_dir}/validator.invalid.exitcode"
  else
    echo $? > "${out_dir}/validator.invalid.exitcode"
  fi

  if [[ "$case_name" == "large_mid_error" ]]; then
    # Keep large-case validator reports readable by retaining only key lines.
    for report in "${out_dir}/validator.valid.txt" "${out_dir}/validator.invalid.txt"; do
      tmp_report="${report}.tmp"
      awk '
        /^Validation target:/ ||
        /^  Filter:/ ||
        /^  Baseline static scheme:/ ||
        /^  Runtime-informed scheme:/ ||
        /^  Reconstructed input domain:/ ||
        /^  Converged:/ ||
        /^Evaluation:/ ||
        /^Scheme validator:/ ||
        /^  Accepted by reconstructed domain:/ ||
        /^  Validation issues:/ ||
        /^    - /
      ' "$report" > "$tmp_report"
      mv "$tmp_report" "$report"
    done
  fi

  # Warm jqv caches before timing so measurements represent steady-state wrapper overhead.
  JQ_BIN="$JQ_BIN_PATH" "$JQV_BIN" -c -f "$program" "$valid_input" > /dev/null 2>&1 || true
  JQ_BIN="$JQ_BIN_PATH" "$JQV_BIN" -c -f "$program" "$invalid_input" > /dev/null 2>&1 || true

  local jq_valid_cmd="jq -c -f \"$program\" \"$valid_input\" > /dev/null 2>/dev/null"
  local jqv_valid_cmd="JQ_BIN=\"$JQ_BIN_PATH\" \"$JQV_BIN\" -c -f \"$program\" \"$valid_input\" > /dev/null 2>/dev/null"
  local jq_invalid_cmd="jq -c -f \"$program\" \"$invalid_input\" > /dev/null 2>/dev/null"
  local jqv_invalid_cmd="JQ_BIN=\"$JQ_BIN_PATH\" \"$JQV_BIN\" -c -f \"$program\" \"$invalid_input\" > /dev/null 2>/dev/null"

  local jq_valid_ms jqv_valid_ms jq_invalid_ms jqv_invalid_ms
  jq_valid_ms="$(measure_avg_ms "$BENCH_RUNS" zero "$jq_valid_cmd")"
  jqv_valid_ms="$(measure_avg_ms "$BENCH_RUNS" zero "$jqv_valid_cmd")"
  jq_invalid_ms="$(measure_avg_ms "$BENCH_RUNS" nonzero "$jq_invalid_cmd")"
  jqv_invalid_ms="$(measure_avg_ms "$BENCH_RUNS" nonzero "$jqv_invalid_cmd")"

  local valid_overhead invalid_overhead
  valid_overhead="$(percent_overhead "$jq_valid_ms" "$jqv_valid_ms")"
  invalid_overhead="$(percent_overhead "$jq_invalid_ms" "$jqv_invalid_ms")"

  cat > "${out_dir}/overhead.txt" <<EOF
bench_runs=${BENCH_RUNS}
jq.valid.avg_ms=${jq_valid_ms}
jqv.valid.avg_ms=${jqv_valid_ms}
valid.overhead_pct=${valid_overhead}
jq.invalid.avg_ms=${jq_invalid_ms}
jqv.invalid.avg_ms=${jqv_invalid_ms}
invalid.overhead_pct=${invalid_overhead}
EOF
}

run_case "metrics_sum" ".metrics.a + .metrics.b"
run_case "map_increment_id" "map(.id + 1)"
run_case "split_csv" "split(\",\")"
run_case "strict_boolean_gate" "if . == true or . == false then 1 else error end"
run_case "large_mid_error" "map((.id + 1) * 2) | map(. / 3) | map(. * 4) | map(. + 5) | map(. - 6) | map(. * 7) | map(. / 8) | map(. + 9) | map(. - 10) | map(. * 11) | map(. / 12) | map(. + 13) | map(. - 14) | map(. * 15) | map(. / 16) | map(. + 17) | map(. - 18) | map(. * 19) | map(. / 20) | map(. + 21) | sort | reverse | sort | reverse | sort | add"

SUMMARY_FILE="examples/results/perf_summary.md"
{
  echo "# jq vs jqv Overhead"
  echo
  echo "Average wall time over ${BENCH_RUNS} runs per command (steady-state, jqv warmed)."
  echo
  echo "| Case | jq valid (ms) | jqv valid (ms) | valid overhead | jq invalid (ms) | jqv invalid (ms) | invalid overhead |"
  echo "| --- | ---: | ---: | ---: | ---: | ---: | ---: |"

  for case_name in metrics_sum map_increment_id split_csv strict_boolean_gate large_mid_error; do
    out_dir="examples/results/${case_name}"
    jq_valid_ms="$(awk -F= '$1=="jq.valid.avg_ms" {print $2}' "${out_dir}/overhead.txt")"
    jqv_valid_ms="$(awk -F= '$1=="jqv.valid.avg_ms" {print $2}' "${out_dir}/overhead.txt")"
    valid_overhead="$(awk -F= '$1=="valid.overhead_pct" {print $2}' "${out_dir}/overhead.txt")"
    jq_invalid_ms="$(awk -F= '$1=="jq.invalid.avg_ms" {print $2}' "${out_dir}/overhead.txt")"
    jqv_invalid_ms="$(awk -F= '$1=="jqv.invalid.avg_ms" {print $2}' "${out_dir}/overhead.txt")"
    invalid_overhead="$(awk -F= '$1=="invalid.overhead_pct" {print $2}' "${out_dir}/overhead.txt")"
    echo "| ${case_name} | ${jq_valid_ms} | ${jqv_valid_ms} | ${valid_overhead} | ${jq_invalid_ms} | ${jqv_invalid_ms} | ${invalid_overhead} |"
  done
} > "$SUMMARY_FILE"

echo "Regenerated examples/results (including jq vs jqv overhead)."
