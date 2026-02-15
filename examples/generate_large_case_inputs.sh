#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

N="${N:-50000}"
MID="${MID:-25000}"
OUT_DIR="examples/inputs/large_mid_error"
mkdir -p "$OUT_DIR"

generate_file() {
  local path="$1"
  local invalid="$2"

  perl -e '
    my ($path, $n, $mid, $invalid) = @ARGV;
    open my $fh, ">", $path or die "failed to open $path: $!";
    print $fh "[\n";
    for my $i (0 .. $n - 1) {
      my $value = ($invalid && $i == $mid) ? "\"oops\"" : $i;
      my $comma = ($i == $n - 1) ? "" : ",";
      print $fh "  {\"id\":$value}$comma\n";
    }
    print $fh "]\n";
    close $fh;
  ' "$path" "$N" "$MID" "$invalid"
}

generate_file "$OUT_DIR/valid.json" 0
generate_file "$OUT_DIR/invalid.json" 1

echo "Generated $OUT_DIR/valid.json and $OUT_DIR/invalid.json"
wc -l "$OUT_DIR/valid.json" "$OUT_DIR/invalid.json"
