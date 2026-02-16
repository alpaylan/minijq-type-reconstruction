# jq vs jqv Overhead

Average wall time over 30 runs per command (steady-state, jqv warmed).

| Case | jq valid (ms) | jqv valid (ms) | valid overhead | jq invalid (ms) | jqv invalid (ms) | invalid overhead |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| metrics_sum | 12.178 | 12.630 | +3.7% | 9.487 | 12.124 | +27.8% |
| map_increment_id | 12.210 | 11.859 | -2.9% | 9.133 | 12.302 | +34.7% |
| split_csv | 9.231 | 11.795 | +27.8% | 9.539 | 12.312 | +29.1% |
| strict_boolean_gate | 10.022 | 11.915 | +18.9% | 9.248 | 14.653 | +58.4% |
| large_mid_error | 302.486 | 290.541 | -3.9% | 50.357 | 96.003 | +90.6% |
