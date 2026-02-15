# jq vs jqv Overhead

Average wall time over 20 runs per command (steady-state, jqv warmed).

| Case | jq valid (ms) | jqv valid (ms) | valid overhead | jq invalid (ms) | jqv invalid (ms) | invalid overhead |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| metrics_sum | 9.894 | 11.593 | +17.2% | 8.982 | 11.828 | +31.7% |
| map_increment_id | 9.340 | 11.671 | +25.0% | 9.005 | 12.076 | +34.1% |
| split_csv | 9.508 | 11.607 | +22.1% | 9.023 | 11.912 | +32.0% |
| strict_boolean_gate | 9.071 | 11.622 | +28.1% | 8.995 | 11.735 | +30.5% |
| large_mid_error | 323.615 | 298.010 | -7.9% | 50.033 | 360.838 | +621.2% |
