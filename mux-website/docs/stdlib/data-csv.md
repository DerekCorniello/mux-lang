---
title: Data CSV
---

# std.data.csv — CSV parsing

`std.data.csv` exposes light CSV parsing helpers that return structured `Csv` values with the parser taking care of quoting and delimiters.

- `data.csv.parse(string text) -> result<Csv, string>` — parse rows without headers (each row is a list of strings).
- `data.csv.parse_with_headers(string text) -> result<Csv, string>` — parse rows and preserve the first line as header names.

The resulting `Csv` value is a first-class type in the stdlib (see `csv.stringify` for output).

Example:

```mux
import std.csv

func main() returns void {
    const data = "name,age\nAlice,30"
    match csv.parse_with_headers(data) {
        ok(table) {
            match table.stringify(none) {  // uses Csv.stringify()
                ok(text) { print(text) }
                err(e) { print("csv stringify failed: " + e) }
            }
        }
        err(e) { print("csv parse failed: " + e) }
    }
}
```
