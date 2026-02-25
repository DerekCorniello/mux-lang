# Datetime Module

The `datetime` module provides Unix timestamp utilities, calendar field extraction, formatting, and blocking sleep helpers.

## Import

```mux
import std.datetime
```

## Time Retrieval

- `datetime.now() returns result<int, string>`
- `datetime.now_millis() returns result<int, string>`

`now()` returns seconds since Unix epoch in UTC. `now_millis()` returns milliseconds since Unix epoch in UTC.

## Calendar Fields

- `datetime.year(int ts) returns result<int, string>`
- `datetime.month(int ts) returns result<int, string>`
- `datetime.day(int ts) returns result<int, string>`
- `datetime.hour(int ts) returns result<int, string>`
- `datetime.minute(int ts) returns result<int, string>`
- `datetime.second(int ts) returns result<int, string>`
- `datetime.weekday(int ts) returns result<int, string>` where `0 = Sunday ... 6 = Saturday`

## Formatting

- `datetime.format(int ts, string pattern) returns result<string, string>` for UTC output
- `datetime.format_local(int ts, string pattern) returns result<string, string>` for local timezone output

Format patterns use chrono `strftime` tokens:

- `%A` full weekday name
- `%a` abbreviated weekday name
- `%B` full month name
- `%b` abbreviated month name
- `%Y-%m-%d %H:%M:%S` date and time

## Sleep

- `datetime.sleep(int seconds) returns result<void, string>`
- `datetime.sleep_millis(int milliseconds) returns result<void, string>`

Both functions block at the call site and return `err` for negative durations.

## Example

```mux title="datetime_example.mux"
import std.datetime

func main() returns void {
    match datetime.now() {
        ok(ts) {
            match datetime.format(ts, "%A, %B %d, %Y %H:%M:%S UTC") {
                ok(text) { print("UTC: " + text) }
                err(msg) { print("format error: " + msg) }
            }

            match datetime.format_local(ts, "%A, %B %d, %Y %H:%M:%S %Z") {
                ok(text) { print("Local: " + text) }
                err(msg) { print("local format error: " + msg) }
            }

            print("before sleep")
            match datetime.sleep_millis(1000) {
                ok(_) { print("after sleep") }
                err(msg) { print("sleep error: " + msg) }
            }
        }
        err(msg) { print("now error: " + msg) }
    }
}
```
