# Datetime Module

The `datetime` module provides Unix timestamp utilities, calendar field extraction, formatting, and blocking sleep helpers.

## Import

```mux
import std.datetime
```

## Time Retrieval

- `datetime.now() returns Result<int, string>`
- `datetime.now_millis() returns Result<int, string>`

`now()` returns seconds since Unix epoch in UTC. `now_millis()` returns milliseconds since Unix epoch in UTC.

## Calendar Fields

- `datetime.year(int ts) returns Result<int, string>`
- `datetime.month(int ts) returns Result<int, string>`
- `datetime.day(int ts) returns Result<int, string>`
- `datetime.hour(int ts) returns Result<int, string>`
- `datetime.minute(int ts) returns Result<int, string>`
- `datetime.second(int ts) returns Result<int, string>`
- `datetime.weekday(int ts) returns Result<int, string>` where `0 = Sunday ... 6 = Saturday`

## Formatting

- `datetime.format(int ts, string pattern) returns Result<string, string>` for UTC output
- `datetime.format_local(int ts, string pattern) returns Result<string, string>` for local timezone output

Format patterns use chrono `strftime` tokens:

- `%A` full weekday name
- `%a` abbreviated weekday name
- `%B` full month name
- `%b` abbreviated month name
- `%Y-%m-%d %H:%M:%S` date and time

## Sleep

- `datetime.sleep(int seconds) returns Result<void, string>`
- `datetime.sleep_millis(int milliseconds) returns Result<void, string>`

Both functions block at the call site and return `Err` for negative durations.

## Example

```mux title="datetime_example.mux"
import std.datetime

func main() returns void {
    match datetime.now() {
        Ok(ts) {
            match datetime.format(ts, "%A, %B %d, %Y %H:%M:%S UTC") {
                Ok(text) { print("UTC: " + text) }
                Err(msg) { print("format error: " + msg) }
            }

            match datetime.format_local(ts, "%A, %B %d, %Y %H:%M:%S %Z") {
                Ok(text) { print("Local: " + text) }
                Err(msg) { print("local format error: " + msg) }
            }

            print("before sleep")
            match datetime.sleep_millis(1000) {
                Ok(_) { print("after sleep") }
                Err(msg) { print("sleep error: " + msg) }
            }
        }
        Err(msg) { print("now error: " + msg) }
    }
}
```
