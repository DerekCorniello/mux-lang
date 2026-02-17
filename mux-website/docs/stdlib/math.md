# Math Module

The `math` module provides common floating-point math functions and constants.

## Import

```mux
import std.math
```

## Constants

- `math.pi` returns `float`
- `math.e` returns `float`

## Unary Functions

All unary math functions take one `float` and return a `float`:

- `math.sqrt(x)`
- `math.sin(x)`
- `math.cos(x)`
- `math.tan(x)`
- `math.asin(x)`
- `math.acos(x)`
- `math.atan(x)`
- `math.ln(x)`
- `math.log2(x)`
- `math.log10(x)`
- `math.exp(x)`
- `math.abs(x)`
- `math.floor(x)`
- `math.ceil(x)`
- `math.round(x)`

## Binary Functions

All binary math functions take two `float` values and return a `float`:

- `math.atan2(y, x)`
- `math.log(x, base)`
- `math.min(a, b)`
- `math.max(a, b)`
- `math.hypot(a, b)`
- `math.pow(base, exp)`

## Example

```mux title="math_example.mux"
import std.math

func main() returns void {
    auto radius = 5.0
    auto area = math.pi * math.pow(radius, 2.0)
    auto angle = math.atan2(1.0, 1.0)
    auto rounded = math.round(area)

    print("area: " + area.to_string())
    print("angle: " + angle.to_string())
    print("rounded: " + rounded.to_string())
}
```
