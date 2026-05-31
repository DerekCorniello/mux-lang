---
id: references
title: References
description: Passing values efficiently with references in Mux
---

import VideoPlaceholder from '@site/src/components/VideoPlaceholder';
import EmbeddedPlayground from '@site/src/components/EmbeddedPlayground';

# References

<VideoPlaceholder topic="References" />

References allow functions to modify variables directly without copying.

## Reference Parameters

Use `ref` to pass a variable by reference:

<EmbeddedPlayground initialCode={`func increment(int ref n) returns void {
    n = n + 1
}

func main() returns void {
    int num = 5
    print("Before: " + num.to_string())
    increment(num)
    print("After: " + num.to_string())
}`} />

## Swap Function

<EmbeddedPlayground initialCode={`func swap(int ref a, int ref b) returns void {
    int temp = a
    a = b
    b = temp
}

func main() returns void {
    int x = 1
    int y = 2
    
    print("Before: x = " + x.to_string() + ", y = " + y.to_string())
    swap(x, y)
    print("After: x = " + x.to_string() + ", y = " + y.to_string())
}`} />

## When to Use References

References are useful when you need to modify the original value, avoid copying large data, or return multiple values.

<EmbeddedPlayground initialCode={`func double(int ref n) returns void {
    n = n * 2
}

func main() returns void {
    int value = 10
    double(value)
    print("Doubled: " + value.to_string())
}`} />

---

Previous: [Interfaces](/docs/tour/interfaces) | Next: [Modules](/docs/tour/modules)