---
id: error-handling
title: Error Handling
description: Result and Optional types for error handling in Mux
---

import VideoPlaceholder from '@site/src/components/VideoPlaceholder';
import EmbeddedPlayground from '@site/src/components/EmbeddedPlayground';

# Error Handling

<VideoPlaceholder topic="Error Handling" />

Mux provides Result and Optional types for handling errors and missing values.

## Result Types

Result types represent either success (ok) or failure (err):

<EmbeddedPlayground initialCode={`func divide(int a, int b) returns result<int, string> {
    if b == 0 {
        return err("Cannot divide by zero")
    }
    return ok(a / b)
}

func main() returns void {
    match divide(10, 2) {
        ok(v) { print("Result: " + v.to_string()) }
        err(e) { print("Error: " + e) }
    }
    
    match divide(10, 0) {
        ok(v) { print("Result: " + v.to_string()) }
        err(e) { print("Error: " + e) }
    }
}`} />

## Optional Types

Optional types represent a value that might not exist:

<EmbeddedPlayground initialCode={`func findEven(list<int> nums) returns optional<int> {
    for int n in nums {
        if n % 2 == 0 { return some(n) }
    }
    return none
}

func main() returns void {
    match findEven([1, 3, 5, 6]) {
        some(v) { print("Found even: " + v.to_string()) }
        none { print("No even number") }
    }
    
    match findEven([1, 3, 5]) {
        some(v) { print("Found even: " + v.to_string()) }
        none { print("No even number") }
    }
}`} />

## Combining with Pattern Matching

<EmbeddedPlayground initialCode={`func safeDivide(int a, int b) returns result<int, string> {
    if b == 0 {
        return err("Division by zero")
    }
    return ok(a / b)
}

func main() returns void {
    list<int> numbers = [10, 5, 0, 20]
    
    for int n in numbers {
        match safeDivide(100, n) {
            ok(v) { print("100 / " + n.to_string() + " = " + v.to_string()) }
            err(e) { print("100 / " + n.to_string() + " = Error: " + e) }
        }
    }
}`} />

---

Previous: [Generics](/docs/tour/generics) | Next: [Interfaces](/docs/tour/interfaces)