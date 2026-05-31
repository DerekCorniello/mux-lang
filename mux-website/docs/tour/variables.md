---
id: variables
title: Variables
description: Declaring and using variables in Mux
---

import VideoPlaceholder from '@site/src/components/VideoPlaceholder';
import EmbeddedPlayground from '@site/src/components/EmbeddedPlayground';

# Variables

<VideoPlaceholder topic="Variables" />

Variables store data that your program can use and modify.

## Declaring Variables

Mux has explicit typing with type inference. You can declare variables with explicit types or let Mux infer the type:

<EmbeddedPlayground initialCode={`func main() returns void {
    // Explicit type
    int age = 25
    
    // Type inference (auto keyword)
    auto name = "Mux"
    auto temperature = 72.5
    auto isActive = true
    
    print("Name: " + name)
    print("Age: " + age.to_string())
    print("Temp: " + temperature.to_string())
    print("Active: " + isActive.to_string())
}`} />

## When to Use Explicit Types

The `auto` keyword works well when the type is obvious from the value:

- `auto name = "Mux"` - string inferred
- `auto count = 42` - int inferred
- `auto numbers = [1, 2, 3]` - type inferred from contents

Some cases require an explicit type because the type can't be inferred:

- Empty collections: `auto empty = []` (error - can't infer element type)
- Use explicit types: `List<int> numbers = []`, `Map<string, int> ages = {}`

<EmbeddedPlayground initialCode={`func main() returns void {
    // These work with auto
    auto name = "Mux"
    auto count = 42
    auto numbers = [1, 2, 3]

    // These require explicit types
    List<int> emptyNums = []
    Map<string, int> ages = {}
    
    print("Name: " + name)
    print("Count: " + count.to_string())
    print("Numbers: " + numbers.to_string())
}`} />

## Constants

Use `const` to declare values that never change:

<EmbeddedPlayground initialCode={`func main() returns void {
    const int MAX_COUNT = 100
    const string APP_NAME = "MyApp"
    const float PI = 3.14159
    
    print("Max: " + MAX_COUNT.to_string())
    print("App: " + APP_NAME)
    print("Pi: " + PI.to_string())
}`} />

## Incrementing and Decrementing

Variables can be incremented or decremented using `++` and `--`:

<EmbeddedPlayground initialCode={`func main() returns void {
    int count = 0
    
    print("Start: " + count.to_string())
    
    count++
    print("After ++: " + count.to_string())
    
    count--
    print("After --: " + count.to_string())
}`} />

---

Previous: [Hello World](/docs/tour/hello-world) | Next: [Basic Types](/docs/tour/basic-types)