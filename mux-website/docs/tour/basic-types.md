---
id: basic-types
title: Basic Types
description: The fundamental data types in Mux
---

import VideoPlaceholder from '@site/src/components/VideoPlaceholder';
import EmbeddedPlayground from '@site/src/components/EmbeddedPlayground';

# Basic Types

<VideoPlaceholder topic="Basic Types" />

Mux has four fundamental data types: integers, floating-point numbers, booleans, and strings.

## Integers (int)

Whole numbers without decimal points:

<EmbeddedPlayground initialCode={`func main() returns void {
    int positive = 42
    int negative = -17
    int zero = 0
    
    print("Positive: " + positive.to_string())
    print("Negative: " + negative.to_string())
    print("Zero: " + zero.to_string())
}`} />

## Floating-Point (float)

Numbers with decimal points:

<EmbeddedPlayground initialCode={`func main() returns void {
    float price = 19.99
    float temperature = -5.5
    float pi = 3.14159
    
    print("Price: " + price.to_string())
    print("Temp: " + temperature.to_string())
    print("Pi: " + pi.to_string())
}`} />

## Booleans (bool)

True or false values:

<EmbeddedPlayground initialCode={`func main() returns void {
    bool isReady = true
    bool isEmpty = false
    
    print("Ready: " + isReady.to_string())
    print("Empty: " + isEmpty.to_string())
}`} />

## Strings

Text data:

<EmbeddedPlayground initialCode={`func main() returns void {
    string greeting = "Hello"
    string name = "Mux"
    
    print(greeting + ", " + name + "!")
}`} />

## Type Conversions

Convert between types using `.to_string()`:

<EmbeddedPlayground initialCode={`func main() returns void {
    int num = 42
    float f = 3.14
    bool b = true
    
    print("Num: " + num.to_string())
    print("Float: " + f.to_string())
    print("Bool: " + b.to_string())
}`} />

---

Previous: [Variables](/docs/tour/variables) | Next: [Functions](/docs/tour/functions)