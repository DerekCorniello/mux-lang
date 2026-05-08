---
id: functions
title: Functions
description: Writing and calling functions in Mux
---

import VideoPlaceholder from '@site/src/components/VideoPlaceholder';
import EmbeddedPlayground from '@site/src/components/EmbeddedPlayground';

# Functions

<VideoPlaceholder topic="Functions" />

Functions are reusable blocks of code that perform specific tasks.

## Basic Functions

<EmbeddedPlayground initialCode={`func main() returns void {
    int result = add(5, 3)
    print("5 + 3 = " + result.to_string())
}

func add(int a, int b) returns int {
    return a + b
}`} />

## Parameters and Return Values

<EmbeddedPlayground initialCode={`func main() returns void {
    print("Area: " + calculateArea(5.0, 3.0).to_string())
}

func calculateArea(float width, float height) returns float {
    return width * height
}`} />

## Default Parameters

Functions can have default parameter values:

<EmbeddedPlayground initialCode={`func main() returns void {
    greet("World")
    greet("Mux", 3)
}

func greet(string name, int times = 1) returns void {
    for int i in range(0, times) {
        print("Hello " + name)
    }
}`} />

## Lambda Functions

Anonymous functions assigned to variables:

<EmbeddedPlayground initialCode={`func main() returns void {
    auto square = func(int n) returns int {
        return n * n
    }
    
    print("Square of 5: " + square(5).to_string())
}`} />

---

Previous: [Basic Types](/docs/tour/basic-types) | Next: [Control Flow](/docs/tour/control-flow)