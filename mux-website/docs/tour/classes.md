---
id: classes
title: Classes
description: Object-oriented programming with classes in Mux
---

import VideoPlaceholder from '@site/src/components/VideoPlaceholder';
import EmbeddedPlayground from '@site/src/components/EmbeddedPlayground';

# Classes

<VideoPlaceholder topic="Classes" />

Classes encapsulate data and behavior into single units.

## Basic Class

<EmbeddedPlayground initialCode={`class Counter {
    int value
    
    common func from_start(int start) returns Counter {
        Counter c = Counter.new()
        c.value = start
        return c
    }
    
    func increment() returns void {
        self.value = self.value + 1
    }
    
    func getValue() returns int {
        return self.value
    }
}

func main() returns void {
    auto counter = Counter.from_start(0)
    counter.increment()
    counter.increment()
    print("Count: " + counter.getValue().to_string())
}`} />

## Class Methods

<EmbeddedPlayground initialCode={`class Calculator {
    func add(int a, int b) returns int {
        return a + b
    }
    
    func multiply(int a, int b) returns int {
        return a * b
    }
}

func main() returns void {
    Calculator calc = Calculator.new()
    print("5 + 3 = " + calc.add(5, 3).to_string())
    print("5 * 3 = " + calc.multiply(5, 3).to_string())
}`} />

---

Previous: [Sets](/docs/tour/sets) | Next: [Enums](/docs/tour/enums)
