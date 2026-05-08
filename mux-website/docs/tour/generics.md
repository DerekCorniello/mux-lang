---
id: generics
title: Generics
description: Writing flexible, reusable code with generics in Mux
---

import VideoPlaceholder from '@site/src/components/VideoPlaceholder';
import EmbeddedPlayground from '@site/src/components/EmbeddedPlayground';

# Generics

<VideoPlaceholder topic="Generics" />

Generics allow you to write flexible, reusable code that works with any type while maintaining type safety.

## Generic Functions

<EmbeddedPlayground initialCode={`func max<T is Comparable>(T a, T b) returns T {
    if a > b {
        return a
    }
    return b
}

func main() returns void {
    print("Max of 5 and 3: " + max(5, 3).to_string())
    print("Max of 1.5 and 2.7: " + max(1.5, 2.7).to_string())
}`} />

## Generic Classes

<EmbeddedPlayground initialCode={`class Container<T> {
    T value
    
    common func new(T val) returns Container<T> {
        Container<T> c = Container<T>.new()
        c.value = val
        return c
    }
    
    func get() returns T {
        return self.value
    }
}

func main() returns void {
    auto intContainer = Container<int>.new(42)
    auto strContainer = Container<string>.new("Hello")
    
    print("Int: " + intContainer.get().to_string())
    print("String: " + strContainer.get())
}`} />

## Generic Stack Example

<EmbeddedPlayground initialCode={`class Stack<T> {
    list<T> items
    
    common func new() returns Stack<T> {
        Stack<T> s = Stack<T>.new()
        s.items = []
        return s
    }
    
    func push(T item) returns void {
        self.items.push_back(item)
    }
    
    func pop() returns optional<T> {
        if self.items.is_empty() { return none }
        return self.items.pop_back()
    }
}

func main() returns void {
    auto stack = Stack<int>.new()
    stack.push(1)
    stack.push(2)
    stack.push(3)
    
    match stack.pop() {
        some(v) { print("Popped: " + v.to_string()) }
        none { print("Empty") }
    }
}`} />

---

Previous: [Enums](/docs/tour/enums) | Next: [Error Handling](/docs/tour/error-handling)