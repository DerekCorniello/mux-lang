---
id: enums
title: Enums
description: Tagged unions and pattern matching with enums in Mux
---

import VideoPlaceholder from '@site/src/components/VideoPlaceholder';
import EmbeddedPlayground from '@site/src/components/EmbeddedPlayground';

# Enums

<VideoPlaceholder topic="Enums" />

Enums define tagged unions with named variants that can hold data.

## Basic Enum

<EmbeddedPlayground initialCode={`enum Direction {
    North
    South
    East
    West
}

func main() returns void {
    Direction dir = Direction.North
    
    match dir {
        North { print("Heading North") }
        South { print("Heading South") }
        East { print("Heading East") }
        West { print("Heading West") }
    }
}`} />

## Enum with Data

<EmbeddedPlayground initialCode={`enum Shape {
    Circle(float radius)
    Rectangle(float width, float height)
}

func main() returns void {
    Shape circle = Shape.Circle(5.0)
    
    match circle {
        Circle(r) {
            print("Circle with radius: " + r.to_string())
        }
        Rectangle(w, h) {
            print("Rectangle: " + w.to_string() + "x" + h.to_string())
        }
    }
}`} />

## Matching Enums

<EmbeddedPlayground initialCode={`enum Result {
    Success(int value)
    Error(string message)
}

func main() returns void {
    Result res = Result.Success(42)
    
    match res {
        Success(v) {
            print("Success: " + v.to_string())
        }
        Error(msg) {
            print("Error: " + msg)
        }
    }
}`} />

---

Previous: [Classes](/docs/tour/classes) | Next: [Generics](/docs/tour/generics)