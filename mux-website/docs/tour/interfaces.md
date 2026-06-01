---
id: interfaces
title: Interfaces
description: Defining contracts with interfaces in Mux
---

import VideoPlaceholder from '@site/src/components/VideoPlaceholder';
import EmbeddedPlayground from '@site/src/components/EmbeddedPlayground';

# Interfaces

<VideoPlaceholder topic="Interfaces" />

Interfaces define contracts that classes must implement.

## Basic Interface

<EmbeddedPlayground initialCode={`interface Drawable {
    func draw() returns void
}

class Circle is Drawable {
    float radius
    
    common func from_radius(float r) returns Circle {
        Circle c = Circle.new()
        c.radius = r
        return c
    }
    
    func draw() returns void {
        print("Drawing circle with radius " + self.radius.to_string())
    }
}

func main() returns void {
    Circle c = Circle.from_radius(5.0)
    c.draw()
}`} />

## Interface with Fields

<EmbeddedPlayground initialCode={`interface Named {
    string name
    func getName() returns string
}

class Person is Named {
    string name
    
    common func from_name(string n) returns Person {
        Person p = Person.new()
        p.name = n
        return p
    }
    
    func getName() returns string {
        return self.name
    }
}

func main() returns void {
    Person p = Person.from_name("Alice")
    print("Name: " + p.getName())
}`} />

---

Previous: [Error Handling](/docs/tour/error-handling) | Next: [References](/docs/tour/references)
