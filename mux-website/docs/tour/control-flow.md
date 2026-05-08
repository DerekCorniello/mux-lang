---
id: control-flow
title: Control Flow
description: If/else statements, loops, and pattern matching in Mux
---

import VideoPlaceholder from '@site/src/components/VideoPlaceholder';
import EmbeddedPlayground from '@site/src/components/EmbeddedPlayground';

# Control Flow

<VideoPlaceholder topic="Control Flow" />

Control flow statements determine the order in which code executes.

## If/Else Statements

<EmbeddedPlayground initialCode={`func main() returns void {
    int score = 85
    
    if score >= 90 {
        print("Grade: A")
    } else if score >= 80 {
        print("Grade: B")
    } else if score >= 70 {
        print("Grade: C")
    } else {
        print("Grade: F")
    }
}`} />

## For Loops

Iterate a specific number of times:

<EmbeddedPlayground initialCode={`func main() returns void {
    for int i in range(0, 5) {
        print("Count: " + i.to_string())
    }
}`} />

## While Loops

Repeat while a condition is true:

<EmbeddedPlayground initialCode={`func main() returns void {
    int count = 0
    
    while count &lt; 3 {
        print("Count: " + count.to_string())
        count = count + 1
    }
}`} />

## Pattern Matching with Match

Powerful pattern matching with enums and optionals:

<EmbeddedPlayground initialCode={`func main() returns void {
    optional&lt;int&gt; opt = some(42)
    
    match opt {
        some(value) {
            print("Got value: " + value.to_string())
        }
        none {
            print("Got nothing")
        }
    }
}`} />

---

Previous: [Functions](/docs/tour/functions) | Next: [Lists](/docs/tour/lists)