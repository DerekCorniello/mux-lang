---
id: hello-world
title: Hello World
description: Your first Mux program
---

import VideoPlaceholder from '@site/src/components/VideoPlaceholder';
import EmbeddedPlayground from '@site/src/components/EmbeddedPlayground';

# Hello World

<VideoPlaceholder topic="Hello World" />

Every Mux program starts with a `main` function. This is the entry point where execution begins. Like Python, code outside of functions is also executed when the file runs, but `main()` is the conventional entry point.

## The Simplest Program

The `main` function is special - it's where your program starts:

- `func main()` defines the main function
- `returns void` means this function doesn't return a value
- `print()` outputs text to the console

<EmbeddedPlayground initialCode={`func main() returns void {
    print("Hello, World!")
}`} />

## Adding Comments

Comments explain your code and are ignored by the compiler:

<EmbeddedPlayground initialCode={`func main() returns void {
    // This is a single-line comment
    
    /*
       This is a
       multi-line comment
    */
    
    print("Comments are ignored!")
}`} />

## Running Your Program

Save your code to a file (e.g., `hello.mux`) and run it:

```bash
mux run hello.mux
```

---

Next: [Variables](/docs/tour/variables) - Learn how to store and use data in your programs.