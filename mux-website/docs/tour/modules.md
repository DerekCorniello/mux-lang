---
id: modules
title: Modules
description: Organizing your code with modules in Mux
---

import VideoPlaceholder from '@site/src/components/VideoPlaceholder';
import EmbeddedPlayground from '@site/src/components/EmbeddedPlayground';

# Modules

<VideoPlaceholder topic="Modules" />

Modules help organize your code into separate files and namespaces.

## Import Statements

Import items from other files:

<EmbeddedPlayground initialCode={`import std::io

func main() returns void {
    print("Using std::io module")
}`} />

## Using Standard Library

<EmbeddedPlayground initialCode={`import std::math

func main() returns void {
    print("Pi from math: " + std::math.PI.to_string())
}`} />

## Creating Your Own Modules

Organize related code into separate files with the `.mux` extension.

---

Previous: [References](/docs/tour/references) | Next: [Next Steps](/docs/tour/next-steps)