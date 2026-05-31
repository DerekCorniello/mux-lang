---
id: lists
title: Lists
description: Working with ordered collections in Mux
---

import VideoPlaceholder from '@site/src/components/VideoPlaceholder';
import EmbeddedPlayground from '@site/src/components/EmbeddedPlayground';

# Lists

<VideoPlaceholder topic="Lists" />

Lists are ordered, growable collections of elements.

## Creating Lists

<EmbeddedPlayground initialCode={`func main() returns void {
    list<int> numbers = [1, 2, 3, 4, 5]
    list<string> names = ["Alice", "Bob", "Charlie"]
    
    print("Numbers: " + numbers.to_string())
    print("Names: " + names.to_string())
}`} />

## Accessing Elements

<EmbeddedPlayground initialCode={`func main() returns void {
    list<int> nums = [10, 20, 30, 40, 50]
    
    print("First: " + nums[0].to_string())
    print("Third: " + nums[2].to_string())
    print("Last: " + nums[4].to_string())
}`} />

## Modifying Lists

<EmbeddedPlayground initialCode={`func main() returns void {
    list<int> nums = [1, 2, 3]
    
    nums.push_back(4)
    print("After push: " + nums.to_string())
    
    auto removed = nums.pop_back()
    print("Popped: " + removed.to_string())
    print("After pop: " + nums.to_string())
}`} />

## List Properties

<EmbeddedPlayground initialCode={`func main() returns void {
    list<int> nums = [1, 2, 3]
    
    print("Size: " + nums.size().to_string())
    print("Empty: " + nums.is_empty().to_string())
}`} />

---

Previous: [Control Flow](/docs/tour/control-flow) | Next: [Maps](/docs/tour/maps)