---
id: sets
title: Sets
description: Unique element collections in Mux
---

import VideoPlaceholder from '@site/src/components/VideoPlaceholder';
import EmbeddedPlayground from '@site/src/components/EmbeddedPlayground';

# Sets

<VideoPlaceholder topic="Sets" />

Sets are unordered collections of unique elements.

## Creating Sets

<EmbeddedPlayground initialCode={`func main() returns void {
    set<int> numbers = {1, 2, 3, 4, 5}
    set<string> tags = {"urgent", "important"}
    
    print("Numbers: " + numbers.to_string())
    print("Tags: " + tags.to_string())
}`} />

## Set Operations

<EmbeddedPlayground initialCode={`func main() returns void {
    set<int> nums = {1, 2, 3}
    
    print("Has 2: " + nums.contains(2).to_string())
    print("Has 5: " + nums.contains(5).to_string())
    
    nums.add(4)
    print("After add 4: " + nums.to_string())
    
    auto removed = nums.remove(2)
    print("Removed 2: " + removed.to_string())
    print("After remove: " + nums.to_string())
}`} />

## Set Size

<EmbeddedPlayground initialCode={`func main() returns void {
    set<int> nums = {1, 2, 3}
    
    print("Size: " + nums.size().to_string())
    print("Empty: " + nums.is_empty().to_string())
}`} />

---

Previous: [Maps](/docs/tour/maps) | Next: [Classes](/docs/tour/classes)