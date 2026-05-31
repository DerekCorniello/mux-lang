---
id: maps
title: Maps
description: Key-value data structures in Mux
---

import VideoPlaceholder from '@site/src/components/VideoPlaceholder';
import EmbeddedPlayground from '@site/src/components/EmbeddedPlayground';

# Maps

<VideoPlaceholder topic="Maps" />

Maps store key-value pairs, allowing fast lookup by key.

## Creating Maps

<EmbeddedPlayground initialCode={`func main() returns void {
    map<string, int> scores = {"Alice": 90, "Bob": 85}
    
    print("Scores: " + scores.to_string())
}`} />

## Accessing Values

<EmbeddedPlayground initialCode={`func main() returns void {
    map<string, int> scores = {"Alice": 90, "Bob": 85}
    
    print("Alice: " + scores["Alice"].to_string())
    print("Bob: " + scores["Bob"].to_string())
}`} />

## Modifying Maps

<EmbeddedPlayground initialCode={`func main() returns void {
    map<string, int> scores = {"Alice": 90}
    
    scores["Alice"] = 95
    scores.put("Charlie", 88)
    
    print("After changes: " + scores.to_string())
}`} />

## Map Methods

<EmbeddedPlayground initialCode={`func main() returns void {
    map<string, int> scores = {"Alice": 90, "Bob": 85}
    
    print("Keys: " + scores.get_keys().to_string())
    print("Values: " + scores.get_values().to_string())
    print("Size: " + scores.size().to_string())
    print("Has Alice: " + scores.contains("Alice").to_string())
}`} />

---

Previous: [Lists](/docs/tour/lists) | Next: [Sets](/docs/tour/sets)