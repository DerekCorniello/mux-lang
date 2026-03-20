# DSA Module

The `dsa` module provides data structures and algorithms including stack, queue, heap, binary tree, graph, and utility functions for sorting and searching.

## Import

```mux
import std.dsa

// Or import specific modules:
import std.dsa.stack
import std.dsa.queue
import std.dsa.heap
import std.dsa.bintree
import std.dsa.graph
import std.dsa.algorithm
```

## Collection Interface

All DSA data structures implement the `Collection<T>` interface:

```mux
import std.dsa.collection.Collection
```

| Method | Returns | Description |
|--------|---------|-------------|
| `len()` | `int` | Returns the number of elements |
| `is_empty()` | `bool` | Returns true if the collection is empty |
| `clear()` | `void` | Removes all elements |
| `to_list()` | `list<T>` | Returns elements as a list |

## Stack

A LIFO (last-in, first-out) collection.

```mux
import std.dsa.stack
```

### Stack Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `Stack<T>.new()` | `Stack<T>` | Creates an empty stack |
| `push(T value)` | `void` | Adds an element to the top |
| `pop()` | `optional<T>` | Removes and returns the top element |
| `peek()` | `optional<T>` | Returns the top element without removing it |

Also implements `len()`, `is_empty()`, `clear()`, `to_list()`.

## Queue

A FIFO (first-in, first-out) collection.

```mux
import std.dsa.queue
```

### Queue Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `Queue<T>.new()` | `Queue<T>` | Creates an empty queue |
| `enqueue(T value)` | `void` | Adds an element to the back |
| `dequeue()` | `optional<T>` | Removes and returns the front element |
| `peek()` | `optional<T>` | Returns the front element without removing it |

Also implements `len()`, `is_empty()`, `clear()`, `to_list()`.

## Heap

A min-heap collection where the smallest element is always at the top.

```mux
import std.dsa.heap
```

### Heap Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `Heap<T>.new()` | `Heap<T>` | Creates an empty heap |
| `push(T value)` | `void` | Adds an element |
| `pop()` | `optional<T>` | Removes and returns the minimum element |
| `peek()` | `optional<T>` | Returns the minimum element without removing it |

Type constraint: `T is Comparable`

Also implements `len()`, `is_empty()`, `clear()`, `to_list()`.

## BinaryTree

A binary search tree with set semantics (duplicates are ignored).

```mux
import std.dsa.bintree
```

### BinaryTree Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `BinaryTree<T>.new()` | `BinaryTree<T>` | Creates an empty tree |
| `insert(T value)` | `void` | Adds an element |
| `remove(T value)` | `void` | Removes an element |
| `contains(T value)` | `bool` | Checks if an element exists |

Type constraint: `T is Comparable`

Also implements `len()`, `is_empty()`, `clear()`, `to_list()` (returns elements in sorted order via inorder traversal).

## Graph

A directed graph using adjacency lists.

```mux
import std.dsa.graph
```

### Graph Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `Graph<T>.new()` | `Graph<T>` | Creates an empty graph |
| `add_vertex(T value)` | `void` | Adds a vertex |
| `add_edge(T from, T to)` | `void` | Adds a directed edge |
| `neighbors(T value)` | `list<T>` | Returns neighbors of a vertex |
| `bfs(T start)` | `list<T>` | Breadth-first search traversal |

Type constraint: `T is Hashable & Stringable`

Also implements `len()`, `is_empty()`, `clear()`, `to_list()` (returns vertices in insertion order).

## Algorithm Functions

```mux
import std.dsa.algorithm
```

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `sort` | `<T is Comparable>(list<T> items) returns list<T>` | Quicksort algorithm |
| `binary_search` | `<T is Comparable, E is Collection<T>>(E items, T target) returns int` | Returns index or -1 if not found |
| `max` | `<T is Comparable & Stringable, E is Collection<T>>(E collection) returns optional<T>` | Returns maximum element |
| `min` | `<T is Comparable & Stringable, E is Collection<T>>(E collection) returns optional<T>` | Returns minimum element |
| `reverse` | `<T, E is Collection<T>>(E collection) returns list<T>` | Returns reversed list |

## Example

```mux title="dsa_example.mux"
import std.dsa.*
import std.dsa.collection.Collection

func main() returns void {
    // Stack example
    auto stack = stack.Stack<int>.new()
    stack.push(1)
    stack.push(2)
    stack.push(3)

    print("Stack top: " + match stack.peek() {
        some(v) { v.to_string() }
        none { "empty" }
    })  // 3

    match stack.pop() {
        some(v) { print("Popped: " + v.to_string()) }  // 3
        none {}
    }

    // Queue example
    auto queue = queue.Queue<string>.new()
    queue.enqueue("first")
    queue.enqueue("second")
    queue.enqueue("third")

    match queue.dequeue() {
        some(v) { print("Dequeued: " + v) }  // first
        none {}
    }

    // Heap example (min-heap)
    auto heap = heap.Heap<int>.new()
    heap.push(30)
    heap.push(10)
    heap.push(20)

    match heap.pop() {
        some(v) { print("Min: " + v.to_string()) }  // 10
        none {}
    }

    // Algorithm example
    auto numbers = [5, 3, 8, 1, 2]
    auto sorted = algorithm.sort(numbers)
    print("Sorted: " + sorted.to_string())  // [1, 2, 3, 5, 8]

    match algorithm.binary_search(sorted, 3) {
        ok(idx) { print("Found 3 at index: " + idx.to_string()) }
        err(_) { print("Not found") }
    }
}
```
