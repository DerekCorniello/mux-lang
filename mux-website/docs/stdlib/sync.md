# sync

`sync` provides basic concurrency primitives for threads and coordination.

## Import

```mux
import std.sync
```

## API

- `sync.spawn(fn() -> void) -> result<Thread, string>`
- `sync.sleep(int milliseconds) -> void`
- `Thread.join() -> result<void, string>`
- `Thread.detach() -> result<void, string>`
- `Mutex.new() -> Mutex`
- `Mutex.lock() -> result<void, string>`
- `Mutex.unlock() -> result<void, string>`
- `RwLock.new() -> RwLock`
- `RwLock.read_lock() -> result<void, string>`
- `RwLock.write_lock() -> result<void, string>`
- `RwLock.unlock() -> result<void, string>`
- `CondVar.new() -> CondVar`
- `CondVar.wait(mutex: Mutex) -> result<void, string>`
- `CondVar.signal() -> result<void, string>`
- `CondVar.broadcast() -> result<void, string>`

## Example

```mux
import std.sync

Mutex m = Mutex.new()
CondVar cv = CondVar.new()

result<Thread, string> worker = sync.spawn(func() returns void {
    sync.sleep(50)
    match m.lock() {
        ok(_) {
            cv.signal()
            m.unlock()
        }
        err(_) { }
    }
    return
})

match m.lock() {
    ok(_) {
        cv.wait(m)
        m.unlock()
    }
    err(_) { }
}

match worker {
    ok(t) { t.join() }
    err(_) { }
}
```
