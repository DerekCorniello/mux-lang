---
title: Net
---

# std.net — Networking Primitives

`std.net` exposes low-level networking helpers so you can build clients and simple servers without introducing large abstractions.

## TcpStream

Use `TcpStream` to connect to a TCP server and perform simple blocking-IO operations.

| Method | Signature | Return | Description |
| ------ | --------- | ------ | ----------- |
| `TcpStream.connect(addr)` | `string` | `result<TcpStream, string>` | Establishes a connection to the supplied address. Returns an error if the socket could not connect. |
| `stream.read(size)` | `int` | `result<list<int>, string>` | Reads up to `size` bytes. The returned bytes are a `list<int>` so you can inspect each byte. |
| `stream.write(data)` | `list<int>` | `result<int, string>` | Sends the provided byte list and returns the number of bytes written. |
| `stream.close()` | — | `void` | Releases the socket handle. | 
| `stream.set_nonblocking(enabled)` | `bool` | `result<void, string>` | Toggle non-blocking mode; errors are returned via the result. |
| `stream.peer_addr()` | — | `result<string, string>` | Returns the remote address or an error if the connection is closed. |
| `stream.local_addr()` | — | `result<string, string>` | Returns the local socket address or an error if the socket was closed. |

## UdpSocket

`UdpSocket` is useful for simple datagram protocols or probing localhost.

| Method | Signature | Return | Description |
| ------ | --------- | ------ | ----------- |
| `UdpSocket.bind(addr)` | `string` | `result<UdpSocket, string>` | Binds a new socket to the given port. Use `127.0.0.1:0` to let the OS pick an ephemeral port. |
| `socket.send_to(data, addr)` | `list<int>`, `string` | `result<int, string>` | Sends a datagram to the specified address. |
| `socket.recv_from(size)` | `int` | `result<tuple<list<int>, string>, string>` | Receives up to `size` bytes and returns the payload plus the sender address. |
| `socket.close()` | — | `void` | Closes the socket. |
| `socket.set_nonblocking(enabled)` | `bool` | `result<void, string>` | Toggle non-blocking mode; errors are returned via the result. |
| `socket.peer_addr()` | — | `result<string, string>` | Returns the peer address when the socket is connected. |
| `socket.local_addr()` | — | `result<string, string>` | Returns the local bind address. |

## Http client

`std.net.http` exposes a low-level JSON-driven HTTP client primitive.

| Function | Signature | Return | Description |
| -------- | --------- | ------ | ----------- |
| `http.request(req)` | `Json` | `result<Json, string>` | Sends an HTTP request represented as JSON and returns a JSON response shape. |

### Request JSON shape

- `method`: `string` (required)
- `url`: `string` (required)
- `headers`: `object<string, string>` (optional)
- `body`: `Json` (optional, serialized as JSON text)

### Response JSON shape

- `status`: `number`
- `headers`: `object<string, string>`
- `body`: `Json` (parsed from response text when valid JSON, otherwise raw string)

## Quick HTTP example

```mux
import std.data.json
import std.net

func main() returns void {
    match json.parse("{\"method\":\"POST\",\"url\":\"https://httpbin.org/post\",\"headers\":{\"Accept\":\"application/json\"},\"body\":{\"value\":42}}") {
        ok(req) {
            match net.http.request(req) {
                ok(resp) { print("http response json received") }
                err(e) { print("http error: " + e) }
            }
        }
        err(e) { print("invalid request json: " + e) }
    }
}
```

## Quick UDP example

```mux
import std.assert
import std.net

func main() returns void {
    match net.UdpSocket.bind("127.0.0.1:0") {
        ok(sock) {
            match sock.local_addr() {
                ok(local) {
                    const data = [116, 101, 115, 116]
                    match sock.send_to(data, local) { ok(_) {} err(e) { assert.assert(false, e) } }
                    match sock.recv_from(16) {
                        ok(pair) {
                            assert.assert(pair.right == local)
                            assert.assert(pair.left[0] == data[0])
                        }
                        err(e) { assert.assert(false, e) }
                    }
                }
                err(e) { assert.assert(false, e) }
            }
            sock.close()
        }
        err(e) { assert.assert(false, e) }
    }
}
```
