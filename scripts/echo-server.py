#!/usr/bin/env python3
"""
TCP and UDP echo server for Mux networking integration tests.

- TCP echo server on port 9000: accepts connections, echoes back any data received.
- UDP echo server on port 9001: echoes back datagrams to their sender.

Both servers run concurrently using threads.
"""

import socket
import threading
import sys

TCP_PORT = 9000
UDP_PORT = 9001
BUFFER_SIZE = 4096


def handle_tcp_client(conn, addr):
    """Handle a single TCP client connection by echoing data back."""
    try:
        while True:
            data = conn.recv(BUFFER_SIZE)
            if not data:
                break
            conn.sendall(data)
    except OSError:
        pass
    finally:
        conn.close()


def tcp_server():
    """Run a TCP echo server on TCP_PORT."""
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server.bind(("0.0.0.0", TCP_PORT))
    server.listen(16)
    print(f"TCP echo server listening on port {TCP_PORT}", flush=True)

    while True:
        conn, addr = server.accept()
        t = threading.Thread(target=handle_tcp_client, args=(conn, addr), daemon=True)
        t.start()


def udp_server():
    """Run a UDP echo server on UDP_PORT."""
    server = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server.bind(("0.0.0.0", UDP_PORT))
    print(f"UDP echo server listening on port {UDP_PORT}", flush=True)

    while True:
        data, addr = server.recvfrom(BUFFER_SIZE)
        server.sendto(data, addr)


def main():
    tcp_thread = threading.Thread(target=tcp_server, daemon=True)
    udp_thread = threading.Thread(target=udp_server, daemon=True)

    tcp_thread.start()
    udp_thread.start()

    # Keep main thread alive
    try:
        tcp_thread.join()
    except KeyboardInterrupt:
        print("Shutting down echo servers")
        sys.exit(0)


if __name__ == "__main__":
    main()
