#!/usr/bin/env python3
import socket
import threading

HOST = "0.0.0.0"
PORT = 10000
PREFIX = b"ACK:"


def handle(conn):
    with conn:
        payload = conn.recv(1024)
        if payload:
            conn.sendall(PREFIX + payload)


def main():
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server.bind((HOST, PORT))
    server.listen()

    while True:
        conn, _ = server.accept()
        thread = threading.Thread(target=handle, args=(conn,), daemon=True)
        thread.start()


if __name__ == "__main__":
    main()
