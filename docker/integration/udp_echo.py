#!/usr/bin/env python3
import socket

HOST = "0.0.0.0"
PORT = 11000
PREFIX = b"UDP:"


def main():
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.bind((HOST, PORT))

    while True:
        data, addr = sock.recvfrom(2048)
        if data:
            sock.sendto(PREFIX + data, addr)


if __name__ == "__main__":
    main()
