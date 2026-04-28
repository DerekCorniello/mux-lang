#!/usr/bin/env python3
import json
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import parse_qs, urlparse


class Handler(BaseHTTPRequestHandler):
    server_version = "MuxHttpEcho/1.0"

    def _write_json(self, status_code, body):
        payload = json.dumps(body, separators=(",", ":")).encode("utf-8")
        self.send_response(status_code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(payload)))
        self.send_header("X-Mux-Service", "http-echo")
        self.end_headers()
        self.wfile.write(payload)

    def _body_json(self):
        length = int(self.headers.get("Content-Length", "0"))
        if length <= 0:
            return None
        body = self.rfile.read(length)
        if not body:
            return None
        text = body.decode("utf-8")
        try:
            return json.loads(text)
        except json.JSONDecodeError:
            return text

    def do_GET(self):
        parsed = urlparse(self.path)
        if parsed.path == "/healthz":
            self._write_json(200, {"ok": True})
            return

        self._write_json(
            200,
            {
                "method": "GET",
                "path": parsed.path,
                "query": parsed.query,
                "query_params": {k: v for k, v in parse_qs(parsed.query).items()},
                "body": None,
            },
        )

    def do_POST(self):
        parsed = urlparse(self.path)
        self._write_json(
            200,
            {
                "method": "POST",
                "path": parsed.path,
                "query": parsed.query,
                "query_params": {k: v for k, v in parse_qs(parsed.query).items()},
                "body": self._body_json(),
            },
        )

    def log_message(self, _format, *_args):
        return


if __name__ == "__main__":
    server = ThreadingHTTPServer(("0.0.0.0", 8080), Handler)
    server.serve_forever()
