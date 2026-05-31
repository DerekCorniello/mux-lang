import os
import uuid
import shutil
import logging
import threading
import subprocess
import tempfile
import time
import signal
from pathlib import Path

from flask import Flask, request, jsonify
from flask_limiter import Limiter
from flask_limiter.util import get_remote_address
from flask_cors import CORS

logging.basicConfig(level=logging.INFO, format="%(asctime)s [%(levelname)s] %(message)s")
logger = logging.getLogger("mux-api")

app = Flask(__name__)
app.config["MAX_CONTENT_LENGTH"] = 512 * 1024

CORS(app, origins=[
    "https://mux-lang.dev",
    "http://localhost:3000",
])

limiter = Limiter(
    app=app,
    key_func=get_remote_address,
    default_limits=["20 per minute"],
    storage_uri="memory://",
)

MAX_CODE_SIZE = 100 * 1024
MAX_OUTPUT_SIZE = 1 * 1024 * 1024
READ_POLL_INTERVAL = 0.05


def _env_int(name: str, default: int) -> int:
    raw = os.environ.get(name)
    if raw is None:
        return default
    try:
        parsed = int(raw)
    except ValueError:
        logger.warning("Invalid %s=%r; using default=%d", name, raw, default)
        return default
    if parsed <= 0:
        logger.warning("Non-positive %s=%r; using default=%d", name, raw, default)
        return default
    return parsed


COMPILE_TIMEOUT = _env_int("COMPILE_TIMEOUT", 30)


def _find_mux() -> str:
    env_val = os.environ.get("MUX_BIN")
    if env_val:
        return env_val

    found = shutil.which("mux")
    if found:
        return found

    root = Path(__file__).resolve().parent.parent
    for candidate in [root / "target/release/mux", root / "target/debug/mux"]:
        if candidate.exists():
            return str(candidate)

    return "mux"


MUX_BIN = _find_mux()


def _read_stream(
    stream,
    chunks,
    limit,
    stop_event,
    limit_exceeded_event,
    total_bytes,
    total_lock,
):
    try:
        for chunk in iter(lambda: stream.read(4096), ""):
            if stop_event.is_set():
                break
            chunk_size = len(chunk.encode("utf-8", errors="replace"))
            with total_lock:
                total_bytes[0] += chunk_size
                if total_bytes[0] > limit:
                    limit_exceeded_event.set()
                    break
            chunks.append(chunk)
    except ValueError:
        pass
    finally:
        try:
            stream.close()
        except Exception:
            pass


def _clean_output(text: str) -> str:
    return text.replace("\x00", "")


def _kill_process_group(proc: subprocess.Popen) -> None:
    try:
        os.killpg(proc.pid, signal.SIGKILL)
    except ProcessLookupError:
        return
    except Exception:
        logger.exception("Failed to kill process group for pid=%s", proc.pid)


@app.errorhandler(413)
def request_too_large(_err):
    return jsonify({"error": "Request body exceeds 512KB limit"}), 413


@app.errorhandler(429)
def too_many_requests(_err):
    return jsonify({"error": "Too many requests. Please wait and try again."}), 429


@app.route("/health", methods=["GET"])
def health():
    return jsonify({"status": "ok"})


def _format_result(stdout, stderr, returncode, timed_out, output_too_large):
    if timed_out:
        return jsonify({"error": f"Execution timed out after {COMPILE_TIMEOUT}s"}), 504
    if output_too_large:
        return jsonify({"error": f"Program output exceeds {MAX_OUTPUT_SIZE // 1024}KB limit"}), 413
    if returncode != 0:
        msg = stderr if stderr else f"Process exited with code {returncode}"
        return jsonify({"error": msg}), 200
    return jsonify({"output": stdout}), 200


def _validate_compile_request(data):
    if not isinstance(data, dict):
        return None, (jsonify({"error": "Request body must be a JSON object"}), 400)

    if "code" not in data:
        return None, (jsonify({"error": "Missing 'code' in request body"}), 400)

    raw_code = data["code"]
    if not isinstance(raw_code, str):
        return None, (jsonify({"error": "'code' must be a string"}), 400)

    code_size = len(raw_code.encode("utf-8", errors="replace"))
    if code_size > MAX_CODE_SIZE:
        return None, (jsonify({"error": f"Source code exceeds {MAX_CODE_SIZE // 1024}KB limit"}), 413)

    return raw_code, None


def _execute_compiler(code):
    tmp_dir = tempfile.mkdtemp(prefix="mux_")
    src_file = os.path.join(tmp_dir, f"input_{uuid.uuid4().hex}.mux")
    with open(src_file, "w") as f:
        f.write(code)

    proc = subprocess.Popen(
        [MUX_BIN, "run", src_file],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8",
        errors="replace",
        cwd=tmp_dir,
        start_new_session=True,
    )

    stop_event = threading.Event()
    output_limit_exceeded = threading.Event()
    total_output_bytes = [0]
    total_output_lock = threading.Lock()
    stdout_lines = []
    stderr_lines = []

    for stream, lines in [(proc.stdout, stdout_lines), (proc.stderr, stderr_lines)]:
        t = threading.Thread(
            target=_read_stream,
            args=(stream, lines, MAX_OUTPUT_SIZE, stop_event,
                  output_limit_exceeded, total_output_bytes, total_output_lock),
            daemon=True,
        )
        t.start()

    timed_out = False
    output_too_large = False
    deadline = time.monotonic() + COMPILE_TIMEOUT

    while proc.poll() is None:
        if output_limit_exceeded.is_set():
            output_too_large = True
            _kill_process_group(proc)
            break
        if time.monotonic() >= deadline:
            timed_out = True
            _kill_process_group(proc)
            break
        time.sleep(READ_POLL_INTERVAL)

    try:
        proc.wait(timeout=1)
    except subprocess.TimeoutExpired:
        _kill_process_group(proc)
        proc.wait()

    stop_event.set()

    stdout = _clean_output("".join(stdout_lines))
    stderr = _clean_output("".join(stderr_lines)).strip()

    if len(stdout) > MAX_OUTPUT_SIZE:
        stdout = stdout[:MAX_OUTPUT_SIZE] + "\n... (output truncated)"

    return stdout, stderr, proc.returncode, timed_out, output_too_large, tmp_dir


def _compile_with_cleanup(code):
    tmp_dir = None
    try:
        stdout, stderr, returncode, timed_out, output_too_large, tmp_dir = _execute_compiler(code)
        return _format_result(stdout, stderr, returncode, timed_out, output_too_large)
    except FileNotFoundError:
        logger.error("Compiler binary not found at %s", MUX_BIN)
        return jsonify({"error": "Compiler not found on server"}), 500
    except Exception:
        logger.exception("Unexpected error during compilation")
        return jsonify({"error": "Internal server error"}), 500
    finally:
        if tmp_dir and os.path.exists(tmp_dir):
            shutil.rmtree(tmp_dir, ignore_errors=True)


@app.route("/api/compile", methods=["POST"])
@limiter.limit("20 per minute")
def compile_code():
    data = request.get_json(silent=True)
    validated_code, error_response = _validate_compile_request(data)
    if error_response:
        return error_response
    return _compile_with_cleanup(validated_code)


if __name__ == "__main__":
    app.run(host="127.0.0.1", port=int(os.environ.get("PORT", 8080)))
