import os
import uuid
import shutil
import subprocess
import tempfile

from flask import Flask, request, jsonify
from flask_limiter import Limiter
from flask_limiter.util import get_remote_address
from flask_cors import CORS

app = Flask(__name__)
CORS(app)

limiter = Limiter(
    app=app,
    key_func=get_remote_address,
    default_limits=["10 per minute"],
    storage_uri="memory://",
)

MUX_BIN = os.environ.get("MUX_BIN", "mux")
COMPILE_TIMEOUT = int(os.environ.get("COMPILE_TIMEOUT", "30"))


@app.route("/health")
def health():
    return jsonify({"status": "ok"})


@app.route("/api/compile", methods=["POST"])
@limiter.limit("10 per minute")
def compile_code():
    data = request.get_json(silent=True)
    if not data or "code" not in data:
        return jsonify({"output": "", "error": "Missing 'code' in request body"}), 400

    code = data["code"]
    tmp_dir = None

    try:
        tmp_dir = tempfile.mkdtemp(prefix="mux_")
        src_file = os.path.join(tmp_dir, f"input_{uuid.uuid4().hex}.mux")

        with open(src_file, "w") as f:
            f.write(code)

        result = subprocess.run(
            [MUX_BIN, "run", src_file],
            capture_output=True,
            text=True,
            timeout=COMPILE_TIMEOUT,
            cwd=tmp_dir,
        )

        output = result.stdout
        error = result.stderr or None

        if result.returncode != 0:
            if error:
                error = error.strip()
            else:
                error = f"Process exited with code {result.returncode}"

        return jsonify({"output": output or "", "error": error})

    except subprocess.TimeoutExpired:
        return jsonify({
            "output": "",
            "error": f"Execution timed out after {COMPILE_TIMEOUT}s",
        })
    except FileNotFoundError:
        return jsonify({
            "output": "",
            "error": f"Compiler binary '{MUX_BIN}' not found",
        })
    except Exception as e:
        return jsonify({"output": "", "error": str(e)})
    finally:
        if tmp_dir and os.path.exists(tmp_dir):
            shutil.rmtree(tmp_dir, ignore_errors=True)


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=int(os.environ.get("PORT", 8080)))
