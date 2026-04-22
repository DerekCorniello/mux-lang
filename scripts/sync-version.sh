#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

if [[ -n "${1:-}" ]]; then
  echo "Usage: $0"
  exit 1
fi

python3 - "$REPO_ROOT" <<'PY'
import json
import re
import sys
from pathlib import Path

repo_root = Path(sys.argv[1])

version_file = repo_root / "VERSION"
compiler_toml = repo_root / "mux-compiler" / "Cargo.toml"
runtime_toml = repo_root / "mux-runtime" / "Cargo.toml"
readme = repo_root / "README.md"
website_package = repo_root / "mux-website" / "package.json"
changelog = repo_root / "CHANGELOG.md"


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def write_text(path: Path, content: str) -> None:
    path.write_text(content, encoding="utf-8")


def validate_version(raw_value: str) -> str:
    version = raw_value.strip()
    semver_pattern = r"^[0-9]+\.[0-9]+\.[0-9]+([.-][0-9A-Za-z.-]+)?$"
    if not re.match(semver_pattern, version):
        raise SystemExit(f"VERSION must be a semantic version (got '{version}')")
    return version


def check_changelog(version: str) -> None:
    latest_release = None
    for line in read_text(changelog).splitlines():
        if not line.startswith("## ["):
            continue
        header = line[4:].split("]", 1)[0].lstrip("[")
        if header == "Unreleased":
            continue
        latest_release = header
        break

    if latest_release is None:
        raise SystemExit("CHANGELOG.md has no release sections")
    if latest_release != version:
        raise SystemExit(
            f"CHANGELOG.md is not up to date: latest section is [{latest_release}], expected [{version}]"
        )


def update_toml_scalar(path: Path, section: str, key: str, value: str) -> None:
    lines = read_text(path).splitlines()
    in_section = False
    replaced = False

    for index, line in enumerate(lines):
        stripped = line.strip()
        if stripped.startswith("[") and stripped.endswith("]") and not stripped.startswith("[["):
            in_section = stripped == f"[{section}]"
            continue
        if not in_section:
            continue
        if stripped.startswith(f"{key} = "):
            indent = line[: len(line) - len(line.lstrip())]
            lines[index] = f'{indent}{key} = "{value}"'
            replaced = True
            break

    if not replaced:
        raise SystemExit(f"Could not update {key} in [{section}] for {path}")

    write_text(path, "\n".join(lines) + "\n")


def update_readme(version: str) -> None:
    lines = read_text(readme).splitlines()
    badge_line = (
        f"[![Version](https://img.shields.io/badge/version-{version}-blue.svg?style=flat-square)]"
        "(https://github.com/DerekCorniello/mux-lang/releases)"
    )
    current_version_line = f"- **Current Version:** {version}"

    badge_updated = False
    current_version_updated = False

    for index, line in enumerate(lines):
        if "img.shields.io/badge/version-" in line and line.startswith("[![Version]("):
            lines[index] = badge_line
            badge_updated = True
        if line.startswith("- **Current Version:**"):
            lines[index] = current_version_line
            current_version_updated = True

    if not badge_updated:
        raise SystemExit("Could not find README version badge line")
    if not current_version_updated:
        raise SystemExit("Could not find README current version line")

    write_text(readme, "\n".join(lines) + "\n")


def read_json(path: Path):
    return json.loads(read_text(path))


def update_website_package(version: str) -> None:
    package_data = read_json(website_package)
    package_data["version"] = version
    write_text(website_package, json.dumps(package_data, indent=2) + "\n")


def is_synced(version: str) -> tuple[bool, list[str]]:
    failures: list[str] = []

    compiler = read_text(compiler_toml)
    runtime = read_text(runtime_toml)
    readme_text = read_text(readme)
    package_data = read_json(website_package)

    if f'version = "{version}"' not in compiler:
        failures.append("mux-compiler/Cargo.toml package version")
    if f'mux-runtime = "{version}"' not in compiler:
        failures.append("mux-compiler/Cargo.toml mux-runtime dependency version")
    if f'version = "{version}"' not in runtime:
        failures.append("mux-runtime/Cargo.toml package version")
    if f"img.shields.io/badge/version-{version}-blue.svg?style=flat-square" not in readme_text:
        failures.append("README.md version badge")
    if f"- **Current Version:** {version}" not in readme_text:
        failures.append("README.md current version")
    if package_data.get("version") != version:
        failures.append("mux-website/package.json version")

    return len(failures) == 0, failures


if not version_file.exists():
    raise SystemExit(f"Missing VERSION file at {version_file}")

version = validate_version(read_text(version_file))
check_changelog(version)

synced, failures = is_synced(version)
if synced:
    print(f"Version check passed for {version}")
    raise SystemExit(0)

update_toml_scalar(compiler_toml, "package", "version", version)
update_toml_scalar(compiler_toml, "dependencies", "mux-runtime", version)
update_toml_scalar(runtime_toml, "package", "version", version)
update_readme(version)
update_website_package(version)

print(
    "Synchronized Mux version references to "
    f"{version}; package-lock.json should be updated via npm if needed"
)
PY
