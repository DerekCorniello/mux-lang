#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/.." && pwd)"

export LOCAL_UID="${LOCAL_UID:-$(id -u)}"
export LOCAL_GID="${LOCAL_GID:-$(id -g)}"

mkdir -p "$repo_root/.docker-cache/cargo/registry" "$repo_root/.docker-cache/cargo/git" "$repo_root/.docker-cache/target"
mkdir -p "$repo_root/.docker-home/.cache"

"$repo_root/scripts/wait-for-integration.sh"

docker compose -f "$repo_root/infra/docker-compose.integration.yml" build dev

docker compose -f "$repo_root/infra/docker-compose.integration.yml" up -d dev
