#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/.." && pwd)"

export LOCAL_UID="${LOCAL_UID:-$(id -u)}"
export LOCAL_GID="${LOCAL_GID:-$(id -g)}"

docker compose -f "$repo_root/infra/docker-compose.integration.yml" up -d postgres http-echo tcp-echo
docker compose -f "$repo_root/infra/docker-compose.integration.yml" up -d udp-echo

docker compose -f "$repo_root/infra/docker-compose.integration.yml" up -d --wait postgres http-echo tcp-echo udp-echo
