#!/usr/bin/env python3
import json
import sys
from pathlib import Path


def main() -> int:
    if len(sys.argv) != 3:
        print("usage: check-timings.py <report.json> <baseline.json>", file=sys.stderr)
        return 2

    report_path = Path(sys.argv[1])
    baseline_path = Path(sys.argv[2])

    report = json.loads(report_path.read_text(encoding="utf-8"))
    baseline = json.loads(baseline_path.read_text(encoding="utf-8"))

    try:
        observed = {
            step["name"]: step["duration_seconds"] for step in report.get("steps", [])
        }
    except KeyError as exc:
        print(f"invalid report format in {report_path}: missing key {exc}", file=sys.stderr)
        return 2

    try:
        budgets = {
            step["name"]: step["max_duration_seconds"]
            for step in baseline.get("steps", [])
        }
    except KeyError as exc:
        print(
            f"invalid baseline format in {baseline_path}: missing key {exc}",
            file=sys.stderr,
        )
        return 2

    failures = []
    for name, budget in budgets.items():
        if name not in observed:
            failures.append(f"missing step '{name}' in {report_path}")
            continue
        actual = observed[name]
        if actual > budget:
            failures.append(f"{name} took {actual}s (budget {budget}s)")

    if failures:
        print("timing budget exceeded:", file=sys.stderr)
        for failure in failures:
            print(f"- {failure}", file=sys.stderr)
        return 1

    print(f"timing budgets satisfied for {report_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
