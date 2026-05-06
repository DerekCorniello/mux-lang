#!/usr/bin/env python3

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Any


REPO_ROOT = Path(__file__).resolve().parent.parent
SPEC_PATH = REPO_ROOT / "editor-support" / "spec" / "definitions.json"


def read_spec() -> dict[str, Any]:
    with SPEC_PATH.open("r", encoding="utf-8") as file:
        return json.load(file)


def grouped_keywords(spec: dict[str, Any]) -> list[str]:
    keyword_groups = spec["keywords"]
    return keyword_groups["control"] + keyword_groups["declaration"]


def escape_regex_terms(terms: list[str]) -> list[str]:
    escaped: list[str] = []
    for term in terms:
        value = term
        for ch in ".^$*+?{}[]|()":
            value = value.replace(ch, f"\\{ch}")
        escaped.append(value)
    return escaped


def build_textmate(spec: dict[str, Any]) -> dict[str, Any]:
    language = spec["language"]
    keyword_pattern = "|".join(escape_regex_terms(grouped_keywords(spec)))
    literal_pattern = "|".join(escape_regex_terms(spec["keywords"]["literal"]))
    type_pattern = "|".join(escape_regex_terms(spec["types"]["builtin"]))

    operator_terms: list[str] = []
    for operator_group in spec["operators"].values():
        operator_terms.extend(operator_group)
    operator_pattern = "|".join(escape_regex_terms(list(dict.fromkeys(operator_terms))))

    return {
        "$schema": "https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json",
        "name": language["name"],
        "scopeName": language["scope_name"],
        "fileTypes": language["file_extensions"],
        "patterns": [
            {"include": "#comments"},
            {"include": "#strings"},
            {"include": "#numbers"},
            {"include": "#keywords"},
            {"include": "#types"},
            {"include": "#operators"},
            {"include": "#punctuation"},
            {"include": "#identifiers"},
        ],
        "repository": {
            "comments": {
                "patterns": [
                    {"name": "comment.line.double-slash.mux", "match": "//.*$"},
                    {
                        "name": "comment.block.mux",
                        "begin": "/\\*",
                        "end": "\\*/",
                    },
                ]
            },
            "strings": {
                "patterns": [
                    {
                        "name": "string.quoted.double.mux",
                        "begin": '"',
                        "end": '"',
                        "patterns": [
                            {"name": "constant.character.escape.mux", "match": r"\\\\."}
                        ],
                    },
                    {
                        "name": "string.quoted.single.mux",
                        "begin": "'",
                        "end": "'",
                        "patterns": [
                            {"name": "constant.character.escape.mux", "match": r"\\\\."}
                        ],
                    },
                ]
            },
            "numbers": {
                "patterns": [
                    {"name": "constant.numeric.mux", "match": spec["regex"]["number"]}
                ]
            },
            "keywords": {
                "patterns": [
                    {"name": "keyword.control.mux", "match": rf"\b(?:{keyword_pattern})\b"},
                    {
                        "name": "constant.language.mux",
                        "match": rf"\b(?:{literal_pattern})\b",
                    },
                ]
            },
            "types": {
                "patterns": [
                    {"name": "storage.type.builtin.mux", "match": rf"\b(?:{type_pattern})\b"}
                ]
            },
            "operators": {
                "patterns": [
                    {"name": "keyword.operator.mux", "match": rf"(?:{operator_pattern})"}
                ]
            },
            "punctuation": {
                "patterns": [
                    {
                        "name": "punctuation.bracket.mux",
                        "match": r"[(){}\[\]]",
                    },
                    {
                        "name": "punctuation.separator.mux",
                        "match": r"[,:]",
                    },
                ]
            },
            "identifiers": {
                "patterns": [
                    {
                        "name": "variable.other.readwrite.mux",
                        "match": spec["regex"]["identifier"],
                    }
                ]
            },
        },
    }


def build_sublime_syntax(spec: dict[str, Any]) -> str:
    language = spec["language"]
    keywords = grouped_keywords(spec)
    literals = spec["keywords"]["literal"]
    types = spec["types"]["builtin"]

    operator_terms: list[str] = []
    for operator_group in spec["operators"].values():
        operator_terms.extend(operator_group)

    keyword_pattern = "|".join(escape_regex_terms(keywords))
    literal_pattern = "|".join(escape_regex_terms(literals))
    type_pattern = "|".join(escape_regex_terms(types))
    operator_pattern = "|".join(escape_regex_terms(sorted(operator_terms, key=len, reverse=True)))

    return "\n".join(
        [
            "%YAML 1.2",
            "---",
            f'name: {language["name"]}',
            "file_extensions:",
            *[f"  - {ext}" for ext in language["file_extensions"]],
            f'scope: {language["scope_name"]}',
            "contexts:",
            "  main:",
            "    - include: comments",
            "    - include: strings",
            "    - include: numbers",
            "    - include: keywords",
            "    - include: types",
            "    - include: operators",
            "    - include: punctuation",
            "  comments:",
            "    - match: //.*$",
            "      scope: comment.line.double-slash.mux",
            "    - match: /\\*",
            "      scope: punctuation.definition.comment.begin.mux",
            "      push:",
            "        - meta_scope: comment.block.mux",
            "        - match: \\*/",
            "          scope: punctuation.definition.comment.end.mux",
            "          pop: true",
            "  strings:",
            "    - match: '\"'",
            "      scope: punctuation.definition.string.begin.mux",
            "      push:",
            "        - meta_scope: string.quoted.double.mux",
            "        - match: '\\.'",
            "          scope: constant.character.escape.mux",
            "        - match: '\"'",
            "          scope: punctuation.definition.string.end.mux",
            "          pop: true",
            "    - match: \"'\"",
            "      scope: punctuation.definition.string.begin.mux",
            "      push:",
            "        - meta_scope: string.quoted.single.mux",
            "        - match: '\\.'",
            "          scope: constant.character.escape.mux",
            "        - match: \"'\"",
            "          scope: punctuation.definition.string.end.mux",
            "          pop: true",
            "  numbers:",
            f"    - match: '{spec['regex']['number']}'",
            "      scope: constant.numeric.mux",
            "  keywords:",
            f"    - match: '\\b(?:{keyword_pattern})\\b'",
            "      scope: keyword.control.mux",
            f"    - match: '\\b(?:{literal_pattern})\\b'",
            "      scope: constant.language.mux",
            "  types:",
            f"    - match: '\\b(?:{type_pattern})\\b'",
            "      scope: storage.type.builtin.mux",
            "  operators:",
            f"    - match: '(?:{operator_pattern})'",
            "      scope: keyword.operator.mux",
            "  punctuation:",
            "    - match: '[(){}\\[\\]]'",
            "      scope: punctuation.bracket.mux",
            "    - match: '[,:]'",
            "      scope: punctuation.separator.mux",
            "...",
            "",
        ]
    )


def build_treesitter_query(spec: dict[str, Any]) -> str:
    keywords = grouped_keywords(spec)
    literals = spec["keywords"]["literal"]
    types = spec["types"]["builtin"]

    operator_terms: list[str] = []
    for operator_group in spec["operators"].values():
        operator_terms.extend(operator_group)

    lines: list[str] = [
        ";;; Generated by scripts/build_syntax_highlighting.py",
        ";;; Keep in sync with editor-support/spec/definitions.json",
        "",
        "(comment) @comment",
        "(line_comment) @comment",
        "(block_comment) @comment",
        "(string_literal) @string",
        "(char_literal) @string.special",
        "(int_literal) @number",
        "(float_literal) @number.float",
        "",
        "(identifier) @variable",
        "(function_declaration name: (identifier) @function)",
        "(call_expression function: (identifier) @function.call)",
        "(type_identifier) @type",
        "",
        "[",
    ]
    for keyword in keywords:
        lines.append(f'  "{keyword}"')
    lines.extend(["] @keyword", "", "["])
    for literal in literals:
        lines.append(f'  "{literal}"')
    lines.extend(["] @constant.builtin", "", "["])
    for typ in types:
        lines.append(f'  "{typ}"')
    lines.extend(["] @type.builtin", "", "["])
    for operator in dict.fromkeys(operator_terms):
        lines.append(f'  "{operator}"')
    lines.extend(["] @operator", "", "["])
    for bracket in spec["punctuation"]["brackets"]:
        lines.append(f'  "{bracket}"')
    lines.extend(["] @punctuation.bracket", "", "["])
    for delimiter in spec["punctuation"]["delimiters"]:
        lines.append(f'  "{delimiter}"')
    lines.extend(["] @punctuation.delimiter", ""])

    return "\n".join(lines)


def build_language_configuration() -> dict[str, Any]:
    return {
        "comments": {
            "lineComment": "//",
            "blockComment": ["/*", "*/"],
        },
        "brackets": [["{", "}"], ["[", "]"], ["(", ")"]],
        "autoClosingPairs": [
            {"open": "{", "close": "}"},
            {"open": "[", "close": "]"},
            {"open": "(", "close": ")"},
            {"open": '"', "close": '"'},
            {"open": "'", "close": "'"},
        ],
        "surroundingPairs": [["{", "}"], ["[", "]"], ["(", ")"], ['"', '"'], ["'", "'"]],
    }


def write_text(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content, encoding="utf-8")


def write_json(path: Path, payload: dict[str, Any]) -> None:
    write_text(path, json.dumps(payload, indent=2) + "\n")


def generate_outputs(spec: dict[str, Any]) -> dict[Path, str]:
    textmate = build_textmate(spec)
    sublime = build_sublime_syntax(spec)
    treesitter = build_treesitter_query(spec)
    language_config = build_language_configuration()

    textmate_json = json.dumps(textmate, indent=2) + "\n"
    language_config_json = json.dumps(language_config, indent=2) + "\n"

    return {
        REPO_ROOT / "editor-support" / "textmate" / "mux.tmLanguage.json": textmate_json,
        REPO_ROOT / "editor-support" / "vscode" / "syntaxes" / "mux.tmLanguage.json": textmate_json,
        REPO_ROOT / "editor-support" / "vscode" / "language-configuration.json": language_config_json,
        REPO_ROOT / "editor-support" / "jetbrains" / "textmate" / "mux.tmLanguage.json": textmate_json,
        REPO_ROOT / "editor-support" / "sublime" / "Mux.sublime-syntax": sublime,
        REPO_ROOT / "editor-support" / "treesitter" / "queries" / "highlights.scm": treesitter,
        REPO_ROOT / "editor-support" / "neovim" / "queries" / "mux" / "highlights.scm": treesitter,
        REPO_ROOT / "editor-support" / "helix" / "runtime" / "queries" / "mux" / "highlights.scm": treesitter,
    }


def run(check_only: bool) -> int:
    spec = read_spec()
    outputs = generate_outputs(spec)

    had_difference = False
    for path, expected in outputs.items():
        if path.exists():
            actual = path.read_text(encoding="utf-8")
            if actual != expected:
                had_difference = True
                if check_only:
                    print(f"Stale: {path}", file=sys.stderr)
                if not check_only:
                    write_text(path, expected)
        else:
            had_difference = True
            if check_only:
                print(f"Stale: {path}", file=sys.stderr)
            if not check_only:
                write_text(path, expected)

    if check_only and had_difference:
        return 1

    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description="Build Mux syntax highlighting artifacts.")
    parser.add_argument(
        "--check",
        action="store_true",
        help="Validate generated syntax artifacts are up to date.",
    )
    args = parser.parse_args()
    return run(check_only=args.check)


if __name__ == "__main__":
    raise SystemExit(main())
