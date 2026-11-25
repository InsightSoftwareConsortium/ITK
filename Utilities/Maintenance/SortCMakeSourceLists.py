#!/usr/bin/env python3
# ==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
# ==========================================================================
"""
SortCMakeSourceLists.py — alphabetize source-file entries in CMake set() and
list(APPEND) commands across the ITK source tree.

This is a manually runnable maintenance script, not a pre-commit hook.

What it does
------------
Scans every CMakeLists.txt and *.cmake file under the ITK source tree
(excluding Modules/ThirdParty) and rewrites any ``set(VAR ...)`` or
``list(APPEND VAR ...)`` command whose argument list looks like a C/C++
source- or header-file list, sorting the entries alphabetically so that
future diffs and merge conflicts are easier to interpret.

Detection rules
---------------
A command is treated as a "source file list" if either:

  1. The variable name ends with one of the conventional suffixes used
     by ITK module CMakeLists files:

         _SRCS, _SOURCES, _SRC,
         _HDR, _HDRS, _HEADERS,
         _SOURCE_FILES, _HEADER_FILES

  2. At least 80% of the *plain-text* (non-substitution) entries have
     an extension in this set:

         .cxx .cpp .cc .c .h .hxx .hpp .hh

The command body must be in ITK's canonical "one token per line" layout:

     set(
       VAR_NAME
       file1.cxx
       file2.cxx
       ...
     )

or the equivalent

     list(
       APPEND
       VAR_NAME
       file1.cxx
       ...
     )

Any command that is on a single line, or whose body contains comments,
blank lines, or multi-token lines, is left untouched. This is a safety
feature: the script only rewrites commands whose structure it fully
understands.

Sort rules
----------
* Entries containing a CMake variable substitution ``${...}`` stay at
  the top of the list in their original relative order. This keeps
  generated/binary-dir paths grouped at the top.
* Remaining (plain-filename) entries are sorted case-insensitively, with
  a case-sensitive tiebreaker for determinism.
* If *every* entry contains ``${...}``, the list is skipped — its order
  is likely semantic.
* Lists with fewer than two plain entries are skipped.

Self-verification
-----------------
Before writing a modified file, the script verifies that the multiset of
whitespace-separated word tokens in the file is identical before and
after transformation. If they differ, the file is left unchanged and a
warning is printed. This guarantees that a sort cannot accidentally add,
drop, or rename any token (including CMake keywords, variable names, or
file entries).

Usage
-----
    # Sort the whole tree (in place):
    python3 Utilities/Maintenance/SortCMakeSourceLists.py

    # Preview what would change without writing:
    python3 Utilities/Maintenance/SortCMakeSourceLists.py --dry-run

    # Target a different tree root:
    python3 Utilities/Maintenance/SortCMakeSourceLists.py /path/to/ITK

Exit status
-----------
  0  success (with or without changes)
  1  self-verification failure on at least one file
  2  usage / fatal error
"""

from __future__ import annotations

import argparse
import re
import sys
from collections import Counter
from pathlib import Path

SOURCE_EXTS = (
    ".cxx",
    ".cpp",
    ".cc",
    ".c",
    ".h",
    ".hxx",
    ".hpp",
    ".hh",
)
VAR_NAME_SUFFIXES = (
    "_SRCS",
    "_SOURCES",
    "_SRC",
    "_HDR",
    "_HDRS",
    "_HEADERS",
    "_SOURCE_FILES",
    "_HEADER_FILES",
)

# Canonical "one token per line" body pattern. Each body line must be:
#   <one or more spaces/tabs>
#   <first char: not whitespace, not paren, not #>
#   <remaining non-whitespace chars>
#   <optional trailing spaces>
#   <newline>
# A comment line, blank line, or multi-token line will cause the regex to
# fail to match the surrounding command, which is intentional.
_BODY_LINE = r"[ \t]+[^\s()#][^\s]*[ \t]*\n"
_BODY = rf"(?:{_BODY_LINE})+"

# set(VAR_NAME
#   entry1
#   entry2
#   ...
# )
SET_RE = re.compile(
    rf"""
    (set)\s*\(\s*
    ([A-Za-z_][A-Za-z0-9_]*)\s*\n
    ({_BODY})
    ([ \t]*)\)
    """,
    re.VERBOSE,
)

# list(
#   APPEND
#   VAR_NAME
#   entry1
#   ...
# )
# or the equivalent with APPEND on the opening line.
LIST_APPEND_RE = re.compile(
    rf"""
    (list)\s*\(\s*
    APPEND\s+
    ([A-Za-z_][A-Za-z0-9_]*)\s*\n
    ({_BODY})
    ([ \t]*)\)
    """,
    re.VERBOSE,
)


def looks_like_source(tok: str) -> bool:
    name = tok.rsplit("/", 1)[-1]
    return name.lower().endswith(SOURCE_EXTS)


def is_source_list(var_name: str, args: list[str]) -> bool:
    if any(var_name.endswith(suf) for suf in VAR_NAME_SUFFIXES):
        return True
    plain = [a for a in args if "${" not in a]
    if not plain:
        return False
    matches = sum(1 for a in plain if looks_like_source(a))
    return (matches / len(plain)) >= 0.8


def compute_sorted_args(args: list[str]) -> list[str] | None:
    """Return sorted args, or None if the list should be skipped."""
    sub_args = [a for a in args if "${" in a]
    plain_args = [a for a in args if "${" not in a]
    if len(plain_args) < 2:
        return None  # nothing meaningful to sort
    sorted_plain = sorted(plain_args, key=lambda s: (s.lower(), s))
    return sub_args + sorted_plain


def _rewrite_match(match: re.Match) -> str:
    var = match.group(2)
    body = match.group(3)

    body_lines = body.splitlines(keepends=False)
    if not body_lines:
        return match.group(0)

    entries: list[str] = []
    indents: list[str] = []
    for line in body_lines:
        stripped = line.lstrip(" \t")
        indent = line[: len(line) - len(stripped)]
        token = stripped.rstrip(" \t")
        entries.append(token)
        indents.append(indent)

    if len(set(indents)) != 1:
        return match.group(0)
    indent = indents[0]

    if len(entries) < 2:
        return match.group(0)

    if not is_source_list(var, entries):
        return match.group(0)

    sorted_entries = compute_sorted_args(entries)
    if sorted_entries is None:
        return match.group(0)
    if sorted_entries == entries:
        return match.group(0)

    new_body = "".join(f"{indent}{e}\n" for e in sorted_entries)
    original = match.group(0)
    body_start = match.start(3) - match.start(0)
    body_end = match.end(3) - match.start(0)
    return original[:body_start] + new_body + original[body_end:]


def _token_multiset(text: str) -> Counter[str]:
    """Return a multiset of whitespace-separated tokens for self-verification."""
    return Counter(text.split())


def process_file(path: Path, dry_run: bool) -> tuple[bool, bool]:
    """Return (changed, verified_ok)."""
    original = path.read_text(encoding="utf-8")

    text = SET_RE.sub(_rewrite_match, original)
    text = LIST_APPEND_RE.sub(_rewrite_match, text)

    if text == original:
        return False, True

    if _token_multiset(text) != _token_multiset(original):
        sys.stderr.write(
            f"WARNING: token multiset mismatch after sort; "
            f"leaving unchanged: {path}\n"
        )
        return False, False

    if not dry_run:
        path.write_text(text, encoding="utf-8")
    return True, True


def gather_targets(root: Path) -> list[Path]:
    excluded = (root / "Modules" / "ThirdParty").resolve()
    targets: list[Path] = []
    # Single walk that filters by name/suffix.  Using one rglob avoids the
    # potential for duplicate paths if a future filesystem entry could match
    # multiple patterns (e.g. a hypothetical "CMakeLists.cmake").
    for p in root.rglob("*"):
        if not p.is_file():
            continue
        if p.name != "CMakeLists.txt" and p.suffix != ".cmake":
            continue
        if ".git" in p.parts:
            continue
        try:
            p.resolve().relative_to(excluded)
            continue  # inside ThirdParty — skip
        except ValueError:
            pass
        targets.append(p)
    return sorted(targets)


def main() -> int:
    ap = argparse.ArgumentParser(
        description=(
            "Alphabetically sort source file entries in CMake "
            "set()/list(APPEND) commands across the ITK tree."
        ),
    )
    ap.add_argument(
        "root",
        type=Path,
        nargs="?",
        default=Path.cwd(),
        help="ITK source tree root (default: current directory)",
    )
    ap.add_argument(
        "--dry-run",
        action="store_true",
        help="Do not modify files; only report which files would change",
    )
    args = ap.parse_args()

    root = args.root.resolve()
    if not (root / "CMakeLists.txt").exists():
        sys.stderr.write(f"ERROR: no CMakeLists.txt at {root}\n")
        return 2

    targets = gather_targets(root)
    changed = 0
    failed = 0
    for p in targets:
        did_change, ok = process_file(p, args.dry_run)
        if not ok:
            failed += 1
            continue
        if did_change:
            changed += 1
            rel = p.relative_to(root)
            prefix = "[DRY] " if args.dry_run else ""
            print(f"{prefix}sorted: {rel}")

    action = "would change" if args.dry_run else "changed"
    print(f"\nTotal files {action}: {changed}")
    if failed:
        sys.stderr.write(f"Self-verification failures: {failed}\n")
        return 1
    if args.dry_run and changed:
        # Non-zero exit lets CI gate on drift without actually rewriting files.
        return 3
    return 0


if __name__ == "__main__":
    sys.exit(main())
