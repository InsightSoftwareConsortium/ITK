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
"""Detect and fix redundant namespace qualifiers in C++ source files.

Finds occurrences of ``itk::Symbol`` that appear inside a
``namespace itk { ... }`` block where the ``itk::`` prefix is redundant.

Only flags ``itk::`` inside ``namespace itk { }`` — NOT inside
sub-namespaces like ``namespace itk::v3 { }`` or
``namespace itk::detail { }``, because there ``itk::Foo`` may
refer to a different symbol than the unqualified ``Foo``.

Skips lines where removing ``itk::`` would trigger the
``-Wchanges-meaning`` compiler warning (e.g., ``using X = itk::X``
inside a class, or when unqualifying would shadow a class member).

Usage
-----
As a pre-commit hook (check only)::

    fix-redundant-namespace-qualifiers.py file1.cxx file2.h

With ``--fix`` to auto-correct in place::

    fix-redundant-namespace-qualifiers.py --fix file1.cxx file2.h

Exit codes: 0 = clean, 1 = violations found (or fixed).
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

# Macro names: itk prefixed macros are not namespace-scoped.
_MACRO_RE = re.compile(r"^itk[A-Z]")

# Matches "namespace itk" — must be exactly "itk", not "itk::sub".
# The opening brace may be on the same line or the next line.
_NS_ITK_ONLY_RE = re.compile(r"\bnamespace\s+itk\s*\{")
_NS_ITK_ONLY_NOBRACE_RE = re.compile(r"\bnamespace\s+itk\s*$")
# Matches "namespace itk" with a sub-namespace (itk::v3, itk::detail, etc.)
_NS_ITK_SUB_RE = re.compile(r"\bnamespace\s+itk\s*::")
# Matches any namespace opening (to track non-itk namespaces)
_NS_ANY_RE = re.compile(r"\bnamespace\s+(\w+(?:::\w+)*)\s*\{")
# Bare "namespace {" (anonymous)
_NS_ANON_RE = re.compile(r"\bnamespace\s*\{")

# Matches itk:: followed by an identifier.
_ITK_QUAL_RE = re.compile(r"\bitk::(\w+)")

# Preprocessor directive
_PREPROCESSOR_RE = re.compile(r"^\s*#")

# String/char literal (simplified)
_STRING_RE = re.compile(r'"(?:[^"\\]|\\.)*"|\'(?:[^\'\\]|\\.)*\'')

# Single-line comment
_LINE_COMMENT_RE = re.compile(r"//.*$")

# Contexts where itk:: is needed even inside namespace itk
_USING_RE = re.compile(r"\busing\s+(namespace\s+)?itk::")
_NS_ALIAS_RE = re.compile(r"\bnamespace\s+\w+\s*=\s*itk::")

# "using X = itk::X" — removing itk:: creates self-referential alias.
_USING_ALIAS_RE = re.compile(r"\busing\s+(\w+)\s*=\s*itk::(\w+)")

# Class/struct opening (to track class scope depth)
_CLASS_RE = re.compile(r"\b(class|struct)\s+(\w+)")


def _strip_comments_and_strings(line: str) -> str:
    """Return *line* with string literals and line-comments blanked out."""
    line = _STRING_RE.sub(lambda m: " " * len(m.group()), line)
    line = _LINE_COMMENT_RE.sub(lambda m: " " * len(m.group()), line)
    return line


def _compute_brace_delta(stripped: str) -> int:
    """Count net brace depth change."""
    return stripped.count("{") - stripped.count("}")


def process_file(path: Path, *, fix: bool = False) -> list[tuple[int, str, str]]:
    """Process a single file."""
    text = path.read_text(encoding="utf-8", errors="replace")
    lines = text.splitlines(keepends=True)

    # Namespace tracking: stack of (is_plain_itk, brace_depth_at_open)
    ns_stack: list[tuple[bool, int]] = []
    brace_depth = 0
    in_plain_itk = 0  # depth of plain "namespace itk" we're inside
    # Brace depth at which we entered namespace itk (for class scope detection)
    itk_ns_brace_depth = 0
    pending_ns: str | None = None

    violations: list[tuple[int, str, str]] = []
    new_lines: list[str] = lines.copy()

    in_block_comment = False

    for i, line in enumerate(lines):
        # Track block comments
        if in_block_comment:
            end_pos = line.find("*/")
            if end_pos == -1:
                continue
            in_block_comment = False

        # Check for block comment start
        temp_line = line
        while True:
            start = temp_line.find("/*")
            if start == -1:
                break
            end = temp_line.find("*/", start + 2)
            if end == -1:
                in_block_comment = True
                break
            temp_line = (
                temp_line[:start] + " " * (end + 2 - start) + temp_line[end + 2 :]
            )

        stripped = _strip_comments_and_strings(line)
        if in_block_comment and "*/" not in line:
            stripped = ""

        # Skip preprocessor directives
        if _PREPROCESSOR_RE.match(stripped):
            continue

        # Handle pending namespace (saw "namespace X" but no { yet)
        if pending_ns is not None:
            if "{" in stripped:
                brace_depth += 1
                is_plain = pending_ns == "itk"
                ns_stack.append((is_plain, brace_depth))
                if is_plain:
                    in_plain_itk += 1
                    itk_ns_brace_depth = brace_depth
                pending_ns = None
                # Account for any closing braces on same line
                close_count = stripped.count("}") - (stripped.count("{") - 1)
                if close_count > 0:
                    for _ in range(close_count):
                        brace_depth -= 1
                        while ns_stack and ns_stack[-1][1] > brace_depth:
                            was_plain, _ = ns_stack.pop()
                            if was_plain:
                                in_plain_itk -= 1
            elif "{" not in stripped:
                # Still waiting for brace, skip
                pass
            continue

        # Detect namespace openings
        # Check for "namespace itk::sub" first (sub-namespace)
        if _NS_ITK_SUB_RE.search(stripped):
            if "{" in stripped:
                brace_depth += _compute_brace_delta(stripped)
                ns_stack.append((False, brace_depth))
            else:
                pending_ns = "itk::sub"
            continue
        # Check for "namespace itk {" (plain itk, brace on same line)
        if _NS_ITK_ONLY_RE.search(stripped):
            brace_depth += _compute_brace_delta(stripped)
            ns_stack.append((True, brace_depth))
            in_plain_itk += 1
            itk_ns_brace_depth = brace_depth
            continue
        # Check for "namespace itk" with brace on next line
        if _NS_ITK_ONLY_NOBRACE_RE.search(stripped):
            pending_ns = "itk"
            continue
        # Check for anonymous namespace or other named namespace
        has_ns = _NS_ANON_RE.search(stripped) or _NS_ANY_RE.search(stripped)
        if has_ns and "=" not in stripped.split("namespace")[0]:
            if "{" in stripped:
                brace_depth += _compute_brace_delta(stripped)
                ns_stack.append((False, brace_depth))
            else:
                pending_ns = "other"
            continue

        # Update brace depth for non-namespace lines
        delta = _compute_brace_delta(stripped)
        brace_depth += delta

        # Check if we left any namespace blocks
        while ns_stack and brace_depth < ns_stack[-1][1]:
            was_plain, _ = ns_stack.pop()
            if was_plain:
                in_plain_itk -= 1

        # If we're directly inside "namespace itk" (not a sub-namespace),
        # check for redundant itk:: usage
        if in_plain_itk > 0 and "itk::" in stripped:
            # Skip using declarations (using namespace itk::...)
            if _USING_RE.search(stripped):
                continue
            # Skip namespace aliases
            if _NS_ALIAS_RE.search(stripped):
                continue

            # Skip when inside a nested scope (class/struct/enum body).
            # Inside class definitions, removing itk:: can cause:
            #   - -Wchanges-meaning (using X = itk::X shadows member)
            #   - dependent name lookup failures in templates
            # Only fix at namespace-level (brace depth == namespace depth).
            if brace_depth > itk_ns_brace_depth:
                continue

            # Find replaceable itk::Symbol occurrences
            has_replaceable = False
            for m in _ITK_QUAL_RE.finditer(stripped):
                symbol = m.group(1)
                if _MACRO_RE.match(symbol):
                    continue
                has_replaceable = True

            if has_replaceable:
                fixed_line = _replace_in_code(line)
                if fixed_line != line:
                    violations.append((i + 1, line, fixed_line))
                    new_lines[i] = fixed_line

    if fix and violations:
        path.write_text("".join(new_lines), encoding="utf-8")

    return violations


def _replace_in_code(line: str, protected_symbols: set[str] | None = None) -> str:
    """Remove redundant ``itk::`` qualifiers in code portions of *line*.

    Preserves string literals, character literals, and ``//`` comments.
    Symbols in *protected_symbols* are not modified.
    """
    if protected_symbols is None:
        protected_symbols = set()

    # Build a map of "protected" ranges (strings, comments)
    protected: list[tuple[int, int]] = []
    for m in _STRING_RE.finditer(line):
        protected.append((m.start(), m.end()))
    cm = _LINE_COMMENT_RE.search(line)
    if cm:
        protected.append((cm.start(), cm.end()))

    def _in_protected(pos: int) -> bool:
        return any(s <= pos < e for s, e in protected)

    def _replacer(m: re.Match) -> str:
        symbol = m.group(1)
        if _MACRO_RE.match(symbol):
            return m.group(0)
        if symbol in protected_symbols:
            return m.group(0)
        return symbol

    result = []
    last = 0
    for m in _ITK_QUAL_RE.finditer(line):
        if _in_protected(m.start()):
            continue
        result.append(line[last : m.start()])
        result.append(_replacer(m))
        last = m.end()
    result.append(line[last:])
    return "".join(result)


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Detect/fix redundant itk:: qualifiers inside namespace itk."
    )
    parser.add_argument("files", nargs="*", type=Path, help="Files to check")
    parser.add_argument(
        "--fix",
        action="store_true",
        help="Auto-fix violations in place",
    )
    args = parser.parse_args()

    total_violations = 0
    for fpath in args.files:
        if not fpath.is_file():
            continue
        if fpath.suffix not in {".cxx", ".h", ".hxx", ".txx", ".hpp", ".cpp", ".cc"}:
            continue
        violations = process_file(fpath, fix=args.fix)
        for lineno, old, new in violations:
            action = "Fixed" if args.fix else "Found"
            print(f"{fpath}:{lineno}: {action} redundant itk:: qualifier")
            print(f"  - {old.rstrip()}")
            print(f"  + {new.rstrip()}")
        total_violations += len(violations)

    if total_violations:
        action = "Fixed" if args.fix else "Found"
        print(
            f"\n{action} {total_violations} redundant itk:: qualifier(s) "
            f"in {len(args.files)} file(s)."
        )
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
