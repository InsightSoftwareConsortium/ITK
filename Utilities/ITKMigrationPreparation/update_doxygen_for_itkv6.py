#!/usr/bin/env python3
"""Rewrite deprecated Doxygen aliases to their ITKv6 spellings.

Replaces the deprecated ``doxygen``/``subdoxygen`` ITK Doxygen aliases with
``itkref``/``itksubref`` across a source tree. Run from the root of any
project whose documentation uses them:

    python3 update_doxygen_for_itkv6.py [--dry-run] [paths ...]
"""

import argparse
import re
import sys
from pathlib import Path

REPLACEMENTS = (
    (re.compile(r"\\doxygen\{"), r"\\itkref{"),
    (re.compile(r"\\subdoxygen\{"), r"\\itksubref{"),
)

SUFFIXES = {
    ".h",
    ".hxx",
    ".txx",
    ".cxx",
    ".cpp",
    ".dox",
    ".md",
    ".rst",
    ".tex",
    ".py",
    ".wrap",
}


def rewrite(path: Path, dry_run: bool) -> int:
    try:
        original = path.read_text(encoding="utf-8")
    except (UnicodeDecodeError, OSError):
        return 0
    text = original
    count = 0
    for pattern, replacement in REPLACEMENTS:
        text, n = pattern.subn(replacement, text)
        count += n
    if count and not dry_run:
        path.write_text(text, encoding="utf-8")
    return count


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "paths", nargs="*", default=["."], help="files or directories to rewrite"
    )
    parser.add_argument(
        "--dry-run", action="store_true", help="report changes without writing"
    )
    args = parser.parse_args()

    total = 0
    changed_files = 0
    for root in args.paths:
        root_path = Path(root)
        candidates = (
            [root_path]
            if root_path.is_file()
            else (p for p in root_path.rglob("*") if p.is_file())
        )
        for path in candidates:
            if path.suffix not in SUFFIXES or ".git" in path.parts:
                continue
            count = rewrite(path, args.dry_run)
            if count:
                changed_files += 1
                total += count
                print(
                    f"{path}: {count} replacement(s){' (dry run)' if args.dry_run else ''}"
                )
    print(f"{total} replacement(s) in {changed_files} file(s)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
