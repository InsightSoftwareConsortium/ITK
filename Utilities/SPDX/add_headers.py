#!/usr/bin/env python3
# SPDX-FileCopyrightText: Copyright NumFOCUS
# SPDX-License-Identifier: Apache-2.0
"""Add SPDX license and copyright headers to ITK source files.

Follows VTK's convention: two // comment lines before the existing
license block.

For C/C++ files (.h, .hxx, .cxx, .txx):
    // SPDX-FileCopyrightText: Copyright NumFOCUS
    // SPDX-License-Identifier: Apache-2.0
    /*=========================================================================
    ...existing header...

For Python files (.py):
    # SPDX-FileCopyrightText: Copyright NumFOCUS
    # SPDX-License-Identifier: Apache-2.0
    # ==========================================================================
    ...existing header...

For CMake files (.cmake, CMakeLists.txt):
    # SPDX-FileCopyrightText: Copyright NumFOCUS
    # SPDX-License-Identifier: Apache-2.0
    ...existing content...

Only modifies files that:
  - Contain "Copyright NumFOCUS" (ITK's standard header)
  - Do NOT already contain "SPDX-License-Identifier"
  - Are NOT under ThirdParty directories

Usage:
    # Walk the whole tree (repo root deduced from script location):
    python3 add_headers.py [--dry-run] [ITK_SOURCE_DIR]

    # Operate on a specific list of files (for pre-commit integration):
    python3 add_headers.py --files <file1> <file2> ...

    # Check-only mode: exit non-zero if any file needs SPDX. Intended
    # for use as a pre-commit hook.
    python3 add_headers.py --check --files <file1> <file2> ...
"""

import argparse
import sys
from pathlib import Path

from _common import (
    ITK_SPDX_COPYRIGHT as SPDX_COPYRIGHT,
)
from _common import (
    ITK_SPDX_LICENSE as SPDX_LICENSE,
)
from _common import (
    repo_root_from_script,
)

C_HEADER = (
    f"// SPDX-FileCopyrightText: {SPDX_COPYRIGHT}\n"
    f"// SPDX-License-Identifier: {SPDX_LICENSE}\n"
)

HASH_HEADER = (
    f"# SPDX-FileCopyrightText: {SPDX_COPYRIGHT}\n"
    f"# SPDX-License-Identifier: {SPDX_LICENSE}\n"
)

C_EXTENSIONS = {".h", ".hxx", ".cxx", ".txx"}
HASH_EXTENSIONS = {".py", ".cmake"}
# CMakeLists.txt handled separately by name

SKIP_PATTERNS = [
    "/ThirdParty/",
    "/.pixi/",
    "/cmake-build",
    "/build",
]


def should_skip(path: Path) -> bool:
    s = str(path)
    return any(pat in s for pat in SKIP_PATTERNS)


UTF8_BOM = "\ufeff"


def needs_spdx(content: str) -> bool:
    # Check only the first 50 lines so that scripts which legitimately
    # mention 'SPDX-License-Identifier' in their prose (e.g. the SPDX
    # tooling scripts themselves) are not falsely treated as already-SPDX.
    header_region = "\n".join(content.splitlines()[:50])
    return (
        "Copyright NumFOCUS" in content
        and "SPDX-License-Identifier" not in header_region
    )


def detect_line_ending(content: str) -> str:
    """Return the dominant line ending of the file: '\\r\\n' or '\\n'."""
    crlf = content.count("\r\n")
    lf = content.count("\n") - crlf
    return "\r\n" if crlf > lf else "\n"


def add_spdx_header(content: str, header: str) -> str:
    """Prepend the SPDX header while preserving BOM and line endings."""
    # Preserve an existing UTF-8 BOM by stripping, prepending SPDX, and
    # re-attaching the BOM at byte 0.
    bom = ""
    if content.startswith(UTF8_BOM):
        bom = UTF8_BOM
        content = content[len(UTF8_BOM) :]

    # Match the file's dominant line ending for the inserted SPDX lines.
    eol = detect_line_ending(content)
    if eol != "\n":
        header = header.replace("\n", eol)

    # Insert after a shebang line when present (Python scripts).
    if content.startswith("#!"):
        first_eol = content.find(eol)
        if first_eol < 0:
            # Shebang without a trailing newline: append one.
            return bom + content + eol + header
        first_eol += len(eol)
        return bom + content[:first_eol] + header + content[first_eol:]

    return bom + header + content


def process_file(path: Path, dry_run: bool) -> bool:
    """Add SPDX header to a single file. Returns True if modified."""
    # Read bytes and decode explicitly so we can detect BOM and CRLF
    # without Python's universal-newlines translation clobbering them.
    raw = path.read_bytes()
    try:
        content = raw.decode("utf-8")
    except UnicodeDecodeError:
        content = raw.decode("utf-8", errors="replace")

    if not needs_spdx(content):
        return False

    suffix = path.suffix
    name = path.name

    if suffix in C_EXTENSIONS:
        header = C_HEADER
    elif suffix in HASH_EXTENSIONS or name == "CMakeLists.txt":
        header = HASH_HEADER
    else:
        return False

    new_content = add_spdx_header(content, header)

    if dry_run:
        print(f"  would modify: {path}")
    else:
        # Write bytes to preserve the chosen EOL / BOM exactly.
        path.write_bytes(new_content.encode("utf-8"))

    return True


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("source_dir", nargs="?", default=None)
    parser.add_argument(
        "--dry-run", action="store_true", help="Show what would be changed"
    )
    parser.add_argument(
        "--check",
        action="store_true",
        help="Check-only mode: report missing headers and exit non-zero. "
        "Intended for use as a pre-commit hook. Implies --dry-run.",
    )
    parser.add_argument(
        "--files",
        nargs="+",
        default=None,
        help="Operate only on the listed files instead of walking a tree. "
        "When set, source_dir is ignored.",
    )
    args = parser.parse_args()

    dry_run = args.dry_run or args.check

    if args.files:
        # Pre-commit-style invocation: act on the explicit file list.
        modified = 0
        scanned = 0
        for file_arg in args.files:
            path = Path(file_arg)
            if should_skip(path) or not path.is_file():
                continue
            suffix = path.suffix
            name = path.name
            if (
                suffix not in C_EXTENSIONS
                and suffix not in HASH_EXTENSIONS
                and name != "CMakeLists.txt"
            ):
                continue
            scanned += 1
            if process_file(path, dry_run):
                modified += 1

        if args.check:
            if modified:
                print(
                    f"ERROR: {modified} staged file(s) are missing the SPDX "
                    f"header. Run:\n"
                    f"    python3 Utilities/SPDX/add_headers.py "
                    f"--files <paths>\n"
                    f"to add them, then stage the result and re-commit.",
                    file=sys.stderr,
                )
                return 1
            return 0

        action = "Would modify" if dry_run else "Modified"
        print(f"Scanned {scanned} files, {action} {modified}")
        return 0

    if args.source_dir:
        root = Path(args.source_dir)
    else:
        root = repo_root_from_script(__file__)

    if not (root / "CMakeLists.txt").exists():
        print(f"ERROR: {root} does not look like an ITK source tree", file=sys.stderr)
        return 1

    # Collect candidate files
    globs = [
        "**/*.h",
        "**/*.hxx",
        "**/*.cxx",
        "**/*.txx",
        "**/*.py",
        "**/*.cmake",
        "**/CMakeLists.txt",
    ]

    modified = 0
    scanned = 0

    for pattern in globs:
        for path in sorted(root.glob(pattern)):
            if should_skip(path):
                continue
            scanned += 1
            if process_file(path, dry_run):
                modified += 1

    action = "Would modify" if dry_run else "Modified"
    print(f"Scanned {scanned} files, {action} {modified}")
    if args.check and modified:
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
