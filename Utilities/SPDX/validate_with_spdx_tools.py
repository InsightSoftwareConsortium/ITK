# SPDX-FileCopyrightText: Copyright NumFOCUS
# SPDX-License-Identifier: Apache-2.0
"""Validate a generated SPDX SBOM against the full SPDX 2.3 schema.

Uses the official `spdx-tools` Python package (if available) to run the
full reference validator, which checks field names, value formats,
license-expression syntax, relationship integrity, and document-level
consistency rules that the lightweight in-tree JSON validator cannot.

If `spdx-tools` is not installed, the script exits 77 (CTest's
skip-test code) so the test is reported as skipped rather than failed
on build hosts that lack the optional dependency.

Usage:
    python3 validate_with_spdx_tools.py <path-to-sbom.spdx.json>

Exit codes:
    0  -> SBOM is valid
    1  -> SBOM has validation errors (details on stderr)
    77 -> spdx-tools not installed; test should be skipped

Requires Python 3.10 or later.
"""

import sys
from pathlib import Path

from _common import EXIT_FAIL, EXIT_OK, EXIT_SKIP, EXIT_USAGE


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print(
            "Usage: validate_with_spdx_tools.py <sbom.spdx.json>",
            file=sys.stderr,
        )
        return EXIT_USAGE

    sbom_path = Path(argv[1])
    if not sbom_path.is_file():
        print(f"SBOM file not found: {sbom_path}", file=sys.stderr)
        return EXIT_FAIL

    try:
        from spdx_tools.spdx.parser.parse_anything import parse_file
        from spdx_tools.spdx.validation.document_validator import (
            validate_full_spdx_document,
        )
    except ImportError:
        print(
            "spdx-tools is not installed; skipping full schema validation.\n"
            "    To enable this test, install the package:\n"
            "        pip install spdx-tools\n"
            "    The lightweight in-tree ITKSBOMValidation test still runs.",
            file=sys.stderr,
        )
        return EXIT_SKIP

    try:
        doc = parse_file(str(sbom_path))
    except Exception as exc:
        print(f"SBOM parse failed: {exc}", file=sys.stderr)
        return EXIT_FAIL

    messages = validate_full_spdx_document(doc)
    if messages:
        for msg in messages:
            print(f"SBOM validation error: {msg.validation_message}", file=sys.stderr)
            if msg.context:
                print(f"  Context: {msg.context}", file=sys.stderr)
        return EXIT_FAIL

    pkg_count = len(doc.packages) if doc.packages else 0
    rel_count = len(doc.relationships) if doc.relationships else 0
    print(
        f"spdx-tools validation passed: "
        f"{pkg_count} packages, {rel_count} relationships."
    )
    return EXIT_OK


if __name__ == "__main__":
    sys.exit(main(sys.argv))
