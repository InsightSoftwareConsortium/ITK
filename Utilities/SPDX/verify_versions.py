#!/usr/bin/env python3
# SPDX-FileCopyrightText: Copyright NumFOCUS
# SPDX-License-Identifier: Apache-2.0
"""Verify SPDX_VERSION in itk-module.cmake matches UpdateFromUpstream.sh tags.

For each ThirdParty module that has both an UpdateFromUpstream.sh with a
parseable version tag and an SPDX_VERSION in itk-module.cmake, verify
they are consistent.

Modules tracking 'master', commit SHAs, or custom ITK tags are skipped
since their version cannot be derived from the tag alone.

Exit code 0 if all checked modules match, 1 if any mismatch is found.

Requires Python 3.10 or later (uses PEP 604 union syntax).
"""

import re
import sys
from pathlib import Path

from _common import EXIT_FAIL, EXIT_OK, repo_root_from_script


def extract_tag_from_upstream_script(script_path: Path) -> str | None:
    """Extract the 'tag' value from UpdateFromUpstream.sh."""
    text = script_path.read_text()
    # Match: readonly tag="..." or tag="..." or readonly tag='...'
    m = re.search(r"""(?:readonly\s+)?tag\s*=\s*['"]([^'"]+)['"]""", text)
    return m.group(1) if m else None


def normalize_version_from_tag(tag: str) -> str | None:
    """Attempt to extract a semver-like version from a git tag.

    Returns None if the tag is a SHA, 'master', or an unrecognizable format.
    """
    # Skip SHAs (40-hex or short)
    if re.fullmatch(r"[0-9a-f]{7,40}", tag):
        return None
    # Skip 'master' or 'main'
    if tag in ("master", "main"):
        return None
    # Skip custom ITK tags like 'for/itk-20260305-4c99fca'
    if tag.startswith("for/"):
        return None

    # Try common patterns:
    # v1.2.3 or V1.2.3
    m = re.fullmatch(r"[vV]?(\d+\.\d+(?:\.\d+)?)", tag)
    if m:
        return m.group(1)

    # hdf5_1.14.5
    m = re.fullmatch(r"[a-zA-Z0-9]+[_-](\d+\.\d+(?:\.\d+)?)", tag)
    if m:
        return m.group(1)

    # R_2_7_4 (Expat style)
    m = re.fullmatch(r"R_(\d+)_(\d+)_(\d+)", tag)
    if m:
        return f"{m.group(1)}.{m.group(2)}.{m.group(3)}"

    # Bare version like 2.2.5 or 3.0.4
    m = re.fullmatch(r"(\d+\.\d+(?:\.\d+)?)", tag)
    if m:
        return m.group(1)

    return None


def extract_spdx_version(module_cmake: Path) -> str | None:
    """Extract SPDX_VERSION value from itk-module.cmake."""
    text = module_cmake.read_text()
    m = re.search(r'SPDX_VERSION\s+"([^"]+)"', text)
    return m.group(1) if m else None


def main() -> int:
    if len(sys.argv) > 1:
        itk_source = Path(sys.argv[1])
    else:
        itk_source = repo_root_from_script(__file__)

    thirdparty_dir = itk_source / "Modules" / "ThirdParty"
    if not thirdparty_dir.is_dir():
        print(f"ERROR: {thirdparty_dir} not found", file=sys.stderr)
        return EXIT_FAIL

    errors = []
    checked = 0
    skipped = 0

    for module_dir in sorted(thirdparty_dir.iterdir()):
        if not module_dir.is_dir():
            continue

        upstream_script = module_dir / "UpdateFromUpstream.sh"
        module_cmake = module_dir / "itk-module.cmake"

        if not upstream_script.exists() or not module_cmake.exists():
            continue

        tag = extract_tag_from_upstream_script(upstream_script)
        if tag is None:
            skipped += 1
            continue

        expected_version = normalize_version_from_tag(tag)
        if expected_version is None:
            skipped += 1
            continue

        declared_version = extract_spdx_version(module_cmake)
        if declared_version is None:
            errors.append(
                f"{module_dir.name}: UpdateFromUpstream.sh tag='{tag}' "
                f"implies version {expected_version}, but no SPDX_VERSION "
                f"declared in itk-module.cmake"
            )
            checked += 1
            continue

        if declared_version != expected_version:
            errors.append(
                f"{module_dir.name}: SPDX_VERSION='{declared_version}' "
                f"does not match UpdateFromUpstream.sh tag='{tag}' "
                f"(expected '{expected_version}')"
            )

        checked += 1

    print(f"Checked {checked} modules, skipped {skipped} " f"(master/SHA/custom tags)")

    if errors:
        print(f"\n{len(errors)} version mismatch(es):", file=sys.stderr)
        for e in errors:
            print(f"  FAIL: {e}", file=sys.stderr)
        return EXIT_FAIL

    print("All SPDX_VERSION entries match UpdateFromUpstream.sh tags.")
    return EXIT_OK


if __name__ == "__main__":
    sys.exit(main())
