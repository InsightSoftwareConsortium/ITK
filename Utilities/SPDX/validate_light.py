#!/usr/bin/env python3
# SPDX-FileCopyrightText: Copyright NumFOCUS
# SPDX-License-Identifier: Apache-2.0
"""Lightweight in-tree validator for the ITK-generated SPDX SBOM.

Checks a small set of must-hold invariants that do not require the
third-party spdx-tools package:

    1. Valid JSON syntax
    2. Required SPDX 2.3 top-level fields are present
    3. spdxVersion is SPDX-2.3 and dataLicense is CC0-1.0
    4. At least the ITK package exists
    5. DESCRIBES relationship is present
    6. Package SPDXIDs are unique
    7. Each hasExtractedLicensingInfos entry has licenseId and extractedText

The companion ITKSBOMSchemaValidation CTest runs the spdx-tools
reference validator for the full SPDX 2.3 schema; this lightweight
check runs everywhere without optional dependencies and surfaces
obvious regressions in the hand-written CMake JSON emitter.

Usage:
    python3 validate_light.py <path-to-sbom.spdx.json>

Exit codes:
    0 -> SBOM passes the lightweight checks
    1 -> SBOM has one or more errors (details on stderr)

Requires Python 3.10 or later.
"""

import json
import sys
from pathlib import Path

from _common import (
    EXIT_FAIL,
    EXIT_OK,
    EXIT_USAGE,
    SPDX_DATA_LICENSE,
    SPDX_VERSION,
    load_sbom,
)


def validate(sbom_path: Path) -> list[str]:
    errors: list[str] = []

    try:
        doc = load_sbom(sbom_path)
    except (OSError, json.JSONDecodeError) as exc:
        return [f"Cannot parse SBOM JSON: {exc}"]

    required = [
        "spdxVersion",
        "dataLicense",
        "SPDXID",
        "name",
        "documentNamespace",
        "creationInfo",
        "packages",
        "relationships",
    ]
    for field in required:
        if field not in doc:
            errors.append(f"Missing required field: {field}")

    if doc.get("spdxVersion") != SPDX_VERSION:
        errors.append(
            f"Expected spdxVersion {SPDX_VERSION}, got {doc.get('spdxVersion')}"
        )

    if doc.get("dataLicense") != SPDX_DATA_LICENSE:
        errors.append(
            f"Expected dataLicense {SPDX_DATA_LICENSE}, got {doc.get('dataLicense')}"
        )

    packages = doc.get("packages", [])
    if len(packages) < 1:
        errors.append("No packages found")

    if not any(p.get("name") == "ITK" for p in packages):
        errors.append("ITK package not found")

    rels = doc.get("relationships", [])
    if not any(r.get("relationshipType") == "DESCRIBES" for r in rels):
        errors.append("No DESCRIBES relationship found")

    spdx_ids = [p.get("SPDXID") for p in packages]
    dupes = {x for x in spdx_ids if spdx_ids.count(x) > 1}
    if dupes:
        errors.append(f"Duplicate SPDX IDs: {dupes}")

    # Note: plural per SPDX 2.3 schema
    extracted = doc.get("hasExtractedLicensingInfos", [])
    for entry in extracted:
        if "licenseId" not in entry:
            errors.append("Extracted license missing licenseId")
        if "extractedText" not in entry:
            errors.append("Extracted license missing extractedText")

    if not errors:
        print(
            f"SBOM valid: {len(packages)} packages, {len(rels)} relationships, "
            f"{len(extracted)} extracted licenses"
        )

    return errors


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print("Usage: validate_light.py <sbom.spdx.json>", file=sys.stderr)
        return EXIT_USAGE

    sbom_path = Path(argv[1])
    errors = validate(sbom_path)
    if errors:
        for e in errors:
            print(f"SBOM ERROR: {e}", file=sys.stderr)
        return EXIT_FAIL
    return EXIT_OK


if __name__ == "__main__":
    sys.exit(main(sys.argv))
