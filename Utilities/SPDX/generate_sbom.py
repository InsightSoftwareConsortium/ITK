# SPDX-FileCopyrightText: Copyright NumFOCUS
# SPDX-License-Identifier: Apache-2.0
"""Emit an SPDX 2.3 SBOM from a CMake-produced manifest.

ITKSBOMGeneration.cmake collects per-module SPDX metadata at configure
time and writes a simple line-based manifest (``sbom-inputs.manifest``).
This script reads that manifest, constructs a proper SPDX 2.3 JSON
document, and writes ``sbom.spdx.json``. All JSON escaping, relationship
wiring, creation-info assembly, and LicenseRef validation live here
rather than in hand-written CMake ``string(APPEND)`` code.

Manifest format (UTF-8, LF line endings):

    document.<field>=<value>
    package.begin
    <field>=<value>
    ...
    package.end
    extracted_license.begin
    <field>=<value>
    ...
    extracted_license.end

Blank lines and ``#``-prefixed lines are ignored. Values are unescaped:
``\\\\`` -> ``\\``, ``\\n`` -> newline, ``\\r`` -> CR.

Usage:
    python3 generate_sbom.py <manifest> <output-sbom.json>

Exit codes:
    0 -> SBOM written successfully
    1 -> Manifest parse error or invalid content
    2 -> Usage error

Requires Python 3.10 or later.
"""

from __future__ import annotations

import json
import re
import sys
from pathlib import Path

from _common import EXIT_FAIL, EXIT_OK, EXIT_USAGE, SPDX_DATA_LICENSE, SPDX_VERSION

# SPDX 5.x license-ref regex per the spec. Custom license identifiers
# declared in hasExtractedLicensingInfos must match this pattern.
_LICENSE_REF_RE = re.compile(r"^LicenseRef-[A-Za-z0-9.\-]+$")


def _unescape(value: str) -> str:
    """Reverse the CMake-side escape for newline / CR / backslash."""
    out: list[str] = []
    i = 0
    while i < len(value):
        c = value[i]
        if c == "\\" and i + 1 < len(value):
            nxt = value[i + 1]
            if nxt == "n":
                out.append("\n")
            elif nxt == "r":
                out.append("\r")
            elif nxt == "\\":
                out.append("\\")
            else:
                out.append(c)
                out.append(nxt)
            i += 2
            continue
        out.append(c)
        i += 1
    return "".join(out)


def parse_manifest(text: str) -> dict:
    """Parse a manifest string into ``{"document": {...}, "packages": [...],
    "extracted_licenses": [...]}``.
    """
    doc: dict[str, str] = {}
    packages: list[dict[str, str]] = []
    extracted: list[dict[str, str]] = []
    current: dict[str, str] | None = None
    mode: str | None = None

    for lineno, raw in enumerate(text.splitlines(), 1):
        line = raw.strip()
        if not line or line.startswith("#"):
            continue

        if line == "package.begin":
            current = {}
            mode = "package"
            continue
        if line == "package.end":
            if mode != "package" or current is None:
                raise ValueError(f"line {lineno}: unmatched package.end")
            packages.append(current)
            current = None
            mode = None
            continue
        if line == "extracted_license.begin":
            current = {}
            mode = "extracted"
            continue
        if line == "extracted_license.end":
            if mode != "extracted" or current is None:
                raise ValueError(f"line {lineno}: unmatched extracted_license.end")
            extracted.append(current)
            current = None
            mode = None
            continue

        if "=" not in line:
            raise ValueError(f"line {lineno}: expected 'key=value', got {line!r}")
        key, _, value = line.partition("=")
        value = _unescape(value)
        key = key.strip()

        if key.startswith("document."):
            doc[key[len("document.") :]] = value
        elif mode in ("package", "extracted") and current is not None:
            current[key] = value
        else:
            raise ValueError(f"line {lineno}: stray key outside block: {key!r}")

    if mode is not None:
        raise ValueError(f"manifest ends inside unterminated {mode} block")

    return {"document": doc, "packages": packages, "extracted_licenses": extracted}


def _pkg_to_json(pkg: dict[str, str]) -> dict:
    """Convert a parsed package record into its SPDX 2.3 JSON form."""
    out: dict = {
        "SPDXID": pkg["spdx_id"],
        "name": pkg["name"],
        "versionInfo": pkg.get("version") or "NOASSERTION",
        "downloadLocation": pkg.get("download_location") or "NOASSERTION",
        "supplier": pkg.get("supplier") or "NOASSERTION",
        "licenseConcluded": pkg.get("license_concluded") or "NOASSERTION",
        "licenseDeclared": pkg.get("license_declared") or "NOASSERTION",
        "copyrightText": pkg.get("copyright") or "NOASSERTION",
        "filesAnalyzed": False,
    }
    if pkg.get("originator"):
        out["originator"] = pkg["originator"]
    if pkg.get("description"):
        out["description"] = pkg["description"]
    if pkg.get("primary_package_purpose"):
        out["primaryPackagePurpose"] = pkg["primary_package_purpose"]
    if pkg.get("purl"):
        out["externalRefs"] = [
            {
                "referenceCategory": "PACKAGE-MANAGER",
                "referenceType": "purl",
                "referenceLocator": pkg["purl"],
            }
        ]
    return out


def build_sbom(parsed: dict) -> dict:
    """Assemble the final SPDX JSON document."""
    doc = parsed["document"]
    packages = parsed["packages"]
    extracted = parsed["extracted_licenses"]

    for lic in extracted:
        lid = lic.get("license_id", "")
        if not _LICENSE_REF_RE.match(lid):
            raise ValueError(
                f"invalid LicenseRef identifier {lid!r}: must match "
                f"'LicenseRef-[A-Za-z0-9.-]+'"
            )

    if not packages:
        raise ValueError("manifest contains no packages")

    sbom: dict = {
        "spdxVersion": SPDX_VERSION,
        "dataLicense": SPDX_DATA_LICENSE,
        "SPDXID": "SPDXRef-DOCUMENT",
        "name": doc.get("name", "ITK-SBOM"),
        "documentNamespace": doc["namespace"],
        "creationInfo": {
            "created": doc["timestamp"],
            "creators": [
                f"Tool: CMake-{doc.get('cmake_version', 'NOASSERTION')}",
                "Tool: ITKSBOMGeneration",
                "Organization: NumFOCUS",
            ],
            "licenseListVersion": doc.get("spdx_license_list_version", "NOASSERTION"),
        },
        "packages": [_pkg_to_json(p) for p in packages],
    }

    # First package is treated as the primary subject of the document.
    primary_id = packages[0]["spdx_id"]
    rels: list[dict] = [
        {
            "spdxElementId": "SPDXRef-DOCUMENT",
            "relationshipType": "DESCRIBES",
            "relatedSpdxElement": primary_id,
        }
    ]
    for pkg in packages[1:]:
        rels.append(
            {
                "spdxElementId": primary_id,
                "relationshipType": "DEPENDS_ON",
                "relatedSpdxElement": pkg["spdx_id"],
            }
        )
    sbom["relationships"] = rels

    if extracted:
        sbom["hasExtractedLicensingInfos"] = [
            {
                "licenseId": lic["license_id"],
                "name": lic.get("name", lic["license_id"]),
                "extractedText": lic.get("text", ""),
            }
            for lic in extracted
        ]

    return sbom


def main(argv: list[str]) -> int:
    if len(argv) != 3:
        print(
            "Usage: generate_sbom.py <manifest> <output-sbom.json>",
            file=sys.stderr,
        )
        return EXIT_USAGE

    manifest_path = Path(argv[1])
    output_path = Path(argv[2])

    try:
        text = manifest_path.read_text(encoding="utf-8")
    except OSError as exc:
        print(f"Cannot read manifest: {exc}", file=sys.stderr)
        return EXIT_FAIL

    try:
        parsed = parse_manifest(text)
        sbom = build_sbom(parsed)
    except (KeyError, ValueError) as exc:
        print(f"Manifest error: {exc}", file=sys.stderr)
        return EXIT_FAIL

    output_path.write_text(
        json.dumps(sbom, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
    print(f"SBOM written: {output_path}")
    return EXIT_OK


if __name__ == "__main__":
    sys.exit(main(sys.argv))
