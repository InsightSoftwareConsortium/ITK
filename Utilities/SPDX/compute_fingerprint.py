# SPDX-FileCopyrightText: Copyright NumFOCUS
# SPDX-License-Identifier: Apache-2.0
"""Compute a stable fingerprint of an ITK SBOM for drift detection.

The fingerprint is a SHA-256 hash of the sorted, canonicalized list of
package name + version + license + PURL tuples. Fields that change on
every build (timestamps, document namespace UUIDs) are deliberately
excluded so that the fingerprint is reproducible for a given set of
enabled modules and third-party versions.

Usage:
    # Print fingerprint to stdout:
    python3 compute_fingerprint.py <sbom.spdx.json>

    # Compare against a baseline file and fail if they differ:
    python3 compute_fingerprint.py <sbom.spdx.json> --compare <baseline>

Exit codes:
    0  -> fingerprint matches baseline (or no --compare given)
    1  -> fingerprint differs or input missing

Requires Python 3.10 or later.
"""

import argparse
import hashlib
import sys
from pathlib import Path

from _common import EXIT_FAIL, EXIT_OK, load_sbom


def compute_fingerprint(sbom_path: Path) -> str:
    doc = load_sbom(sbom_path)

    entries: list[str] = []
    for pkg in doc.get("packages", []):
        name = pkg.get("name", "")
        version = pkg.get("versionInfo", "")
        license_ = pkg.get("licenseDeclared", "")
        purl = ""
        for ref in pkg.get("externalRefs", []) or []:
            if ref.get("referenceType") == "purl":
                purl = ref.get("referenceLocator", "")
                break
        entries.append(f"{name}|{version}|{license_}|{purl}")

    entries.sort()
    canonical = "\n".join(entries).encode("utf-8")
    return hashlib.sha256(canonical).hexdigest()


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("sbom", type=Path, help="Path to sbom.spdx.json")
    parser.add_argument(
        "--compare",
        type=Path,
        default=None,
        help="Baseline fingerprint file to compare against",
    )
    parser.add_argument(
        "--update",
        action="store_true",
        help="Write the computed fingerprint to --compare path instead " "of comparing",
    )
    args = parser.parse_args(argv[1:])

    if not args.sbom.is_file():
        print(f"SBOM file not found: {args.sbom}", file=sys.stderr)
        return EXIT_FAIL

    fingerprint = compute_fingerprint(args.sbom)

    if args.update:
        if args.compare is None:
            print("--update requires --compare <baseline>", file=sys.stderr)
            return EXIT_FAIL
        args.compare.write_text(fingerprint + "\n")
        print(f"Wrote fingerprint {fingerprint} to {args.compare}")
        return EXIT_OK

    if args.compare is None:
        print(fingerprint)
        return EXIT_OK

    if not args.compare.is_file():
        print(
            f"Baseline fingerprint file not found: {args.compare}\n"
            f"To create it, run:\n"
            f"    python3 {sys.argv[0]} {args.sbom} "
            f"--compare {args.compare} --update",
            file=sys.stderr,
        )
        return EXIT_FAIL

    baseline = args.compare.read_text().strip()
    if baseline != fingerprint:
        print(
            "SBOM FINGERPRINT DRIFT DETECTED:\n"
            f"  baseline: {baseline}\n"
            f"  current:  {fingerprint}\n"
            "\n"
            "The set of packages (name, version, license, or PURL) emitted\n"
            "into the SBOM has changed relative to the committed baseline.\n"
            "If the change is intentional (dependency bump, new ThirdParty\n"
            "module), regenerate the baseline:\n"
            f"    python3 {sys.argv[0]} {args.sbom} "
            f"--compare {args.compare} --update\n"
            "and commit the updated baseline in the same PR.",
            file=sys.stderr,
        )
        return EXIT_FAIL

    print(f"SBOM fingerprint matches baseline: {fingerprint}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
