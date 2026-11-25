#!/usr/bin/env python3
"""Extract and print build warnings/errors from CTest Build.xml.

ctest_build() writes compiler diagnostics to Build.xml for CDash but
does not print them to stdout.  This script reads Build.xml from the
most recent test run and prints the <Text> content of every <Warning>
and <Error> element so diagnostics appear directly in CI logs.

Usage:
    python report_build_diagnostics.py <build-directory>
"""

import os
import sys
import xml.etree.ElementTree as ET


def main():
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <build-directory>", file=sys.stderr)
        sys.exit(1)

    build_dir = sys.argv[1]

    # CTest writes a TAG file whose first line is the timestamp directory.
    tag_file = os.path.join(build_dir, "Testing", "TAG")
    if not os.path.isfile(tag_file):
        print(f"No TAG file found at {tag_file} — skipping.", file=sys.stderr)
        return

    with open(tag_file) as f:
        tag_dir = f.readline().strip()

    build_xml = os.path.join(build_dir, "Testing", tag_dir, "Build.xml")
    if not os.path.isfile(build_xml):
        print(f"No Build.xml found at {build_xml} — skipping.", file=sys.stderr)
        return

    tree = ET.parse(build_xml)
    root = tree.getroot()

    # Find all <Warning> and <Error> elements under <Build>.
    warnings = []
    errors = []
    for build_elem in root.iter("Build"):
        for warning in build_elem.findall("Warning"):
            text = warning.findtext("Text", "").strip()
            src = warning.findtext("SourceFile", "").strip()
            line = warning.findtext("SourceLineNumber", "").strip()
            if text:
                warnings.append((src, line, text))
        for error in build_elem.findall("Error"):
            text = error.findtext("Text", "").strip()
            src = error.findtext("SourceFile", "").strip()
            line = error.findtext("SourceLineNumber", "").strip()
            if text:
                errors.append((src, line, text))

    if not warnings and not errors:
        print("No build warnings or errors found.")
        return

    if errors:
        print(f"========== BUILD ERRORS ({len(errors)}) ==========")
        for src, line, text in errors:
            print(f"  {text}")
        print()

    if warnings:
        print(f"========== BUILD WARNINGS ({len(warnings)}) ==========")
        for src, line, text in warnings:
            print(f"  {text}")
        print()

    print("====================================================")

    # Exit with non-zero status only if there are errors (warnings are
    # informational).  This lets CI pipelines treat the step as a
    # soft-fail for warnings but hard-fail for errors if desired.


if __name__ == "__main__":
    main()
