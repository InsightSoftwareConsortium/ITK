#!/usr/bin/env python3
"""Set ITK_VERSION_TWEAK in CMake/itkVersion.cmake to a YYYYMMDD date.

Usage:
  BumpVersionTweak.py            # bump to today's UTC date
  BumpVersionTweak.py 20260801   # bump to an explicit date
"""

import argparse
import datetime
import re
import sys
from pathlib import Path

VERSION_FILE = Path(__file__).resolve().parents[2] / "CMake" / "itkVersion.cmake"
PATTERN = re.compile(r'(set\(ITK_VERSION_TWEAK ")(\d{8})("\))')


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "date",
        nargs="?",
        default=datetime.datetime.now(datetime.UTC).strftime("%Y%m%d"),
        help="YYYYMMDD value (default: today, UTC)",
    )
    args = parser.parse_args()

    try:
        datetime.datetime.strptime(args.date, "%Y%m%d")
    except ValueError:
        parser.error(f"'{args.date}' is not a valid YYYYMMDD date")

    content = VERSION_FILE.read_text()
    match = PATTERN.search(content)
    if match is None:
        print(f"ERROR: ITK_VERSION_TWEAK not found in {VERSION_FILE}", file=sys.stderr)
        return 1
    if args.date < match.group(2):
        print(
            f"ERROR: {args.date} is older than current tweak {match.group(2)}",
            file=sys.stderr,
        )
        return 1
    if args.date == match.group(2):
        print(f"ITK_VERSION_TWEAK already {args.date}")
        return 0

    VERSION_FILE.write_text(PATTERN.sub(rf"\g<1>{args.date}\g<3>", content))
    print(f"ITK_VERSION_TWEAK: {match.group(2)} -> {args.date}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
