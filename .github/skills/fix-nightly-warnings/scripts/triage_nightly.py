#!/usr/bin/env python3
# /// script
# requires-python = ">=3.8"
# ///
"""
triage_nightly.py — Single-command CDash nightly warning triage.

Chains list_nightly_warnings and get_build_warnings to produce a
deduplicated, actionable summary of warnings grouped by flag and
source file. Designed for agentic automation: one tool call instead
of multiple script invocations.

Exit codes:
  0  Success (results printed, possibly empty)
  2  Network or API error
"""

import argparse
import json
import sys
from pathlib import Path

# Import sibling modules
sys.path.insert(0, str(Path(__file__).parent))
from get_build_warnings import extract_flag, fetch_entries
from list_nightly_warnings import graphql, INSIGHT_PROJECT_ID, QUERY_TEMPLATE

from datetime import datetime, timedelta, timezone


def main() -> None:
    parser = argparse.ArgumentParser(
        prog="triage_nightly.py",
        description="Triage CDash nightly warnings into an actionable summary.",
        epilog=(
            "Examples:\n"
            "  python3 scripts/triage_nightly.py\n"
            "  python3 scripts/triage_nightly.py --since 48\n"
            "  python3 scripts/triage_nightly.py --json"
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "--type",
        default="Nightly",
        metavar="TYPE",
        help="Build type to filter (default: Nightly)",
    )
    parser.add_argument(
        "--since",
        type=float,
        default=None,
        metavar="HOURS",
        help="Only include builds from the last N hours (default: since nightly start)",
    )
    parser.add_argument(
        "--limit-builds",
        type=int,
        default=25,
        metavar="N",
        help="Max builds to inspect (default: 25)",
    )
    parser.add_argument(
        "--limit-warnings",
        type=int,
        default=200,
        metavar="N",
        help="Max warnings to fetch per build (default: 200)",
    )
    parser.add_argument(
        "--json",
        action="store_true",
        dest="json_output",
        help="Output as JSON for programmatic use",
    )
    args = parser.parse_args()

    # Determine time window
    if args.since is None:
        CTEST_NIGHTLY_START = "01:00:00+00:00"
        today = datetime.now(timezone.utc).date()
        dt = datetime.fromisoformat(f"{today.isoformat()}T{CTEST_NIGHTLY_START}")
        now_utc = datetime.now(timezone.utc)
        since_dt = dt if now_utc >= dt else dt - timedelta(days=1)
    else:
        since_dt = datetime.now(timezone.utc) - timedelta(hours=args.since)

    since_str = since_dt.strftime("%Y-%m-%dT%H:%M:%S+00:00")

    # Step 1: List builds with warnings
    query = QUERY_TEMPLATE % (INSIGHT_PROJECT_ID, 500, args.type, since_str)
    data = graphql(query)

    if "errors" in data:
        print(f"Error: GraphQL returned errors: {data['errors']}", file=sys.stderr)
        sys.exit(2)

    edges = data["data"]["project"]["builds"]["edges"]
    builds_with_warnings = [
        e["node"]
        for e in edges
        if (e["node"].get("buildWarningsCount") or 0) > 0
        or (e["node"].get("buildErrorsCount") or 0) > 0
    ]
    builds_with_warnings.sort(
        key=lambda n: (
            n.get("buildErrorsCount") or 0,
            n.get("buildWarningsCount") or 0,
        ),
        reverse=True,
    )
    builds_with_warnings = builds_with_warnings[: args.limit_builds]

    if not builds_with_warnings:
        if args.json_output:
            print(json.dumps({"builds_inspected": 0, "warnings": []}, indent=2))
        else:
            print("No builds with warnings found.", file=sys.stderr)
        return

    # Step 2: Fetch warnings from each build and deduplicate
    seen = {}  # (sourceFile, flag) -> {count, builds, sample_message}
    builds_inspected = []

    for build_node in builds_with_warnings:
        build_id = build_node["id"]
        build_name = build_node["name"]
        site = build_node["site"]["name"]

        builds_inspected.append({"id": build_id, "name": build_name, "site": site})

        try:
            _meta, entries = fetch_entries(
                str(build_id), "WARNING", args.limit_warnings
            )
        except SystemExit:
            continue

        for e in entries:
            src = e.get("sourceFile") or "<unknown>"
            if "ThirdParty" in src:
                continue
            text = e.get("stdError") or e.get("stdOutput") or ""
            flag = extract_flag(text)
            key = (src, flag)
            if key not in seen:
                seen[key] = {
                    "sourceFile": src,
                    "flag": flag,
                    "count": 0,
                    "builds": [],
                    "sample": text[:200],
                }
            seen[key]["count"] += 1
            if build_name not in seen[key]["builds"]:
                seen[key]["builds"].append(build_name)

    # Step 3: Group by flag, sort by count
    by_flag = {}
    for info in seen.values():
        flag = info["flag"]
        if flag not in by_flag:
            by_flag[flag] = {"flag": flag, "total_count": 0, "files": []}
        by_flag[flag]["total_count"] += info["count"]
        by_flag[flag]["files"].append(info)

    result = sorted(by_flag.values(), key=lambda g: -g["total_count"])

    if args.json_output:
        print(
            json.dumps(
                {
                    "builds_inspected": len(builds_inspected),
                    "builds": builds_inspected,
                    "warnings_by_flag": result,
                },
                indent=2,
            )
        )
    else:
        print(
            f"Inspected {len(builds_inspected)} builds  |  "
            f"{len(seen)} unique (file, flag) pairs  |  "
            f"{len(by_flag)} distinct flags",
            file=sys.stderr,
        )
        print(file=sys.stderr)
        print(f"{'TOTAL':>6}  {'FILES':>6}  FLAG")
        print("-" * 60)
        for group in result:
            print(
                f"{group['total_count']:>6}  {len(group['files']):>6}  {group['flag']}"
            )


if __name__ == "__main__":
    main()
