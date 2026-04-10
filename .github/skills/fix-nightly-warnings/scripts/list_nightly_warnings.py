#!/usr/bin/env python3
# /// script
# requires-python = ">=3.8"
# ///
"""
list_nightly_warnings.py — List ITK CDash builds that have warnings or errors.

Queries the CDash GraphQL API for recent builds of a given type in the Insight/ITK project
and prints those with a non-zero warning or error count. By default, selects Nightly builds
submitted since the project's nightly start time (CTEST_NIGHTLY_START). When `--since` is
omitted the script uses a hard-coded ISO time `01:00:00+00:00` (matches CTestConfig.cmake),
otherwise `--since N` selects builds from the last N hours.

Build types visible on the CDash dashboard (https://open.cdash.org/index.php?project=Insight):
  Nightly        — scheduled nightly builds (default)
  Continuous     — CI builds triggered on commits
  Experimental   — developer-submitted one-off builds

Exit codes:
  0  Success (results printed, possibly empty)
  1  Argument error
  2  Network or API error
"""

import argparse
import json
import sys
import urllib.error
import urllib.request
from datetime import datetime, timedelta, timezone

CDASH_GRAPHQL = "https://open.cdash.org/graphql"
INSIGHT_PROJECT_ID = "2"

QUERY_TEMPLATE = """
{
  project(id: "%s") {
    name
    builds(first: %d, filters: { all: [
      { contains: { stamp: "%s" } }
      { gt: { startTime: "%s" } }
    ]}) {
      edges {
        node {
          id name stamp buildType buildWarningsCount buildErrorsCount startTime
          site { name }
        }
      }
    }
  }
}
"""


def graphql(query: str) -> dict:
    payload = json.dumps({"query": query}).encode()
    req = urllib.request.Request(
        CDASH_GRAPHQL,
        data=payload,
        headers={"Content-Type": "application/json"},
    )
    try:
        with urllib.request.urlopen(req, timeout=30) as r:
            return json.loads(r.read())
    except urllib.error.URLError as e:
        print(f"Error: network request failed: {e}", file=sys.stderr)
        sys.exit(2)


def main() -> None:
    parser = argparse.ArgumentParser(
        prog="list_nightly_warnings.py",
        description="List ITK CDash builds that have warnings or errors.",
        epilog=(
            "Examples:\n"
            "  python3 scripts/list_nightly_warnings.py\n"
            "  python3 scripts/list_nightly_warnings.py --type Continuous\n"
            "  python3 scripts/list_nightly_warnings.py --type Experimental --limit 100 --all\n"
            "  python3 scripts/list_nightly_warnings.py --json | jq '.[] | select(.warnings > 10)'"
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "--type",
        default="Nightly",
        metavar="TYPE",
        help=(
            "Build type to filter by: Nightly (default), Continuous, Experimental, etc. "
            "Matched as a substring of the build stamp (e.g. '20260310-0100-Nightly')."
        ),
    )
    parser.add_argument(
        "--since",
        type=float,
        default=None,
        metavar="HOURS",
        help=(
            "Only show builds submitted in the last N hours. If omitted, uses the "
            "project nightly start time (hard-coded to 01:00:00+00:00 in this script)."
        ),
    )
    parser.add_argument(
        "--limit",
        type=int,
        default=100,
        metavar="N",
        help="Maximum number of builds to fetch from CDash (default: 100)",
    )
    parser.add_argument(
        "--all",
        action="store_true",
        help="Show all matching builds, not just those with warnings or errors",
    )
    parser.add_argument(
        "--new-warnings-only",
        action="store_true",
        help=(
            "Only show builds that have MORE warnings than the lowest warning "
            "count among recent builds (i.e., regressions from a clean baseline)"
        ),
    )
    parser.add_argument(
        "--json",
        action="store_true",
        dest="json_output",
        help="Output as a JSON array instead of a human-readable table",
    )
    args = parser.parse_args()

    if args.since is None:
        # Hard-coded nightly start time (matches CTestConfig.cmake project-level
        # setting `CTEST_NIGHTLY_START_TIME` in the repository root). Update here
        # if that value changes. Use an ISO-like time with UTC offset to simplify
        # parsing (no manual tz mapping required): `HH:MM:SS+00:00`.
        CTEST_NIGHTLY_START = "01:00:00+00:00"
        # Build an aware datetime from today's date + the hard-coded time offset.
        today = datetime.now(timezone.utc).date()
        dt = datetime.fromisoformat(f"{today.isoformat()}T{CTEST_NIGHTLY_START}")
        now_utc = datetime.now(timezone.utc)
        since_dt = dt if now_utc >= dt else dt - timedelta(days=1)
    else:
        since_dt = datetime.now(timezone.utc) - timedelta(hours=args.since)

    since_str = since_dt.strftime("%Y-%m-%dT%H:%M:%S+00:00")

    # Request more builds from the server than we may display so we can
    # sort by errors/warnings client-side and then apply the display `--limit`.
    fetch_count = max(args.limit, 500)
    query = QUERY_TEMPLATE % (INSIGHT_PROJECT_ID, fetch_count, args.type, since_str)
    data = graphql(query)

    if "errors" in data:
        print(f"Error: GraphQL returned errors: {data['errors']}", file=sys.stderr)
        sys.exit(2)

    edges = data["data"]["project"]["builds"]["edges"]
    project_name = data["data"]["project"]["name"]

    nodes = [e["node"] for e in edges]
    if not args.all:
        nodes = [
            n
            for n in nodes
            if (n.get("buildWarningsCount") or 0) > 0
            or (n.get("buildErrorsCount") or 0) > 0
        ]

    # Sort by number of errors (desc), then warnings (desc), then startTime (desc).
    nodes.sort(
        key=lambda n: (
            n.get("buildErrorsCount") or 0,
            n.get("buildWarningsCount") or 0,
            n.get("startTime", ""),
        ),
        reverse=True,
    )

    if args.new_warnings_only and nodes:
        # Find the minimum warning count as the "clean baseline" and keep
        # only builds that exceed it, indicating regressions.
        min_warnings = min(n.get("buildWarningsCount") or 0 for n in nodes)
        nodes = [
            n
            for n in nodes
            if (n.get("buildWarningsCount") or 0) > min_warnings
            or (n.get("buildErrorsCount") or 0) > 0
        ]

    # Apply the display limit after sorting.
    # Use displayed_nodes to distinguish the limited subset from the
    # full filtered list, facilitating debugging.
    displayed_nodes = nodes[: args.limit]

    if args.json_output:
        output = [
            {
                "id": n["id"],
                "name": n["name"],
                "stamp": n["stamp"],
                "buildType": n["buildType"],
                "warnings": n["buildWarningsCount"] or 0,
                "errors": n["buildErrorsCount"] or 0,
                "startTime": n["startTime"],
                "site": n["site"]["name"],
            }
            for n in displayed_nodes
        ]
        print(json.dumps(output, indent=2))
        return

    print(
        f"Project: {project_name}  |  type={args.type!r}  |  since={since_str}  |  "
        f"Builds fetched: {len(edges)}  |  With warnings/errors: {len(displayed_nodes)}",
        file=sys.stderr,
    )
    print(file=sys.stderr)
    print(
        f"{'BUILD_ID':>10}  {'W':>4}  {'E':>4}  {'STAMP':<25}  {'SITE':<25}  BUILD_NAME"
    )
    print("-" * 110)
    for n in displayed_nodes:
        warn = n["buildWarningsCount"] or 0
        err = n["buildErrorsCount"] or 0
        print(
            f"{n['id']:>10}  {warn:>4}  {err:>4}  "
            f"{n['stamp'][:24]:<25}  {n['site']['name'][:24]:<25}  {n['name']}"
        )


if __name__ == "__main__":
    main()
