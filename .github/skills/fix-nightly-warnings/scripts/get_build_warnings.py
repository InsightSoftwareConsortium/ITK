#!/usr/bin/env python3
# /// script
# requires-python = ">=3.8"
# ///
"""
get_build_warnings.py — Fetch and summarize warnings or errors for a CDash build.

Queries the CDash GraphQL API for all warning (or error) entries associated with
a specific build ID, then groups them by source file and warning flag.

Exit codes:
  0  Success
  1  Argument error
  2  Network or API error
"""

import argparse
import json
import re
import sys
import urllib.error
import urllib.request

CDASH_GRAPHQL = "https://open.cdash.org/graphql"

QUERY_TEMPLATE = """
{
  build(id: "%s") {
    name stamp startTime buildWarningsCount buildErrorsCount
    site { name }
    buildErrors(filters: { eq: { type: %s } }, first: %d%s) {
      pageInfo { hasNextPage endCursor }
      edges {
        node { sourceFile sourceLine stdError stdOutput }
      }
    }
  }
}
"""

WARNING_FLAG_RE = re.compile(r'\[(-W[^\]]+)\]')


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


def extract_flag(text: str) -> str:
    m = WARNING_FLAG_RE.search(text)
    return m.group(1) if m else "?"


def fetch_entries(build_id: str, error_type: str, limit: int) -> tuple[dict, list]:
    """Fetch all entries up to limit, following pagination if needed."""
    all_entries = []
    cursor = None
    build_meta = None

    while True:
        after_clause = f', after: "{cursor}"' if cursor else ""
        query = QUERY_TEMPLATE % (build_id, error_type, min(limit, 200), after_clause)
        data = graphql(query)

        if "errors" in data:
            print(f"Error: GraphQL returned errors: {data['errors']}", file=sys.stderr)
            sys.exit(2)

        build = data["data"]["build"]
        if build_meta is None:
            build_meta = {
                "name": build["name"],
                "stamp": build["stamp"],
                "site": build["site"]["name"],
                "buildWarningsCount": build["buildWarningsCount"],
                "buildErrorsCount": build["buildErrorsCount"],
            }

        page = build["buildErrors"]
        all_entries.extend(e["node"] for e in page["edges"])

        if not page["pageInfo"]["hasNextPage"] or len(all_entries) >= limit:
            break
        cursor = page["pageInfo"]["endCursor"]

    return build_meta, all_entries[:limit]


def main() -> None:
    parser = argparse.ArgumentParser(
        prog="get_build_warnings.py",
        description="Fetch and summarize warnings or errors for a CDash build.",
        epilog=(
            "Examples:\n"
            "  python3 scripts/get_build_warnings.py 11107692\n"
            "  python3 scripts/get_build_warnings.py 11107692 --raw\n"
            "  python3 scripts/get_build_warnings.py 11107692 --errors\n"
            "  python3 scripts/get_build_warnings.py 11107692 --json | jq '.entries[] | select(.flag == \"-Wthread-safety-negative\")'\n"
            "  python3 scripts/get_build_warnings.py 11107692 --limit 500"
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "build_id",
        metavar="BUILD_ID",
        help="CDash build ID (integer from list_nightly_warnings.py output or CDash URL)",
    )
    parser.add_argument(
        "--errors",
        action="store_true",
        help="Fetch build errors instead of warnings (default: warnings)",
    )
    parser.add_argument(
        "--raw",
        action="store_true",
        help="Print one entry per line with file:line, flag, and message snippet instead of grouping",
    )
    parser.add_argument(
        "--json",
        action="store_true",
        dest="json_output",
        help="Output as JSON: {build: {...}, entries: [...]} for programmatic use",
    )
    parser.add_argument(
        "--limit",
        type=int,
        default=200,
        metavar="N",
        help="Maximum number of entries to retrieve (default: 200)",
    )
    parser.add_argument(
        "--exclude-thirdparty",
        action="store_true",
        help="Exclude warnings from Modules/ThirdParty/ paths",
    )
    args = parser.parse_args()

    error_type = "ERROR" if args.errors else "WARNING"
    label = "error" if args.errors else "warning"

    build_meta, entries = fetch_entries(args.build_id, error_type, args.limit)
    if args.exclude_thirdparty:
        entries = [
            e for e in entries
            if not (e.get("sourceFile") or "").find("ThirdParty") >= 0
        ]
    total = build_meta["buildErrorsCount" if args.errors else "buildWarningsCount"] or 0

    if args.json_output:
        output = {
            "build": build_meta,
            "type": error_type,
            "totalCount": total,
            "fetchedCount": len(entries),
            "entries": [
                {
                    "sourceFile": n["sourceFile"],
                    "sourceLine": n["sourceLine"],
                    "flag": extract_flag(n["stdError"] or n["stdOutput"] or ""),
                    "stdError": n["stdError"],
                    "stdOutput": n["stdOutput"],
                }
                for n in entries
            ],
        }
        print(json.dumps(output, indent=2))
        return

    # Diagnostics to stderr so stdout stays parseable
    print(f"Build: {build_meta['name']}", file=sys.stderr)
    print(f"Site:  {build_meta['site']}", file=sys.stderr)
    print(f"Stamp: {build_meta['stamp']}", file=sys.stderr)
    print(f"Total {label}s: {total}  (fetched: {len(entries)})", file=sys.stderr)
    if total > len(entries):
        print(
            f"Note:  {total - len(entries)} entries not fetched; use --limit to increase.",
            file=sys.stderr,
        )
    print(file=sys.stderr)

    if args.raw:
        print(f"{'FILE:LINE':<60}  {'FLAG':<35}  SNIPPET")
        print("-" * 120)
        for n in entries:
            loc = f"{n['sourceFile']}:{n['sourceLine']}"
            text = n["stdError"] or n["stdOutput"] or ""
            flag = extract_flag(text)
            # Extract a short message snippet after the flag
            idx = text.find(flag)
            if idx >= 0:
                snippet = text[idx + len(flag):].strip().replace("\n", " ")[:80]
            else:
                snippet = text.replace("\n", " ")[:80]
            print(f"{loc:<60}  {flag:<35}  {snippet}")
    else:
        # Group by (sourceFile, flag)
        groups: dict = {}
        for n in entries:
            src = n["sourceFile"] or "<unknown>"
            text = n["stdError"] or n["stdOutput"] or ""
            flag = extract_flag(text)
            key = (src, flag)
            groups[key] = groups.get(key, 0) + 1

        print(f"{'COUNT':>6}  {'FLAG':<35}  SOURCE FILE")
        print("-" * 100)
        for (src, flag), count in sorted(groups.items(), key=lambda x: (-x[1], x[0][0])):
            print(f"{count:>6}  {flag:<35}  {src}")


if __name__ == "__main__":
    main()
