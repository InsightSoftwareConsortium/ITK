#!/usr/bin/env python3
"""Prune superseded GitHub Actions caches for the ITK repository.

ITK's CI workflows seed ccache via cache keys of the form
``ccache-v4-<OS>-<variant>-<sha>``. Each new push produces a new cache
with a fresh SHA suffix, while ``actions/cache`` ``restore-keys`` falls
back to the most recent prefix match. Older SHA-suffixed entries are
therefore dead weight: they consume the per-repository 10 GB cache
budget but never get restored once a newer entry exists.

For each ``(ref, key-prefix-stripped-of-trailing-SHA)`` tuple this
script keeps the newest cache and deletes the rest. It is intended to
run as a non-blocking housekeeping job at the end of each CI workflow.
It deliberately exits 0 even on partial failure so that a missing
``actions: write`` permission (e.g. fork-PR runs) cannot turn a green
build red.

Required environment when run from CI:
- ``GITHUB_TOKEN``      — token with ``actions: write`` scope
- ``GITHUB_REPOSITORY`` — ``owner/repo`` of the target repository

Usage::

    python3 PruneSupersededCiCaches.py [--dry-run] [--repo OWNER/REPO]
                                       [--token TOKEN] [--keep N]
"""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import subprocess
import sys
from collections import defaultdict
from typing import Any

SHA_RE = re.compile(r"^(?P<prefix>.+)-(?P<sha>[0-9a-f]{40})$")
REPO_RE = re.compile(r"^[A-Za-z0-9._-]+/[A-Za-z0-9._-]+$")


def _gh(args: list[str], token: str) -> tuple[int, str, str]:
    """Run ``gh`` with the supplied args. Token is passed via env, never argv."""
    gh = shutil.which("gh")
    if gh is None:
        return 127, "", "gh CLI not found on PATH"
    env = os.environ.copy()
    env["GH_TOKEN"] = token
    proc = subprocess.run(  # noqa: S603 — fixed argv, token via env
        [gh, *args], capture_output=True, text=True, env=env, check=False
    )
    return proc.returncode, proc.stdout, proc.stderr


REF_RE = re.compile(r"^refs/(?:heads|tags|pull)/[A-Za-z0-9._/+-]+$")


def list_caches(repo: str, token: str, ref: str | None = None) -> list[dict[str, Any]]:
    caches: list[dict[str, Any]] = []
    page = 1
    # Ref scoping: when this script runs on a PR, the GITHUB_TOKEN is
    # repo-scoped (not ref-scoped) and would otherwise list and prune
    # superseded entries across every ref in the repo, including main
    # and unrelated PRs. Filtering by ref keeps each run honest about
    # which caches it can touch.
    ref_query = f"&ref={ref}" if ref else ""
    while True:
        path = (
            f"/repos/{repo}/actions/caches"
            f"?per_page=100&page={page}&sort=created_at&direction=desc"
            f"{ref_query}"
        )
        rc, out, err = _gh(
            ["api", "-H", "X-GitHub-Api-Version: 2022-11-28", path], token
        )
        if rc != 0:
            print(f"WARN: list caches failed: {err.strip()[:200]}", file=sys.stderr)
            return caches
        try:
            payload = json.loads(out)
        except json.JSONDecodeError as exc:
            print(f"WARN: cannot parse cache list: {exc}", file=sys.stderr)
            return caches
        batch = payload.get("actions_caches", [])
        caches.extend(batch)
        if len(batch) < 100:
            break
        page += 1
    return caches


def delete_cache(repo: str, token: str, cache_id: int) -> bool:
    path = f"/repos/{repo}/actions/caches/{cache_id}"
    rc, _, err = _gh(["api", "-X", "DELETE", path], token)
    if rc == 0:
        return True
    print(f"WARN: delete cache {cache_id} failed: {err.strip()[:200]}", file=sys.stderr)
    return False


def normalize_prefix(key: str) -> str:
    """Strip a trailing ``-<40-hex-sha>`` so siblings group together."""
    m = SHA_RE.match(key)
    return m.group("prefix") if m else key


def plan_deletions(
    caches: list[dict[str, Any]], keep: int
) -> tuple[list[dict[str, Any]], list[dict[str, Any]]]:
    groups: dict[tuple[str, str], list[dict[str, Any]]] = defaultdict(list)
    for c in caches:
        # An unexpectedly shaped cache entry (missing ref/key/created_at)
        # must not crash the housekeeping run. Skip with a notice instead.
        try:
            groups[(c["ref"], normalize_prefix(c["key"]))].append(c)
        except KeyError as exc:
            print(
                f"WARN: skipping cache with missing field {exc}: id={c.get('id', '?')}",
                file=sys.stderr,
            )
    keep_list: list[dict[str, Any]] = []
    delete_list: list[dict[str, Any]] = []
    for members in groups.values():
        try:
            members.sort(key=lambda c: c["created_at"], reverse=True)
        except KeyError:
            print("WARN: skipping group with missing created_at", file=sys.stderr)
            continue
        keep_list.extend(members[:keep])
        delete_list.extend(members[keep:])
    return keep_list, delete_list


def human_size(n: int) -> str:
    return f"{n / 1e9:.2f} GB" if n >= 1e9 else f"{n / 1e6:.1f} MB"


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--repo",
        default=os.environ.get("GITHUB_REPOSITORY"),
        help="OWNER/REPO (defaults to $GITHUB_REPOSITORY)",
    )
    parser.add_argument(
        "--token",
        default=os.environ.get("GITHUB_TOKEN"),
        help="GitHub token with actions:write (defaults to $GITHUB_TOKEN)",
    )
    parser.add_argument(
        "--keep",
        type=int,
        default=1,
        help="How many newest caches to keep per (ref, key-prefix). Default: 1",
    )
    parser.add_argument(
        "--ref",
        default=os.environ.get("GITHUB_REF"),
        help=(
            "Restrict the prune to caches that belong to this ref"
            " (e.g. refs/heads/main, refs/pull/123/merge). Defaults to"
            " $GITHUB_REF so that PR runs only touch their own caches."
            " Pass an empty string to operate repo-wide."
        ),
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Print the deletion plan without calling DELETE",
    )
    args = parser.parse_args(argv)

    if args.repo and not REPO_RE.match(args.repo):
        print(f"INFO: invalid --repo value: {args.repo!r}. Skipping.", file=sys.stderr)
        return 0

    if args.ref and not REF_RE.match(args.ref):
        print(
            f"INFO: --ref does not look like a refs/... path: {args.ref!r}. Skipping.",
            file=sys.stderr,
        )
        return 0

    # --keep < 1 would empty every group. Treat as a misuse rather than a
    # silent wipe; exit 0 to keep CI green.
    if args.keep < 1:
        print(
            f"INFO: --keep must be >= 1 (got {args.keep}). Skipping.",
            file=sys.stderr,
        )
        return 0

    if not args.repo or not args.token:
        # Housekeeping should never fail a build. Exit 0 with a notice.
        print(
            "INFO: --repo and --token are required (or $GITHUB_REPOSITORY"
            " and $GITHUB_TOKEN). Skipping cache prune.",
            file=sys.stderr,
        )
        return 0

    caches = list_caches(args.repo, args.token, ref=args.ref or None)
    if not caches:
        print("INFO: no caches found (or insufficient permissions). Nothing to do.")
        return 0

    keep_list, delete_list = plan_deletions(caches, keep=args.keep)
    total = sum(c["size_in_bytes"] for c in caches)
    keep_bytes = sum(c["size_in_bytes"] for c in keep_list)
    drop_bytes = sum(c["size_in_bytes"] for c in delete_list)

    print(f"Repo:    {args.repo}")
    print(f"Caches:  {len(caches)} ({human_size(total)})")
    print(f"Keeping: {len(keep_list)} ({human_size(keep_bytes)})")
    print(f"Pruning: {len(delete_list)} ({human_size(drop_bytes)})")

    if not delete_list:
        return 0

    deleted = 0
    freed = 0
    try:
        sorted_delete_list = sorted(
            delete_list, key=lambda c: (c["ref"], c["created_at"])
        )
    except KeyError as exc:
        print(f"WARN: cannot sort deletion list: missing {exc}", file=sys.stderr)
        return 0
    for c in sorted_delete_list:
        try:
            action = "DRY-RUN" if args.dry_run else "DELETE "
            print(
                f"  {action} id={c['id']:>12}  {c['ref']:<28}  "
                f"{c['size_in_bytes'] / 1e6:>7.1f} MB  {c['key'][:80]}"
            )
            if args.dry_run or delete_cache(args.repo, args.token, c["id"]):
                deleted += 1
                freed += c["size_in_bytes"]
        except KeyError as exc:
            print(
                f"WARN: skipping cache with missing field {exc}",
                file=sys.stderr,
            )
            continue

    verb = "Would free" if args.dry_run else "Freed"
    print(f"{verb} {human_size(freed)} across {deleted} cache(s).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
