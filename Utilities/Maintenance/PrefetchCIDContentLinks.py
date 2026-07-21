#!/usr/bin/env python3
"""Prefetch every CID referenced by a ``.cid`` content link into the
``ExternalData_OBJECT_STORES`` directory so the GitHub Actions cache entry
saved after a CI run is complete.

Without this step, only the data for modules selected for compilation (and
whose tests ran) trigger an ExternalData fetch. Everything else stays out of
the store, and the next cold boot has to re-download it from the gateways.
This script walks the whole source tree, reads every ``.cid`` file, and fills
in any missing object at ``<store>/cid/<cid>`` via the same gateway list used
by ``CMake/ITKExternalData.cmake``.

Integrity: the IPFS gateways serve content-addressed bytes, so a correctly
configured gateway cannot return the wrong bytes for a given CID — the CID
*is* the content hash. CMake's ExternalData verifies the hash again at
consumer time, so any bad prefetch is self-healing.
"""
from __future__ import annotations

import argparse
import concurrent.futures as cf
import os
import subprocess
import sys
import time
from pathlib import Path
from urllib.parse import urlsplit

# Same ordered list as CMake/ITKExternalData.cmake. The GitHub Pages mirror
# comes first because it is the lowest-latency bulk-hosted option and does
# not rate-limit CI.
GATEWAYS = (
    "https://insightsoftwareconsortium.github.io/ITKTestingData/CID/{cid}",
    "https://ipfs.io/ipfs/{cid}",
    "https://gateway.pinata.cloud/ipfs/{cid}",
    "https://cloudflare-ipfs.com/ipfs/{cid}",
    "https://dweb.link/ipfs/{cid}",
)

PER_URL_TIMEOUT_SECONDS = 60
MAX_WORKERS_DEFAULT = 16


def collect_cids(repo_root: Path) -> dict[str, list[Path]]:
    """Map each CID found in the repo to the ``.cid`` files that reference it."""
    cids: dict[str, list[Path]] = {}
    for p in repo_root.rglob("*.cid"):
        if ".git" in p.parts:
            continue
        try:
            cid = p.read_text().strip()
        except OSError as e:
            print(f"WARN: cannot read {p}: {e}", file=sys.stderr)
            continue
        if not cid or any(c.isspace() for c in cid):
            print(f"WARN: malformed .cid file {p}", file=sys.stderr)
            continue
        cids.setdefault(cid, []).append(p)
    return cids


def fetch_one(cid: str, dest: Path) -> tuple[str, str, int]:
    """Download ``cid`` to ``dest``, trying gateways in order.

    Returns ``(cid, status, bytes_written)`` where status is one of
    ``ok``, ``skip`` (already present), or ``fail``.
    """
    if dest.exists() and dest.stat().st_size > 0:
        return cid, "skip", dest.stat().st_size

    tmp = dest.with_suffix(dest.suffix + ".part")
    last_err: str | None = None
    for tpl in GATEWAYS:
        url = tpl.format(cid=cid)
        parts = urlsplit(url)
        if parts.scheme != "https":
            last_err = f"refusing non-https URL: {url}"
            continue
        # Delegate HTTPS to curl: avoids pulling in third-party HTTP libs
        # and keeps TLS cert verification entirely in the system stack.
        cmd = [
            "curl",
            "--silent",
            "--show-error",
            "--location",
            "--fail",
            "--proto",
            "=https",
            "--max-time",
            str(PER_URL_TIMEOUT_SECONDS),
            "--user-agent",
            "itk-ci-prefetch/1",
            "--output",
            str(tmp),
            "--",
            url,
        ]
        try:
            r = subprocess.run(
                cmd,
                capture_output=True,
                check=False,
                timeout=PER_URL_TIMEOUT_SECONDS + 10,
            )
        except subprocess.TimeoutExpired:
            last_err = f"curl timeout on {url}"
            continue
        if r.returncode != 0:
            last_err = f"curl rc={r.returncode} on {url}: {r.stderr.decode(errors='replace').strip()}"
            continue
        try:
            nbytes = tmp.stat().st_size
        except OSError as e:
            last_err = f"{url}: {e}"
            continue
        if nbytes == 0:
            last_err = f"empty body from {url}"
            continue
        tmp.replace(dest)
        return cid, "ok", nbytes
    if tmp.exists():
        try:
            tmp.unlink()
        except OSError:
            pass
    return cid, f"fail: {last_err}", 0


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument(
        "--repo-root",
        type=Path,
        default=Path.cwd(),
        help="Root of the source tree to scan for .cid files.",
    )
    ap.add_argument(
        "--store",
        type=Path,
        default=Path(os.environ.get("ExternalData_OBJECT_STORES", "")),
        help="ExternalData_OBJECT_STORES directory. Defaults to the env var.",
    )
    ap.add_argument(
        "--jobs",
        type=int,
        default=MAX_WORKERS_DEFAULT,
        help=f"Parallel download workers (default {MAX_WORKERS_DEFAULT}).",
    )
    ap.add_argument(
        "--fail-on-missing",
        action="store_true",
        help="Exit non-zero if any CID could not be fetched.",
    )
    args = ap.parse_args()

    if not args.store or str(args.store) == ".":
        print(
            "ERROR: --store or ExternalData_OBJECT_STORES must be set", file=sys.stderr
        )
        return 2
    cid_dir = args.store / "cid"
    cid_dir.mkdir(parents=True, exist_ok=True)

    cids = collect_cids(args.repo_root)
    print(
        f"==> {len(cids)} unique CIDs referenced by {sum(len(v) for v in cids.values())} .cid files"
    )

    start = time.monotonic()
    ok = skip = fail = 0
    bytes_ok = 0
    failed: list[str] = []
    with cf.ThreadPoolExecutor(max_workers=max(1, args.jobs)) as pool:
        futures = {pool.submit(fetch_one, cid, cid_dir / cid): cid for cid in cids}
        for i, fut in enumerate(cf.as_completed(futures), 1):
            cid, status, nbytes = fut.result()
            if status == "ok":
                ok += 1
                bytes_ok += nbytes
            elif status == "skip":
                skip += 1
            else:
                fail += 1
                failed.append(f"{cid}: {status}")
            if i % 500 == 0 or i == len(futures):
                elapsed = time.monotonic() - start
                print(
                    f"  [{i}/{len(futures)}] ok={ok} skip={skip} fail={fail} "
                    f"downloaded={bytes_ok / 1e6:.1f} MB in {elapsed:.0f}s",
                    flush=True,
                )

    elapsed = time.monotonic() - start
    print(
        f"==> prefetch done in {elapsed:.0f}s: ok={ok} skip={skip} fail={fail} "
        f"downloaded={bytes_ok / 1e6:.1f} MB"
    )
    if failed:
        print(f"==> {len(failed)} CID(s) could not be fetched:", file=sys.stderr)
        for line in failed[:50]:
            print(f"  {line}", file=sys.stderr)
        if len(failed) > 50:
            print(f"  ... and {len(failed) - 50} more", file=sys.stderr)
    return 1 if (fail and args.fail_on_missing) else 0


if __name__ == "__main__":
    sys.exit(main())
