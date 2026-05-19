#!/usr/bin/env python3
"""Pin ITKTestingData/CID/ files to Filebase and add missing manifest entries.

For each file under ``<ITKTestingData>/CID/``, this script:

1. Skips the file if the CID (its filename) is already in
   ``Testing/Data/content-links.manifest``.
2. Otherwise, scans the ITK source tree for ``.cid`` content links whose
   value matches the CID. If no link references it, skips.
3. Uploads the file's bytes to Filebase via the Filebase S3 API and
   verifies the CID Filebase reports matches the filename.
4. Adds a ``<cid> <repo-relative-path>`` entry to the manifest for every
   ``.cid`` content link in the source tree that holds the value.

Use case: the developer has files in their local ``ITKTestingData/CID/``
mirror plus matching ``.cid`` content links in the ITK source tree, but
the manifest is missing those entries and the bytes may not be pinned on
Filebase. This script reconciles that state without touching the
source-tree ``.cid`` files.
"""

from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path

import upload as upload_module
from upload import (
    CIDV1_RE,
    MANIFEST,
    REPO_ROOT,
    update_manifest,
    upload_file_to_filebase,
)


def load_manifest_cids() -> set[str]:
    """Return the set of CIDs already present in the manifest."""
    if not MANIFEST.exists():
        return set()
    cids: set[str] = set()
    for line in MANIFEST.read_text().splitlines():
        if line.startswith("#") or not line.strip():
            continue
        fields = line.split()
        if len(fields) >= 2:
            cids.add(fields[0])
    return cids


def find_cid_links() -> dict[str, list[Path]]:
    """Map CID value to the .cid links in the source tree that reference it.

    Uses ``git ls-files`` (tracked + non-ignored untracked) so build
    directories, the pixi env, and other non-source paths are skipped
    automatically without having to enumerate them.
    """
    result = subprocess.run(
        [
            "git",
            "-C",
            str(REPO_ROOT),
            "ls-files",
            "--cached",
            "--others",
            "--exclude-standard",
            "*.cid",
        ],
        capture_output=True,
        text=True,
        check=True,
    )
    mapping: dict[str, list[Path]] = {}
    for rel in result.stdout.splitlines():
        if not rel.strip():
            continue
        path = REPO_ROOT / rel
        try:
            value = path.read_text().strip()
        except OSError:
            continue
        if not CIDV1_RE.match(value):
            continue
        mapping.setdefault(value, []).append(path)
    return mapping


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description=(
            "Upload ITKTestingData/CID/ files to Filebase and add manifest "
            "entries for them, but only when a matching .cid content link "
            "exists in the source tree and the CID is absent from the "
            "manifest."
        ),
    )
    parser.add_argument(
        "--testing-data-repo",
        metavar="PATH",
        required=True,
        help=(
            "Local clone of "
            "github.com/InsightSoftwareConsortium/ITKTestingData; the script "
            "reads files from <PATH>/CID/."
        ),
    )
    parser.add_argument(
        "--bucket",
        help="Filebase IPFS bucket (default: $FILEBASE_BUCKET).",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help=(
            "List CIDs that would be uploaded; do not upload or modify the " "manifest."
        ),
    )
    args = parser.parse_args(argv)

    testing_data_repo = Path(args.testing_data_repo).resolve()
    if not testing_data_repo.is_dir():
        sys.exit(
            f"ERROR: --testing-data-repo path is not a directory: "
            f"{args.testing_data_repo}"
        )
    cid_dir = testing_data_repo / "CID"
    if not cid_dir.is_dir():
        sys.exit(f"ERROR: {cid_dir} not found.")

    if args.dry_run:
        access_key = secret_key = bucket = ""
    else:
        upload_module._check_node_available()
        access_key, secret_key, bucket = upload_module._credentials(args)

    manifest_cids = load_manifest_cids()
    print(f"==> {len(manifest_cids)} CID(s) already in the manifest")

    print(f"==> Indexing .cid content links under {REPO_ROOT}...")
    cid_links = find_cid_links()
    print(f"    {len(cid_links)} unique CID(s) referenced by .cid links")

    candidates: list[tuple[str, Path, list[Path]]] = []
    for cid_file in sorted(cid_dir.iterdir()):
        if not cid_file.is_file():
            continue
        cid = cid_file.name
        if not CIDV1_RE.match(cid):
            continue
        if cid in manifest_cids:
            continue
        links = cid_links.get(cid)
        if not links:
            continue
        candidates.append((cid, cid_file, links))

    if not candidates:
        print(
            "Nothing to do: every CID under ITKTestingData/CID/ is either "
            "already in the manifest or has no matching .cid link in the "
            "source tree."
        )
        return 0

    print(f"==> {len(candidates)} CID(s) to upload")
    if args.dry_run:
        for cid, _, links in candidates:
            for link in links:
                rel = link.with_suffix("").relative_to(REPO_ROOT).as_posix()
                print(f"WOULD-UPLOAD  {cid}  -> {rel}")
        print("(--dry-run: no Filebase uploads, no manifest changes)")
        return 0

    fail = 0
    for cid, cid_file, links in candidates:
        print(f"==> Uploading {cid}")
        try:
            returned_cid = upload_file_to_filebase(
                cid_file, bucket, access_key, secret_key
            )
        except (subprocess.CalledProcessError, RuntimeError) as exc:
            print(f"FAIL  {cid}  upload-failed: {exc}", file=sys.stderr)
            fail += 1
            continue
        if returned_cid != cid:
            print(
                f"FAIL  {cid}  cid-mismatch: filebase returned "
                f"{returned_cid} for ITKTestingData/CID/{cid}",
                file=sys.stderr,
            )
            fail += 1
            continue
        for link in links:
            real_rel = link.with_suffix("").relative_to(REPO_ROOT).as_posix()
            update_manifest(cid, real_rel)
            print(f"OK    {cid}  -> {real_rel}")

    if fail:
        print(f"WARN: {fail} CID(s) failed to sync.", file=sys.stderr)
        return 2

    print()
    print(f"Done. Updated {MANIFEST.relative_to(REPO_ROOT)}.")
    print("Next steps:")
    print("  git add Testing/Data/content-links.manifest")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
