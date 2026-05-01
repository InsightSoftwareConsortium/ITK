#!/usr/bin/env python3
"""Normalize ITK content links: convert ``.md5`` / ``.shaNNN`` links to ``.cid``.

For each content link found, the script:

1. Fetches bytes via the gateway templates declared in
   ``CMake/ITKExternalData.cmake`` (identical order to the build).
2. Verifies the bytes against the declared hash or CID.
3. Re-materializes the file alongside the link, then runs the Filebase
   uploader from ``upload.py`` so a fresh CID is produced under the
   ``unixfs-v1-2025`` profile and pinned on Filebase. The old
   ``.md5`` / ``.shaNNN`` link is removed; a ``.cid`` link is written
   in its place.

For ``.cid`` content links, this re-uploads under the current
``unixfs-v1-2025`` profile so a CID produced years ago with a different
chunker is regenerated to match the build pipeline.
"""

from __future__ import annotations

import argparse
import hashlib
import re
import subprocess
import sys
import tempfile
import urllib.parse
from pathlib import Path

import upload as upload_module
from upload import (
    REPO_ROOT,
    upload_file_to_filebase,
    update_manifest,
    mirror_to_testing_data,
    CIDV1_RE,
)

CMAKE_FILE = REPO_ROOT / "CMake" / "ITKExternalData.cmake"

ALGO_UC = {
    "md5": "MD5",
    "sha1": "SHA1",
    "sha224": "SHA224",
    "sha256": "SHA256",
    "sha384": "SHA384",
    "sha512": "SHA512",
    "cid": "cid",
}


def parse_url_templates(cmake_file: Path) -> list[str]:
    """Extract URL templates from the ``ExternalData_URL_TEMPLATES`` list().

    Locates the ``list(APPEND ExternalData_URL_TEMPLATES ...)`` invocation,
    walks its argument list with a paren-aware scanner (templates contain
    ``%(hash)`` / ``%(algo)``, so naive ``.*?`` regex closes the match
    prematurely on those inner parens), and returns every quoted argument
    that contains ``%(hash)`` in declaration order.
    """
    text = cmake_file.read_text()
    anchor = re.search(
        r"list\s*\(\s*APPEND\s+ExternalData_URL_TEMPLATES\s",
        text,
    )
    if anchor is None:
        sys.exit(
            f"ERROR: failed to find ExternalData_URL_TEMPLATES list() in "
            f"{cmake_file}"
        )

    depth = 1
    i = anchor.end()
    in_string = False
    end_idx: int | None = None
    while i < len(text):
        ch = text[i]
        if in_string:
            if ch == "\\":
                i += 2
                continue
            if ch == '"':
                in_string = False
        else:
            if ch == '"':
                in_string = True
            elif ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    end_idx = i
                    break
        i += 1

    if end_idx is None:
        sys.exit(
            f"ERROR: unterminated ExternalData_URL_TEMPLATES list() in "
            f"{cmake_file}"
        )

    body = text[anchor.end() : end_idx]
    return [t for t in re.findall(r'"([^"]+)"', body) if "%(hash)" in t]


def render_url(template: str, algo: str, value: str) -> str:
    return template.replace("%(algo)", algo).replace("%(hash)", value)


def hash_bytes(ext: str, data: bytes) -> str:
    return hashlib.new(ext, data).hexdigest()


def fetch_and_verify(ext: str, value: str, templates: list[str]) -> Path:
    """Download bytes from the first gateway whose response verifies; return tempfile path.

    For ``.cid`` links, accept any successful HTTP fetch from a path containing
    ``/ipfs/`` because IPFS HTTP gateways verify CIDs server-side. For hash
    links, recompute the digest locally.
    """
    import requests  # imported lazily so --help works without the env active

    algo_uc = ALGO_UC.get(ext)
    if algo_uc is None:
        raise RuntimeError(f"Unknown content-link extension: .{ext}")

    last_error: Exception | None = None
    for template in templates:
        rendered = render_url(template, algo_uc, value)
        if ext != "cid" and "/ipfs/" in urllib.parse.urlparse(rendered).path:
            continue
        try:
            response = requests.get(rendered, timeout=(10, 120))
            response.raise_for_status()
        except requests.RequestException as exc:
            last_error = exc
            continue

        body = response.content
        if not body:
            continue

        if ext == "cid":
            if "/ipfs/" not in urllib.parse.urlparse(rendered).path:
                # Non-IPFS origin (e.g. GitHub Pages mirror). We can't verify
                # locally without risking chunker-drift false negatives, so
                # we keep looking for an IPFS gateway entry.
                continue
        else:
            actual = hash_bytes(ext, body)
            if actual.lower() != value.lower():
                print(
                    f"WARN:   content from {rendered} did not verify; "
                    "trying next gateway",
                    file=sys.stderr,
                )
                continue

        out = Path(tempfile.mkstemp(prefix="itk-content-link.")[1])
        out.write_bytes(body)
        return out

    raise RuntimeError(
        f"Failed to fetch and verify {ext}={value} from any gateway"
        + (f" (last error: {last_error})" if last_error else "")
    )


def enumerate_links(target: Path, hash_only: bool, cid_only: bool) -> list[Path]:
    if target.is_file():
        return [target]
    exts = {f".{e}" for e in upload_module.CONTENT_LINK_EXTS}
    found = sorted(p for p in target.rglob("*") if p.is_file() and p.suffix in exts)
    filtered: list[Path] = []
    for link in found:
        ext = link.suffix.lstrip(".")
        if hash_only and ext == "cid":
            continue
        if cid_only and ext != "cid":
            continue
        filtered.append(link)
    return filtered


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description=(
            "Normalize ITK content links: convert .md5 / .shaNNN to .cid and "
            "regenerate existing .cid under the unixfs-v1-2025 profile."
        ),
    )
    parser.add_argument("target", help="Path or directory to process")
    parser.add_argument(
        "--testing-data-repo",
        metavar="PATH",
        help="Forwarded to upload.py; mirror bytes into a local ITKTestingData clone.",
    )
    parser.add_argument(
        "--bucket",
        help="Filebase IPFS bucket (default: $FILEBASE_BUCKET).",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="List what would change, modify nothing.",
    )
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument(
        "--hash-only",
        action="store_true",
        help="Process only .md5 / .shaNNN links; leave .cid alone.",
    )
    mode.add_argument(
        "--cid-only",
        action="store_true",
        help="Process only .cid links (re-hash under unixfs-v1-2025).",
    )
    args = parser.parse_args(argv)

    target = Path(args.target)
    if not target.exists():
        sys.exit(f"ERROR: not found: {args.target}")
    if not CMAKE_FILE.exists():
        sys.exit(f"ERROR: cannot find {CMAKE_FILE}")

    upload_module._check_node_available()
    access_key, secret_key, bucket = upload_module._credentials(args)

    testing_data_repo: Path | None = None
    if args.testing_data_repo:
        testing_data_repo = Path(args.testing_data_repo).resolve()
        if not (testing_data_repo / ".git").exists():
            sys.exit(
                f"ERROR: --testing-data-repo is not a git checkout: "
                f"{args.testing_data_repo}"
            )

    templates = parse_url_templates(CMAKE_FILE)
    print(f"==> Loaded {len(templates)} gateway template(s) from {CMAKE_FILE}")

    links = enumerate_links(target, args.hash_only, args.cid_only)
    if not links:
        print(f"No matching content links under {target}. Nothing to do.")
        return 0
    print(f"==> Processing {len(links)} content link(s)...")
    if args.dry_run:
        print("(--dry-run: no files will be modified)")

    fail = 0
    for link in links:
        ext = link.suffix.lstrip(".")
        value = link.read_text().strip()
        real_file = link.with_suffix("")
        if not value:
            print(f"FAIL     {link}  empty-content-link", file=sys.stderr)
            fail += 1
            continue
        if ext == "cid" and not CIDV1_RE.match(value):
            print(f"FAIL     {link}  invalid-cid", file=sys.stderr)
            fail += 1
            continue

        if args.dry_run:
            print(
                f"WOULD-NORMALIZE  {link}  ({ext}={value})  ->  {real_file}.cid"
            )
            continue

        print(f"==> Normalizing {link} ({ext}={value})")

        if real_file.exists():
            sys.exit(
                f"ERROR: refusing to normalize: {real_file} already exists. "
                "Delete or move it first."
            )

        try:
            tmp_bytes = fetch_and_verify(ext, value, templates)
        except RuntimeError as exc:
            print(f"FAIL     {link}  {exc}", file=sys.stderr)
            fail += 1
            continue

        tmp_bytes.rename(real_file)
        link.unlink()

        try:
            cid = upload_file_to_filebase(real_file, bucket, access_key, secret_key)
        except (subprocess.CalledProcessError, RuntimeError) as exc:
            print(f"FAIL     {link}  upload-failed: {exc}", file=sys.stderr)
            link.write_text(value + "\n")
            real_file.unlink(missing_ok=True)
            fail += 1
            continue

        if testing_data_repo is not None:
            mirror_to_testing_data(real_file, cid, testing_data_repo)

        cid_path = real_file.with_name(real_file.name + ".cid")
        cid_path.write_text(cid + "\n")
        real_file.unlink()
        rel_path = real_file.relative_to(REPO_ROOT).as_posix()
        update_manifest(cid, rel_path)
        print(f"NORMALIZE  {link}  ({ext})  ->  {cid_path}")

    if fail:
        print(f"WARN: {fail} content link(s) failed to normalize.", file=sys.stderr)
        return 2

    print(
        "Done. Review changes and commit as a STYLE: commit "
        "(see Documentation/AI/git-commits.md)."
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
