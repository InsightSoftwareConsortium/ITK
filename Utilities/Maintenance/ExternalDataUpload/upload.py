#!/usr/bin/env python3
"""Upload a file to Filebase IPFS storage and replace it with a .cid content link.

Builds a CARv1 of the input via ``npx ipfs-car pack`` (defaults: 1 MiB chunks,
1024 children per node, raw leaves, CIDv1 — matches the ``unixfs-v1-2025`` /
IPIP-0499 profile so CIDs are reproducible across implementations) and uploads
the CAR to a Filebase IPFS bucket through its S3-compatible REST API with the
``import: car`` user metadata header. Filebase imports the CAR server-side and
returns the resulting CID in object metadata, which is read back via
``head_object`` and compared against the locally computed root CID.

The ``.cid`` content link, the manifest entry in
``Testing/Data/content-links.manifest``, and (optionally) a mirror of the bytes
in a local ITKTestingData clone are all produced in the same invocation.
"""

from __future__ import annotations

import argparse
import os
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
MANIFEST = REPO_ROOT / "Testing" / "Data" / "content-links.manifest"
GITHUB_FILE_LIMIT_BYTES = 50 * 1024 * 1024
FILEBASE_ENDPOINT = "https://s3.filebase.com"

CONTENT_LINK_EXTS = ("cid", "md5", "sha1", "sha224", "sha256", "sha384", "sha512")
CIDV1_RE = re.compile(r"^baf[a-z0-9]{50,}$")


def build_car(input_path: Path, output_car: Path) -> str:
    """Pack ``input_path`` into a CARv1 at ``output_car`` and return its root CID.

    Uses ``npx ipfs-car pack`` with ``--no-wrap`` (single-file upload, no
    wrapping directory). ipfs-car v1+ defaults match the unixfs-v1-2025 profile
    (1 MiB chunks, 1024 links/node, raw leaves, CIDv1), so no extra flags are
    needed to produce a reproducible CID.
    """
    result = subprocess.run(
        [
            "npx",
            "--yes",
            "ipfs-car",
            "pack",
            str(input_path),
            "--no-wrap",
            "--output",
            str(output_car),
        ],
        capture_output=True,
        text=True,
        check=True,
    )
    for line in reversed(result.stdout.splitlines()):
        token = line.strip().split()[-1] if line.strip() else ""
        if CIDV1_RE.match(token):
            return token
    raise RuntimeError(
        f"Could not parse CID from `npx ipfs-car pack` output:\n{result.stdout}"
    )


def upload_car_to_filebase(
    car_path: Path,
    bucket: str,
    object_key: str,
    access_key: str,
    secret_key: str,
) -> str:
    """Upload a CAR to a Filebase IPFS bucket and return the CID Filebase reports.

    Setting ``Metadata={"import": "car"}`` tells Filebase to import the CAR
    server-side; the imported root CID is then exposed via
    ``head_object()['Metadata']['cid']``. ``put_object`` is used directly
    rather than ``upload_file`` because the latter's multipart code path can
    strip user metadata on small payloads.
    """
    import boto3  # imported lazily so --help works without the env active

    s3 = boto3.client(
        "s3",
        endpoint_url=FILEBASE_ENDPOINT,
        aws_access_key_id=access_key,
        aws_secret_access_key=secret_key,
        region_name="us-east-1",
    )
    with car_path.open("rb") as f:
        s3.put_object(
            Bucket=bucket,
            Key=object_key,
            Body=f,
            Metadata={"import": "car"},
        )
    head = s3.head_object(Bucket=bucket, Key=object_key)
    return head.get("Metadata", {}).get("cid", "")


def upload_file_to_filebase(
    input_path: Path,
    bucket: str,
    access_key: str,
    secret_key: str,
) -> str:
    """End-to-end: build CAR for ``input_path``, upload it, verify, return CID."""
    object_key = input_path.name + ".car"
    with tempfile.NamedTemporaryFile(suffix=".car", delete=False) as tmp:
        car_path = Path(tmp.name)
    try:
        local_cid = build_car(input_path, car_path)
        remote_cid = upload_car_to_filebase(
            car_path, bucket, object_key, access_key, secret_key
        )
        if not remote_cid:
            raise RuntimeError(
                f"Filebase did not return a CID for {object_key}. The CAR may "
                "not have been recognised — check the bucket is an IPFS bucket "
                "and the access key has write permission."
            )
        if local_cid != remote_cid:
            raise RuntimeError(
                f"CID mismatch: local={local_cid}, filebase={remote_cid}. "
                "This indicates a chunker/profile drift between this client "
                "and Filebase — file an issue."
            )
        return local_cid
    finally:
        car_path.unlink(missing_ok=True)


def update_manifest(cid: str, rel_path: str) -> None:
    """Insert/replace ``cid <rel_path>`` in Testing/Data/content-links.manifest."""
    MANIFEST.parent.mkdir(parents=True, exist_ok=True)

    header_lines: list[str] = []
    data_lines: list[str] = []
    if MANIFEST.exists():
        for line in MANIFEST.read_text().splitlines():
            if line.startswith("#"):
                header_lines.append(line)
            elif line.strip():
                fields = line.split()
                if len(fields) >= 2 and fields[1] == rel_path:
                    continue
                data_lines.append(line)
    else:
        header_lines = [
            "# ITK content-link manifest",
            "# One CID per line, format: <cid> <repo-relative-path>",
            "# Maintained by Utilities/Maintenance/ExternalDataUpload/upload.py",
        ]

    data_lines.append(f"{cid} {rel_path}")
    data_lines.sort(key=lambda s: s.split()[1])

    MANIFEST.write_text("\n".join(header_lines + data_lines) + "\n")


def mirror_to_testing_data(
    file_path: Path, cid: str, testing_data_repo: Path
) -> bool:
    """Copy ``file_path`` to ``<repo>/CID/<cid>`` and ``git add`` it.

    Returns False (with a warning) for files over GitHub's 50 MB push limit;
    True after a successful copy + stage.
    """
    size = file_path.stat().st_size
    if size > GITHUB_FILE_LIMIT_BYTES:
        print(
            f"WARNING: {file_path.name} is {size} bytes (> 50 MB). GitHub "
            "rejects pushes containing files > 50 MB, so it will NOT be "
            "mirrored to ITKTestingData. The Filebase upload still succeeded; "
            "the .cid content link will still be produced.",
            file=sys.stderr,
        )
        return False

    mirror_dir = testing_data_repo / "CID"
    mirror_dir.mkdir(parents=True, exist_ok=True)
    mirror_path = mirror_dir / cid
    print(f"==> Mirroring to ITKTestingData: CID/{cid}")
    shutil.copy2(file_path, mirror_path)
    try:
        subprocess.run(
            ["git", "-C", str(testing_data_repo), "add", f"CID/{cid}"],
            check=True,
        )
    except subprocess.CalledProcessError:
        mirror_path.unlink(missing_ok=True)
        raise
    return True


def _validate_input(file_arg: str) -> Path:
    file_path = Path(file_arg)
    if not file_path.exists():
        sys.exit(f"ERROR: File not found: {file_arg}")
    if file_path.is_symlink():
        sys.exit(
            f"ERROR: Symlink paths are not supported: {file_arg}\n"
            "       Pass the real file path instead."
        )
    if not file_path.is_file():
        sys.exit(f"ERROR: Not a regular file: {file_arg}")

    abs_path = file_path.resolve()
    try:
        abs_path.relative_to(REPO_ROOT)
    except ValueError:
        sys.exit(f"ERROR: File must be inside the repository: {abs_path}")

    suffix = abs_path.suffix.lstrip(".")
    if suffix in CONTENT_LINK_EXTS:
        sys.exit(f"ERROR: File is already a .{suffix} content link: {file_arg}")

    rel = abs_path.relative_to(REPO_ROOT).as_posix()
    if any(c.isspace() for c in rel):
        sys.exit(
            f"ERROR: Filepath contains whitespace, which is not supported: {rel}\n"
            "       Rename the file to remove spaces before uploading."
        )

    return abs_path


def _credentials(args: argparse.Namespace) -> tuple[str, str, str]:
    access_key = os.environ.get("FILEBASE_ACCESS_KEY", "")
    secret_key = os.environ.get("FILEBASE_SECRET_KEY", "")
    bucket = args.bucket or os.environ.get("FILEBASE_BUCKET", "")
    missing = [
        name
        for name, value in [
            ("FILEBASE_ACCESS_KEY", access_key),
            ("FILEBASE_SECRET_KEY", secret_key),
            ("FILEBASE_BUCKET (or --bucket)", bucket),
        ]
        if not value
    ]
    if missing:
        sys.exit(
            "ERROR: Missing Filebase credentials: " + ", ".join(missing) + "\n"
            "       See: Utilities/Maintenance/ExternalDataUpload/README.md"
        )
    return access_key, secret_key, bucket


def _check_node_available() -> None:
    if shutil.which("npx") is None:
        sys.exit(
            "ERROR: 'npx' not found on PATH (Node.js required for ipfs-car).\n"
            "       Run inside the pixi environment:\n"
            "         pixi run -e external-data-upload python "
            "Utilities/Maintenance/ExternalDataUpload/upload.py <file>"
        )


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description=(
            "Upload a file to Filebase IPFS storage; produce a .cid content "
            "link, update Testing/Data/content-links.manifest, and optionally "
            "mirror the bytes into ITKTestingData."
        ),
    )
    parser.add_argument("file", help="Path to the file to upload")
    parser.add_argument(
        "--testing-data-repo",
        metavar="PATH",
        help=(
            "Local clone of github.com/InsightSoftwareConsortium/ITKTestingData; "
            "files ≤ 50 MB are copied to <PATH>/CID/<cid> and `git add`ed."
        ),
    )
    parser.add_argument(
        "--bucket",
        help="Filebase IPFS bucket (default: $FILEBASE_BUCKET).",
    )
    args = parser.parse_args(argv)

    _check_node_available()
    abs_path = _validate_input(args.file)
    access_key, secret_key, bucket = _credentials(args)

    testing_data_repo: Path | None = None
    if args.testing_data_repo:
        testing_data_repo = Path(args.testing_data_repo).resolve()
        if not testing_data_repo.is_dir():
            sys.exit(
                f"ERROR: --testing-data-repo path is not a directory: "
                f"{args.testing_data_repo}"
            )
        if not (testing_data_repo / ".git").exists():
            sys.exit(
                f"ERROR: --testing-data-repo is not a git checkout: "
                f"{args.testing_data_repo}"
            )

    rel_path = abs_path.relative_to(REPO_ROOT).as_posix()
    print(f"==> Packing {abs_path.name} into a CAR (unixfs-v1-2025 profile)...")
    print(f"==> Uploading to Filebase bucket {bucket!r}...")
    cid = upload_file_to_filebase(abs_path, bucket, access_key, secret_key)
    print(f"    CID: {cid}")

    mirrored = False
    if testing_data_repo is not None:
        mirrored = mirror_to_testing_data(abs_path, cid, testing_data_repo)

    cid_path = abs_path.with_name(abs_path.name + ".cid")
    cid_path.write_text(cid + "\n")
    abs_path.unlink()

    update_manifest(cid, rel_path)

    rel_cid = cid_path.relative_to(REPO_ROOT).as_posix()
    print()
    print("==> Upload complete.")
    print(f"    CID:  {cid}")
    print(f"    Link: {cid_path}")
    print()
    print("Next steps (ITK repository):")
    print(f'  git rm "{rel_path}"')
    print(f'  git add "{rel_cid}"')
    print("  git add Testing/Data/content-links.manifest")
    if mirrored and testing_data_repo is not None:
        print()
        print(f"Next steps (ITKTestingData repository at {testing_data_repo}):")
        print(
            f'  git -C "{testing_data_repo}" commit '
            f'-m "Add {abs_path.name} ({cid})"'
        )
        print(f'  git -C "{testing_data_repo}" push')
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
