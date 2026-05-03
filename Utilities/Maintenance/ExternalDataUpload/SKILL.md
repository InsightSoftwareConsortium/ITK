---
name: external-data-upload
description: >
  Upload ITK test data to Filebase IPFS storage and produce .cid content
  links via the S3 REST API + npx ipfs-car (no Kubo daemon required),
  optionally mirror into ITKTestingData, and normalize existing
  .md5 / .sha256 / .cid content links. Use when the user wants to add
  test images, baseline data, or model files under Testing/Data/ or a
  module's data/ directory, or when asked to convert hash-based content
  links to CID.
allowed-tools:
  - Bash
  - Read
---

# ITK External Data Upload

Upload a file to Filebase IPFS storage and replace it with a `.cid` content
link, maintain `Testing/Data/content-links.manifest`, and (optionally) mirror
the bytes into `ITKTestingData` for the GitHub Pages gateway. Also: regenerate
existing `.md5` / `.sha256` / `.cid` content links under the unixfs-v1-2025
profile.

## Prerequisites

The developer must have the `external-data-upload` pixi environment installed
and Filebase credentials exported. If not, direct them to
[`README.md`](./README.md) in this directory.

Required:

- pixi environment installed: `pixi install -e external-data-upload`
- Filebase IPFS bucket with an S3 access key
- Environment variables exported: `FILEBASE_ACCESS_KEY`,
  `FILEBASE_SECRET_KEY`, `FILEBASE_BUCKET`

A local Kubo daemon, IPFS Desktop, or any `ipfs pin remote` PSA service is
**not** required — the upload talks to Filebase's S3 REST API directly and
relies on `npx ipfs-car` (installed via Node.js in the pixi environment) for
local CAR construction.

## Tasks this skill handles

### 1. Upload a single file

Run the upload script via pixi:

```bash
pixi run -e external-data-upload python \
    Utilities/Maintenance/ExternalDataUpload/upload.py <filepath>
```

If the user mentions `ITKTestingData` or asks you to mirror the bytes to
GitHub Pages, pass `--testing-data-repo <path>`:

```bash
pixi run -e external-data-upload python \
    Utilities/Maintenance/ExternalDataUpload/upload.py \
    --testing-data-repo <path-to-ITKTestingData> \
    <filepath>
```

The script will:

1. Pack the file into a CARv1 with `npx ipfs-car pack --no-wrap`
   (defaults match the unixfs-v1-2025 profile)
2. Upload the CAR to the Filebase IPFS bucket via boto3 with
   `Metadata={"import": "car"}` and verify the CID returned by
   `head_object` matches the local CID
3. If `--testing-data-repo` given and file ≤ 50 MB, copy to
   `<path>/CID/<cid>` and `git add` it there. Files over 50 MB are skipped
   for the mirror step only (GitHub rejects > 50 MB) — Filebase pinning still
   succeeds.
4. Replace the source file with `<source>.cid`
5. Update `Testing/Data/content-links.manifest`

### 2. Normalize existing content links

Use when the user wants to convert `.md5` / `.sha256` / `.sha512` links to
`.cid`, or re-generate `.cid` links under the unixfs-v1-2025 profile.

```bash
pixi run -e external-data-upload python \
    Utilities/Maintenance/ExternalDataUpload/normalize.py <path-or-file>
```

Useful options:

- `--dry-run` — report what would change
- `--hash-only` — only touch `.md5` / `.shaNNN` links, leave `.cid` alone
- `--cid-only` — only re-hash existing `.cid` links under the new profile
- `--testing-data-repo <path>` — forwarded to the upload helper
- `--bucket <name>` — Filebase bucket override (default: `$FILEBASE_BUCKET`)

The normalize script fetches bytes through the gateway templates in
`CMake/ITKExternalData.cmake` (same order as the build), verifies them
against the declared hash or CID, and calls `upload.upload_file_to_filebase`
to produce the new `.cid`.

## After Upload

Stage the git changes the upload script prints. Typical ITK workflow:

```bash
git rm <original-file>
git add <original-file>.cid
git add Testing/Data/content-links.manifest
```

If `--testing-data-repo` was used, follow the printed commands in that repo:

```bash
git -C <ITKTestingData> commit -m "Add <file> (<cid>)"
git -C <ITKTestingData> push
```

Commit the ITK changes with an appropriate prefix per
[`Documentation/AI/git-commits.md`](../../../Documentation/AI/git-commits.md):

- `ENH:` for new test data
- `STYLE:` for normalizing existing content links (no test semantics change)

## How `.cid` Files Work

A `.cid` file is one line of plain text: a CIDv1, base32-encoded. ITK's
`CMake/ITKExternalData.cmake` recognises the `.cid` extension and fetches
through the gateway list declared there (local Kubo, `ipfs.io`,
`gateway.pinata.cloud`, `cloudflare-ipfs.com`, `dweb.link`, plus the
`ITKTestingData` GitHub Pages mirror at
`insightsoftwareconsortium.github.io/ITKTestingData/CID/<cid>`).

Because CIDs are content-addressed, integrity is verified automatically at
fetch time.
