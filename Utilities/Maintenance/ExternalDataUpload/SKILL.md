---
name: external-data-upload
description: >
  Upload ITK test data to IPFS and produce .cid content links, pin on
  itk-pinata and itk-filebase, optionally mirror into ITKTestingData, and
  normalize existing .md5 / .sha256 / .cid content links. Use when the
  user wants to add test images, baseline data, or model files under
  Testing/Data/ or a module's data/ directory, or when asked to convert
  hash-based content links to CID.
allowed-tools:
  - Bash
  - Read
---

# ITK External Data Upload

Upload a file to IPFS and replace it with a `.cid` content link, maintain the
`Testing/Data/content-links.manifest`, and (optionally) mirror the bytes into
`ITKTestingData` for the GitHub Pages gateway. Also: regenerate existing
`.md5` / `.sha256` / `.cid` content links under the UnixFS v1 2025 profile.

## Prerequisites

The developer must have IPFS and pinning services configured. If not, direct
them to [`README.md`](./README.md) in this directory.

Required:

- IPFS daemon running (`ipfs daemon` or IPFS Desktop)
- UnixFS v1 2025 profile applied (`ipfs config profile apply unixfs-v1-2025`)
- `itk-pinata` remote pinning service configured
- `itk-filebase` remote pinning service configured

## Tasks this skill handles

### 1. Upload a single file

Run the upload script:

```bash
Utilities/Maintenance/ExternalDataUpload/ipfs-upload.sh <filepath>
```

If the user mentions `ITKTestingData` or asks you to mirror the bytes to
GitHub Pages, pass `--testing-data-repo <path>`:

```bash
Utilities/Maintenance/ExternalDataUpload/ipfs-upload.sh \
    --testing-data-repo <path-to-ITKTestingData> \
    <filepath>
```

The script will:

1. Add to IPFS with `--cid-version=1` (UnixFS v1 2025 profile)
2. Pin locally, on `itk-pinata`, and on `itk-filebase`
3. If `--testing-data-repo` given and file ≤ 50 MB, copy to
   `<path>/CID/<cid>` and `git add` it there. Files over 50 MB are skipped
   for the mirror step only (GitHub rejects > 50 MB) — IPFS pinning still
   succeeds.
4. Replace the source file with `<source>.cid`
5. Update `Testing/Data/content-links.manifest`

### 2. Pin every CID from the manifest

```bash
Utilities/Maintenance/ExternalDataUpload/ipfs-pin-all.sh
```

Use for bootstrapping a new IPFS node or re-pinning after rotating a provider.

### 3. Normalize existing content links

Use when the user wants to convert `.md5` / `.sha256` / `.sha512` links to
`.cid`, or re-generate `.cid` links under the UnixFS v1 2025 profile.

```bash
Utilities/Maintenance/ExternalDataUpload/content-link-normalize.sh <path-or-file>
```

Useful options:

- `--dry-run` — report what would change
- `--hash-only` — only touch `.md5` / `.shaNNN` links, leave `.cid` alone
- `--cid-only` — only re-hash existing `.cid` links under the new profile
- `--testing-data-repo <path>` — forwarded to `ipfs-upload.sh`

The normalize script fetches bytes through the gateway templates in
`CMake/ITKExternalData.cmake` (same order as the build), verifies them
against the declared hash or CID, and invokes `ipfs-upload.sh` to produce
the new `.cid`.

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
