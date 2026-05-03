# ITK External Data Upload

Upload large test images and baselines to [Filebase] IPFS storage, optionally
mirror them into the
[`ITKTestingData`](https://github.com/InsightSoftwareConsortium/ITKTestingData)
repository, and replace the original with a lightweight `.cid` content link
committed to the ITK source tree.

This complements [`CMake/ITKExternalData.cmake`](../../../CMake/ITKExternalData.cmake),
which fetches content at test configure time from the gateways listed there
(`ITKTestingData` on GitHub Pages, `data.kitware.com`, `itk.org`, local Kubo
gateway, `ipfs.io`, `gateway.pinata.cloud`, `cloudflare-ipfs.com`,
`dweb.link`).

## How the upload works

Uploads go directly to a Filebase IPFS bucket over Filebase's
S3-compatible REST API. A local
[Kubo](https://github.com/ipfs/kubo) daemon, IPFS Desktop, or any
configured `ipfs pin remote` PSA service is **not** required.

For each upload the helper script:

1. Packs the file into a CARv1 with `npx ipfs-car pack --no-wrap`. ipfs-car
   v1+ defaults to **1 MiB chunks, 1024 children per node, raw leaves,
   CIDv1**, which is exactly the [unixfs-v1-2025] / IPIP-0499 profile, so
   the CID is reproducible across implementations and matches what other
   contributors and CI compute for the same content.
2. PUTs the CAR to the configured Filebase bucket with the
   `x-amz-meta-import: car` header. Filebase imports the CAR and pins the
   resulting CID server-side, exposing it via `head_object` metadata.
3. Reads the CID back from `head_object` and verifies it matches the local
   CID. A mismatch aborts the upload.
4. Writes `<file>.cid`, removes the original file, appends/updates an entry
   in `Testing/Data/content-links.manifest`, and (with
   `--testing-data-repo`) copies the bytes into a local `ITKTestingData`
   clone for the GitHub Pages CDN mirror.

[unixfs-v1-2025]: https://github.com/ipfs/specs/blob/main/IPIP/0499-unixfs-v1-2025-profile.md

## One-Time Developer Setup

### 1. Install the pixi environment

The upload helpers run on top of a small pixi environment that brings in
[boto3] for the Filebase S3 calls, Node.js for `npx ipfs-car`, and
`requests` for the gateway-fetch verification path used by `normalize.py`.
From the ITK source tree:

```bash
pixi install -e external-data-upload
```

[boto3]: https://boto3.amazonaws.com/

That installs everything into `.pixi/envs/external-data-upload/`. Verify:

```bash
pixi run -e external-data-upload python --version
pixi run -e external-data-upload node --version
pixi run -e external-data-upload npx --yes ipfs-car --version
```

The first `npx ipfs-car` invocation downloads the package into the npm
cache; subsequent runs are offline.

### 2. Create a Filebase IPFS bucket and S3 keys

1. Sign up at <https://console.filebase.com> (the free tier supports
   pin-by-CID via the S3 import path).
2. Create an **IPFS bucket** at <https://console.filebase.com/buckets>.
   The bucket name is local to your account — the published CID is the
   only thing other contributors need to retrieve the bytes.
3. Create an S3 access key for that bucket at
   <https://console.filebase.com/keys>. Filebase ties keys to a single
   bucket, so the access key + secret you receive can only see and
   write to that bucket.

### 3. Export the credentials

The helper scripts read three environment variables:

```bash
export FILEBASE_ACCESS_KEY=...      # S3 access key
export FILEBASE_SECRET_KEY=...      # S3 secret key
export FILEBASE_BUCKET=itk-data     # bucket name from step 2
```

Add the exports to your shell profile or a `.env` file you source before
uploads. **Do not** commit credentials to the repository.

## Usage

### Upload a single file

```bash
pixi run -e external-data-upload python \
    Utilities/Maintenance/ExternalDataUpload/upload.py <filepath>
```

The script will:

1. Pack the file into a CAR (CIDv1, unixfs-v1-2025 profile)
2. Upload the CAR to your Filebase IPFS bucket and verify the CID
3. Replace the original file with `<filepath>.cid` containing the CID
4. Append/update an entry in `Testing/Data/content-links.manifest`
5. Print the `git rm` / `git add` commands to stage the change

### Also mirror the bytes to `ITKTestingData`

Pass `--testing-data-repo <path>` to additionally copy the file into a local
clone of
[`ITKTestingData`](https://github.com/InsightSoftwareConsortium/ITKTestingData)
at `CID/<cid-value>` and `git add` it there. This populates the
`https://insightsoftwareconsortium.github.io/ITKTestingData/CID/<cid>` mirror
gateway already listed in `CMake/ITKExternalData.cmake`.

```bash
pixi run -e external-data-upload python \
    Utilities/Maintenance/ExternalDataUpload/upload.py \
    --testing-data-repo ~/src/ITKTestingData \
    Testing/Data/Input/brain.nii.gz
```

**GitHub 50 MB file size limit.** `ITKTestingData` is hosted on GitHub, which
hard-rejects pushes containing files larger than **50 MB** per file. The upload
script checks the file size before mirroring and refuses to copy files over
50 MB into the `ITKTestingData` tree. The Filebase upload still proceeds for
oversized files — the mirror step is the only one that gets skipped, with a
clear warning.

Commit the staged `CID/<cid>` file in `ITKTestingData` and push; the
`gh-pages` workflow on that repo republishes the new file at the GitHub Pages
mirror gateway.

### Normalize existing content links to CID

`.md5` / `.sha256` / `.sha512` content links can be converted to `.cid`, and
existing `.cid` links can be regenerated under the unixfs-v1-2025 profile (in
case they were originally produced with older chunker defaults).

```bash
pixi run -e external-data-upload python \
    Utilities/Maintenance/ExternalDataUpload/normalize.py <path-or-file>
```

The script will, for each content link under the given path:

1. Fetch the bytes through the gateways in `CMake/ITKExternalData.cmake`
   (same order the build uses, so a gateway CI can't reach is a gateway
   this script won't accept).
2. Verify the fetched bytes against the declared hash (for `.md5` / `.shaNNN`
   links) or the declared CID (for `.cid` links — accepted only when fetched
   via an IPFS HTTP gateway, which verifies server-side).
3. Re-materialize the actual file next to the content link, then call the
   Filebase uploader so the new CID is produced under the unixfs-v1-2025
   profile and (if `--testing-data-repo` is passed) mirrored into
   `ITKTestingData`. The old `.md5` / `.sha256` / `.sha512` link is
   removed; a `.cid` link is written in its place.

Common options:

```bash
# Dry run — report what would change, modify nothing.
pixi run -e external-data-upload python \
    Utilities/Maintenance/ExternalDataUpload/normalize.py Modules/Filtering/Foo --dry-run

# Also mirror bytes into a local ITKTestingData checkout.
pixi run -e external-data-upload python \
    Utilities/Maintenance/ExternalDataUpload/normalize.py Testing/Data/Input \
    --testing-data-repo ~/src/ITKTestingData

# Only process files that are currently .md5 / .shaNNN (skip existing .cid).
pixi run -e external-data-upload python \
    Utilities/Maintenance/ExternalDataUpload/normalize.py Modules --hash-only
```

## Content Link Manifest

`Testing/Data/content-links.manifest` is a plain-text index of every CID the
upload script has produced. One entry per line:

```text
<cid> <filepath>
```

Example:

```text
bafybeigdyrzt5sfp7udm7hu76uh7y26nf3efuylqabf3oclgtqy55fbzdi Testing/Data/Input/brain.nii.gz
bafkreihvlpx2z3xyhmhegrqo6vn4balcm3gkskdigoyl3i5v7iq5mhtaee Testing/Data/Baseline/Filtering/brain-diff.mha
```

Rules:

- `<filepath>` is a repo-relative path and **must not contain whitespace** —
  the manifest uses a single space as the field delimiter. Rename files with
  spaces before uploading.
- `upload.py` maintains this file automatically: entries are added on first
  upload and replaced on re-upload. The data lines are sorted by path for a
  minimal review diff; comment lines at the top are preserved.
- The manifest should be committed alongside the `.cid` files the upload
  produced.

## How `.cid` Files Work

A `.cid` file is a single-line plain-text file containing one IPFS CIDv1,
base32-encoded. Example:

```text
bafybeigdyrzt5sfp7udm7hu76uh7y26nf3efuylqabf3oclgtqy55fbzdi
```

ITK's CMake layer recognises the `.cid` extension via
`ExternalData_URL_ALGO_CID_lower` in
[`CMake/ITKExternalData.cmake`](../../../CMake/ITKExternalData.cmake). At
configure time, `ExternalData.cmake` substitutes the CID into each
`ExternalData_URL_TEMPLATES` entry (local Kubo gateway, `ipfs.io`,
`gateway.pinata.cloud`, `cloudflare-ipfs.com`, `dweb.link`, plus the
`ITKTestingData` GitHub Pages mirror) and downloads from the first one that
responds. The downloaded content is cached in
`ExternalData_OBJECT_STORES` under `cid/<cid>`.

Because CIDs are content-addressed, a corrupt download is detected
automatically: a gateway that returns the wrong bytes will produce a different
CID, and the cache lookup misses.

## Troubleshooting

### `ERROR: 'npx' not found on PATH`

The pixi environment is not active. Run the helpers via `pixi run -e
external-data-upload python ...`, or activate the environment first with
`pixi shell -e external-data-upload`.

### `ERROR: Missing Filebase credentials`

Export `FILEBASE_ACCESS_KEY`, `FILEBASE_SECRET_KEY`, and `FILEBASE_BUCKET`
(or pass `--bucket`) before running the upload script. See setup step 3.

### `Filebase did not return a CID for ...`

The CAR was uploaded but Filebase did not import it. Common causes:

- The bucket is a regular S3 bucket, not an **IPFS** bucket — recreate at
  <https://console.filebase.com/buckets>.
- The S3 access key is read-only or scoped to a different bucket.
- Filebase rate-limited the request — retry after a few seconds.

### `CID mismatch: local=... filebase=...`

The CID this client computed (via `npx ipfs-car`) and the CID Filebase
reported after import disagree. This indicates a chunker/profile drift
between the local ipfs-car version and Filebase's importer. Confirm
`pixi run -e external-data-upload npx ipfs-car --version` is v1 or newer,
then retry; if the mismatch persists, file an issue and include both CIDs
in the report.
