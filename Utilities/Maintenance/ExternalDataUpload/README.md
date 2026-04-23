# ITK External Data Upload

Upload large test images and baselines to IPFS, optionally mirror them into the
[`ITKTestingData`](https://github.com/InsightSoftwareConsortium/ITKTestingData)
repository, and replace the original with a lightweight `.cid` content link
committed to the ITK source tree.

This complements [`CMake/ITKExternalData.cmake`](../../../CMake/ITKExternalData.cmake),
which fetches content at test configure time from the gateways listed there
(`ITKTestingData` on GitHub Pages, `data.kitware.com`, `itk.org`, local Kubo
gateway, `ipfs.io`, `gateway.pinata.cloud`, `cloudflare-ipfs.com`,
`dweb.link`).

## One-Time Developer Setup

### 1. Install Kubo (IPFS)

You need the Kubo IPFS implementation. Choose one method:

**IPFS Desktop** (recommended — bundles the Kubo daemon with a GUI, with a
system-tray icon, peer/bandwidth statistics, a file browser for your MFS, and
one-click start/stop):

Download from <https://docs.ipfs.tech/install/ipfs-desktop/>. IPFS Desktop
auto-starts the daemon on login and exposes the same HTTP API that the `ipfs`
CLI uses (default `127.0.0.1:5001`), so every command in this guide works
identically whether you started the daemon from the command line or from the
tray.

**CLI only** (macOS):

```bash
brew install ipfs
```

**CLI only** (Linux):

Download the latest release from <https://dist.ipfs.tech/#kubo>, then:

```bash
tar xvfz kubo_*_linux-amd64.tar.gz
cd kubo && sudo bash install.sh
```

After installation, verify `ipfs` is on your PATH:

```bash
ipfs --version
```

### 2. Initialize and Start the Daemon

```bash
# One-time initialization (creates ~/.ipfs)
ipfs init

# Start the daemon (keep running in a separate terminal, or use IPFS Desktop)
ipfs daemon
```

### 2a. Apply the UnixFS v1 2025 Profile

Requires **Kubo v0.40.0 or later**. Apply once per node, before your first
upload:

```bash
ipfs config profile apply unixfs-v1-2025
```

This pins the UnixFS importer settings (chunker, layout, raw-leaves, HAMT
directory thresholds) to standardized values for reproducible CIDs. Without it,
`ipfs add` defaults may drift across Kubo patch releases and across
implementations (Helia, rust-ipfs, boxo), so two contributors uploading the
same file can produce different CIDs — which breaks the `.cid` content-link
contract ITK relies on.

The profile applies to **new adds only**; existing pinned content and
already-committed `.cid` files are unaffected.

References:

- [Kubo v0.40.0 release notes](https://github.com/ipfs/kubo/releases/tag/v0.40.0)
- [Reproducible CIDs — IPFS blog, March 2026](https://blog.ipfs.tech/2026-03-reproducible-cids/)

### 3. Configure Remote Pinning Services

The upload script pins content on two remote services for redundancy, matching
the gateways declared in `CMake/ITKExternalData.cmake`. Both services must be
configured under the **exact names `itk-pinata` and `itk-filebase`** — the
upload script looks up those names and fails if they are missing.

#### Pinata (service name: `itk-pinata`)

1. Sign up at <https://pinata.cloud>
2. Create an API key at <https://app.pinata.cloud/developers/api-keys>
   - Enable **pinByHash** and **pinFileToIPFS** permissions
3. Copy the JWT token and add the service (use a prompt to avoid leaking
   the token into shell history):

```bash
printf "Pinata JWT: " && read -rs PINATA_JWT && echo
ipfs pin remote service add itk-pinata https://api.pinata.cloud/psa "$PINATA_JWT"
```

4. Verify:

```bash
ipfs pin remote service ls
# Should show: itk-pinata https://api.pinata.cloud/psa
```

#### Filebase (service name: `itk-filebase`)

1. Sign up at <https://console.filebase.com>
2. Create an **IPFS bucket** at <https://console.filebase.com/buckets>
3. Go to <https://console.filebase.com/keys>, select your IPFS bucket in the
   "IPFS Pinning Service API Endpoint" section, and copy the generated token
4. Add the service:

```bash
printf "Filebase token: " && read -rs FILEBASE_TOKEN && echo
ipfs pin remote service add itk-filebase https://api.filebase.io/v1/ipfs "$FILEBASE_TOKEN"
```

5. Verify:

```bash
ipfs pin remote service ls
# Should show: itk-filebase https://api.filebase.io/v1/ipfs
```

## Usage

### Upload a single file

```bash
Utilities/Maintenance/ExternalDataUpload/ipfs-upload.sh <filepath>
```

The script will:

1. Add the file to IPFS with `--cid-version=1` (UnixFS v1 2025 profile)
2. Pin it locally
3. Pin it on `itk-pinata` and `itk-filebase`
4. Replace the original file with `<filepath>.cid` containing the CID
5. Append/update an entry in `Testing/Data/content-links.manifest`
6. Print the `git rm` / `git add` commands to stage the change

### Also mirror the bytes to `ITKTestingData`

Pass `--testing-data-repo <path>` to additionally copy the file into a local
clone of
[`ITKTestingData`](https://github.com/InsightSoftwareConsortium/ITKTestingData)
at `CID/<cid-value>` and `git add` it there. This populates the
`https://insightsoftwareconsortium.github.io/ITKTestingData/CID/<cid>` mirror
gateway already listed in `CMake/ITKExternalData.cmake`.

```bash
Utilities/Maintenance/ExternalDataUpload/ipfs-upload.sh \
    --testing-data-repo ~/src/ITKTestingData \
    Testing/Data/Input/brain.nii.gz
```

**GitHub 50 MB file size limit.** `ITKTestingData` is hosted on GitHub, which
hard-rejects pushes containing files larger than **50 MB** per file. The upload
script checks the file size before mirroring and refuses to copy files over
50 MB into the `ITKTestingData` tree. IPFS pinning (local + `itk-pinata` +
`itk-filebase`) still proceeds for oversized files — the mirror step is the
only one that gets skipped, with a clear warning.

Commit the staged `CID/<cid>` file in `ITKTestingData` and push; the
`gh-pages` workflow on that repo republishes the new file at the GitHub Pages
mirror gateway.

### Batch-pin every CID in the manifest

```bash
Utilities/Maintenance/ExternalDataUpload/ipfs-pin-all.sh
```

Reads `Testing/Data/content-links.manifest` and pins every CID locally plus on
every configured remote pinning service. Useful for:

- Bootstrapping a new local Kubo node with all ITK test content
- Re-pinning everything after rotating a pinning provider
- Verifying all pinned content is still reachable

Use `--background` to queue remote pins asynchronously (the remote services
then fetch the content themselves):

```bash
Utilities/Maintenance/ExternalDataUpload/ipfs-pin-all.sh --background
```

### Normalize existing content links to CID

`.md5` / `.sha256` / `.sha512` content links can be converted to `.cid`, and
existing `.cid` links can be regenerated under the UnixFS v1 2025 profile (in
case they were originally produced with older chunker defaults).

```bash
Utilities/Maintenance/ExternalDataUpload/content-link-normalize.sh <path-or-file>
```

The script will, for each content link under the given path:

1. Fetch the bytes through the gateways in `CMake/ITKExternalData.cmake` (same
   order the build uses, so a gateway CI can't reach is a gateway this script
   won't accept).
2. Verify the fetched bytes against the declared hash (for `.md5` / `.shaNNN`
   links) or the declared CID (for `.cid` links). If verification fails the
   link is left untouched and reported.
3. Re-materialize the actual file next to the content link, then invoke
   `ipfs-upload.sh` on it so the new CID is produced under the UnixFS v1 2025
   profile, pinned locally and on `itk-pinata` / `itk-filebase`, and (if
   `--testing-data-repo` is passed) mirrored into `ITKTestingData`. The old
   `.md5` / `.sha256` / `.sha512` link is removed; a `.cid` link is written in
   its place.

Common options:

```bash
# Dry run — report what would change, modify nothing.
content-link-normalize.sh Modules/Filtering/Foo --dry-run

# Also mirror bytes into a local ITKTestingData checkout.
content-link-normalize.sh Testing/Data/Input --testing-data-repo ~/src/ITKTestingData

# Only process files that are currently .md5 / .shaNNN (skip existing .cid).
content-link-normalize.sh Modules --hash-only
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
- `ipfs-upload.sh` maintains this file automatically: entries are added on
  first upload and replaced on re-upload. The data lines are sorted by path
  for a minimal review diff; comment lines at the top are preserved.
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

### `ipfs command not found on PATH`

Install Kubo (see step 1 above). If using IPFS Desktop on macOS, the app
installs `/usr/local/bin/ipfs` automatically; on Linux, IPFS Desktop does not
install a CLI symlink, so either add Kubo separately or point your shell at
the bundled binary inside the AppImage.

### `IPFS daemon does not appear to be running`

Start the daemon: `ipfs daemon` in a separate terminal, or launch IPFS
Desktop. The script tests the connection with `ipfs swarm peers`, which
requires an active daemon.

### `Required pinning service 'itk-pinata' is not configured`

Run `ipfs pin remote service ls` to see configured services. Re-add with the
commands in step 3 above. Tokens may have expired if you revoked the API key.
The script intentionally refuses to upload if either `itk-pinata` or
`itk-filebase` is missing: a single pin provider is not enough redundancy for
test data CI relies on.

### Remote pin failed

The script prints retry commands for any failed pins. Common causes:

- **Expired API token** — regenerate at the service dashboard
- **Rate limiting** — wait a moment and retry
- **Large file timeout** — the file may take time to transfer; retry the
  printed `ipfs pin remote add` command manually
