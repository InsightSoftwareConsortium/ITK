#!/usr/bin/env bash
set -euo pipefail

# Upload a file to IPFS (CIDv1, UnixFS v1 2025 profile), pin it on the
# itk-pinata and itk-filebase remote pinning services, and replace the
# original with a .cid content link. Optionally mirror the bytes into a
# local ITKTestingData checkout at CID/<cid-value>.
#
# Usage:
#   ipfs-upload.sh [--testing-data-repo <path>] [--background] <filepath>
#
# Options:
#   --testing-data-repo <path>  Path to a local clone of
#                               https://github.com/InsightSoftwareConsortium/ITKTestingData
#                               The uploaded bytes are copied to
#                               <path>/CID/<cid-value> and `git add`ed there.
#                               Skipped with a warning for files > 50 MB,
#                               which GitHub rejects.
#   --background                Submit remote pin requests asynchronously
#                               (pins queue at itk-pinata / itk-filebase and
#                               the script returns without waiting). Useful
#                               for batch workflows. Default is synchronous,
#                               which blocks until each remote reports
#                               'pinned' — safer for one-off uploads because
#                               failures surface immediately, but can take
#                               minutes per file as the remote fetches the
#                               content.
#
# Prerequisites:
#   - Kubo (go-ipfs) installed and `ipfs` on PATH
#   - IPFS daemon running (ipfs daemon, or IPFS Desktop)
#   - UnixFS v1 2025 profile applied: `ipfs config profile apply unixfs-v1-2025`
#   - `itk-pinata` and `itk-filebase` remote pinning services configured
#
# See README.md in this directory for full setup.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

# Required remote pinning services — script errors if not configured.
REQUIRED_SERVICES=(itk-pinata itk-filebase)

# GitHub hard-rejects pushes containing any file > 50 MB. The ITKTestingData
# mirror step is skipped for files over this limit.
GITHUB_FILE_LIMIT_BYTES=$((50 * 1024 * 1024))

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

TESTING_DATA_REPO=""
BACKGROUND=""
FILE=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --testing-data-repo)
            TESTING_DATA_REPO="${2:?--testing-data-repo requires a path}"
            shift 2
            ;;
        --testing-data-repo=*)
            TESTING_DATA_REPO="${1#--testing-data-repo=}"
            shift
            ;;
        --background)
            BACKGROUND="--background"
            shift
            ;;
        -h|--help)
            sed -n '3,/^$/{ s/^# \?//; p }' "$0"
            exit 0
            ;;
        -*)
            echo "ERROR: Unknown option: $1" >&2
            exit 1
            ;;
        *)
            if [[ -n "$FILE" ]]; then
                echo "ERROR: Unexpected positional argument: $1" >&2
                exit 1
            fi
            FILE="$1"
            shift
            ;;
    esac
done

if [[ -z "$FILE" ]]; then
    echo "Usage: $0 [--testing-data-repo <path>] <filepath>" >&2
    exit 1
fi

if [[ ! -f "$FILE" ]]; then
    echo "ERROR: File not found: $FILE" >&2
    exit 1
fi

# Guard: reject symlinks (realpath would resolve to the target, and rm would
# delete the target file rather than the symlink itself).
if [[ -L "$FILE" ]]; then
    echo "ERROR: Symlink paths are not supported: $FILE" >&2
    echo "       Pass the real file path instead." >&2
    exit 1
fi

ABSOLUTE_FILE="$(realpath "$FILE")"

# Guard: file must be inside the repository.
if [[ "$ABSOLUTE_FILE" != "$REPO_ROOT"/* ]]; then
    echo "ERROR: File must be inside the repository: $ABSOLUTE_FILE" >&2
    exit 1
fi

# Guard: reject files that are already content links.
for ext in cid md5 sha1 sha224 sha256 sha384 sha512; do
    if [[ "$FILE" == *."${ext}" ]]; then
        echo "ERROR: File is already a .${ext} content link: $FILE" >&2
        exit 1
    fi
done

REL_FILE="${ABSOLUTE_FILE#"$REPO_ROOT/"}"

# Guard: reject paths with whitespace (manifest format uses space as delimiter).
if [[ "$REL_FILE" =~ [[:space:]] ]]; then
    echo "ERROR: Filepath contains whitespace, which is not supported: $REL_FILE" >&2
    echo "       Rename the file to remove spaces before uploading." >&2
    exit 1
fi

PIN_NAME="$(basename "$ABSOLUTE_FILE")"

# ---------------------------------------------------------------------------
# Validate --testing-data-repo path (before any IPFS work)
# ---------------------------------------------------------------------------

if [[ -n "$TESTING_DATA_REPO" ]]; then
    if [[ ! -d "$TESTING_DATA_REPO" ]]; then
        echo "ERROR: --testing-data-repo path is not a directory: $TESTING_DATA_REPO" >&2
        exit 1
    fi
    if [[ ! -d "$TESTING_DATA_REPO/.git" ]]; then
        echo "ERROR: --testing-data-repo is not a git checkout: $TESTING_DATA_REPO" >&2
        exit 1
    fi
    TESTING_DATA_REPO="$(realpath "$TESTING_DATA_REPO")"
fi

# ---------------------------------------------------------------------------
# Prerequisites
# ---------------------------------------------------------------------------

if ! command -v ipfs &>/dev/null; then
    echo "ERROR: 'ipfs' command not found on PATH." >&2
    echo "       Install Kubo: https://docs.ipfs.tech/install/command-line/" >&2
    echo "       See: Utilities/Maintenance/ExternalDataUpload/README.md" >&2
    exit 1
fi

if ! ipfs swarm peers &>/dev/null; then
    echo "ERROR: IPFS daemon does not appear to be running." >&2
    echo "       Start with: ipfs daemon" >&2
    echo "       Or launch IPFS Desktop." >&2
    exit 1
fi

# Check required remote pinning services are configured.
CONFIGURED_SERVICES="$(ipfs pin remote service ls 2>/dev/null || true)"
for svc in "${REQUIRED_SERVICES[@]}"; do
    if ! echo "$CONFIGURED_SERVICES" | grep -q "^${svc} "; then
        echo "ERROR: Required pinning service '${svc}' is not configured." >&2
        echo "       See: Utilities/Maintenance/ExternalDataUpload/README.md" >&2
        exit 1
    fi
done

# ---------------------------------------------------------------------------
# Add to IPFS
# ---------------------------------------------------------------------------

echo "==> Adding ${PIN_NAME} to IPFS (CIDv1, UnixFS v1 2025 profile)..."
CID="$(ipfs add --cid-version=1 --quieter "$ABSOLUTE_FILE")"

if [[ -z "$CID" ]]; then
    echo "ERROR: ipfs add returned an empty CID." >&2
    exit 1
fi

echo "    CID: ${CID}"

# ---------------------------------------------------------------------------
# Pin locally (ipfs add already pins, but be explicit)
# ---------------------------------------------------------------------------

echo "==> Pinning locally..."
ipfs pin add "$CID" >/dev/null

# ---------------------------------------------------------------------------
# Pin on remote services
# ---------------------------------------------------------------------------

FAILED_PINS=()

for svc in "${REQUIRED_SERVICES[@]}"; do
    # Skip services where this CID is already queued/pinning/pinned —
    # Pinata rejects duplicate `pin remote add` calls with
    # DUPLICATE_OBJECT (400), and resubmitting on Filebase just makes a
    # second queue entry.
    if ipfs pin remote ls --service="$svc" --cid="$CID" \
           --status=queued,pinning,pinned 2>/dev/null | grep -q .; then
        echo "==> Already pinned (or in flight) on ${svc}; skipping"
        continue
    fi

    if [[ -n "$BACKGROUND" ]]; then
        echo "==> Queueing pin on ${svc} (background)..."
    else
        echo "==> Pinning on ${svc}..."
    fi
    if ipfs pin remote add --service="$svc" --name="$PIN_NAME" $BACKGROUND "$CID" 2>&1; then
        echo "    OK: ${svc}"
    else
        echo "    FAILED: ${svc}" >&2
        FAILED_PINS+=("$svc")
    fi
done

if [[ ${#FAILED_PINS[@]} -gt 0 ]]; then
    echo "" >&2
    echo "ERROR: Remote pin submission failed for: ${FAILED_PINS[*]}" >&2
    echo "       The original file has NOT been modified." >&2
    echo "       Fix the issue and retry, or pin manually:" >&2
    for failed_svc in "${FAILED_PINS[@]}"; do
        echo "         ipfs pin remote add --service=${failed_svc} --name=\"${PIN_NAME}\" ${BACKGROUND} ${CID}" >&2
    done
    exit 1
fi

# ---------------------------------------------------------------------------
# Mirror into ITKTestingData (optional, size-gated)
# ---------------------------------------------------------------------------

FILE_SIZE_BYTES="$(stat -c '%s' "$ABSOLUTE_FILE" 2>/dev/null || stat -f '%z' "$ABSOLUTE_FILE")"

if [[ -n "$TESTING_DATA_REPO" ]]; then
    if (( FILE_SIZE_BYTES > GITHUB_FILE_LIMIT_BYTES )); then
        echo "" >&2
        echo "WARNING: ${PIN_NAME} is ${FILE_SIZE_BYTES} bytes (> 50 MB)." >&2
        echo "         GitHub rejects pushes containing files > 50 MB, so it" >&2
        echo "         will NOT be mirrored to ITKTestingData." >&2
        echo "         IPFS pin (local + itk-pinata + itk-filebase) succeeded;" >&2
        echo "         the .cid content link will still be produced." >&2
    else
        MIRROR_DIR="$TESTING_DATA_REPO/CID"
        MIRROR_PATH="$MIRROR_DIR/$CID"
        mkdir -p "$MIRROR_DIR"
        echo "==> Mirroring to ITKTestingData: CID/${CID}"
        cp "$ABSOLUTE_FILE" "$MIRROR_PATH"
        if ! git -C "$TESTING_DATA_REPO" add "CID/$CID"; then
            echo "ERROR: Failed to 'git add CID/$CID' in $TESTING_DATA_REPO" >&2
            rm -f "$MIRROR_PATH"
            exit 1
        fi
    fi
fi

# ---------------------------------------------------------------------------
# Replace original file with .cid content link
# (only reached after all required remote pins succeeded)
#
# Ordering hazard: the .cid file is written and the original data file is
# removed BEFORE the manifest update below. If the process is killed or hits
# a disk-full error between here and the `mv` of "${MANIFEST}.tmp", the
# original is gone, the .cid link exists, but the manifest is not updated.
# content-link-normalize.sh's recovery block restores the original link and
# also removes any orphan .cid in that case. A standalone `ipfs-upload.sh`
# crash here leaves the working tree consistent (CID file present, original
# absent) but the manifest stale; the user can re-run after repairing.
# ---------------------------------------------------------------------------

CID_FILE="${ABSOLUTE_FILE}.cid"
REL_CID="${CID_FILE#"$REPO_ROOT/"}"
printf '%s\n' "$CID" > "$CID_FILE"
rm "$ABSOLUTE_FILE"

# ---------------------------------------------------------------------------
# Update content link manifest
# ---------------------------------------------------------------------------

MANIFEST="$REPO_ROOT/Testing/Data/content-links.manifest"

if [[ -f "$MANIFEST" ]]; then
    # Remove existing entry for this filepath (re-upload case).
    # Use awk for exact string match (grep would treat dots as wildcards).
    awk -v path="$REL_FILE" '$2 != path' "$MANIFEST" > "${MANIFEST}.tmp"
    mv "${MANIFEST}.tmp" "$MANIFEST"
else
    # Seed a fresh manifest with a brief header.
    cat > "$MANIFEST" <<'EOF'
# ITK content-link manifest
# One CID per line, format: <cid> <repo-relative-path>
# Maintained by Utilities/Maintenance/ExternalDataUpload/ipfs-upload.sh
EOF
fi

# Append the new entry.
printf '%s %s\n' "$CID" "$REL_FILE" >> "$MANIFEST"

# Sort data lines by filepath; preserve comment header at top.
{
    grep '^#' "$MANIFEST" || true
    grep -v '^#' "$MANIFEST" | grep -v '^$' | LC_ALL=C sort -k2
} > "${MANIFEST}.tmp"
mv "${MANIFEST}.tmp" "$MANIFEST"

echo ""
echo "==> Upload complete."
echo "    CID:  ${CID}"
echo "    Link: ${CID_FILE}"

# ---------------------------------------------------------------------------
# Suggest git commands
# ---------------------------------------------------------------------------

echo ""
echo "Next steps (ITK repository):"
echo "  git rm \"${REL_FILE}\""
echo "  git add \"${REL_CID}\""
echo "  git add Testing/Data/content-links.manifest"

if [[ -n "$TESTING_DATA_REPO" && $FILE_SIZE_BYTES -le $GITHUB_FILE_LIMIT_BYTES ]]; then
    echo ""
    echo "Next steps (ITKTestingData repository at ${TESTING_DATA_REPO}):"
    echo "  git -C \"${TESTING_DATA_REPO}\" commit -m \"Add ${PIN_NAME} (${CID})\""
    echo "  git -C \"${TESTING_DATA_REPO}\" push"
fi
