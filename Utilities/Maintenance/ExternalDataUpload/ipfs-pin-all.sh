#!/usr/bin/env bash
set -euo pipefail

# Batch-pin every CID in Testing/Data/content-links.manifest locally and on
# every configured remote pinning service (itk-pinata, itk-filebase, ...).
#
# Usage: ipfs-pin-all.sh [--background]
#
# Options:
#   --background   Queue remote pins asynchronously (faster, no wait).

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
MANIFEST="$REPO_ROOT/Testing/Data/content-links.manifest"

BACKGROUND=""
while [[ $# -gt 0 ]]; do
    case "$1" in
        --background)
            BACKGROUND="--background"
            shift
            ;;
        -h|--help)
            sed -n '4,11p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *)
            echo "ERROR: Unknown argument: $1" >&2
            echo "Usage: $(basename "${BASH_SOURCE[0]}") [--background]" >&2
            exit 2
            ;;
    esac
done

# ---------------------------------------------------------------------------
# Validate manifest
# ---------------------------------------------------------------------------

if [[ ! -f "$MANIFEST" ]]; then
    echo "ERROR: Manifest not found: $MANIFEST" >&2
    exit 1
fi

ENTRY_COUNT="$(grep -Evc '^(#|$)' "$MANIFEST" || true)"
if [[ "$ENTRY_COUNT" -eq 0 ]]; then
    echo "Manifest is empty — nothing to pin."
    exit 0
fi

# ---------------------------------------------------------------------------
# Prerequisites
# ---------------------------------------------------------------------------

if ! command -v ipfs &>/dev/null; then
    echo "ERROR: 'ipfs' command not found on PATH." >&2
    echo "       Install Kubo: https://docs.ipfs.tech/install/command-line/" >&2
    exit 1
fi

if ! ipfs swarm peers &>/dev/null; then
    echo "ERROR: IPFS daemon does not appear to be running." >&2
    echo "       Start with: ipfs daemon" >&2
    exit 1
fi

# Discover configured remote services (none required for batch pinning).
CONFIGURED_SERVICES="$(ipfs pin remote service ls 2>/dev/null || true)"
SERVICES=()
while IFS= read -r line; do
    svc="$(echo "$line" | awk '{print $1}')"
    if [[ -n "$svc" ]]; then
        SERVICES+=("$svc")
    fi
done <<< "$CONFIGURED_SERVICES"

if [[ ${#SERVICES[@]} -eq 0 ]]; then
    echo "WARNING: No remote pinning services configured." >&2
    echo "         Only local pinning will be performed." >&2
fi

# ---------------------------------------------------------------------------
# Pin each CID
# ---------------------------------------------------------------------------

TOTAL=0
LOCAL_FAILED=0
LOCAL_FAILED_ENTRIES=()
REMOTE_FAILED=0
REMOTE_FAILED_ENTRIES=()

echo "==> Pinning ${ENTRY_COUNT} CIDs from manifest..."
if [[ -n "$BACKGROUND" ]]; then
    echo "    (remote pins queued in background)"
fi
echo ""

while IFS= read -r line; do
    # Skip comments and empty lines.
    [[ "$line" =~ ^# ]] && continue
    [[ -z "$line" ]] && continue

    CID="$(echo "$line" | awk '{print $1}')"
    FILEPATH="$(echo "$line" | awk '{print $2}')"

    # Skip malformed lines (missing CID or filepath).
    if [[ -z "$CID" || -z "$FILEPATH" ]]; then
        echo "WARNING: Skipping malformed manifest line: $line" >&2
        continue
    fi

    PIN_NAME="$(basename "$FILEPATH")"
    TOTAL=$((TOTAL + 1))

    echo "==> [${TOTAL}/${ENTRY_COUNT}] ${FILEPATH}"
    echo "    CID: ${CID}"

    # Local pin.
    if ! ipfs pin add "$CID" >/dev/null 2>&1; then
        echo "    FAILED: local pin" >&2
        LOCAL_FAILED=$((LOCAL_FAILED + 1))
        LOCAL_FAILED_ENTRIES+=("$FILEPATH")
        continue
    fi
    echo "    OK: local"

    # Remote pins.
    for svc in "${SERVICES[@]}"; do
        # Skip services where this CID is already queued/pinning/pinned —
        # Pinata rejects duplicate `pin remote add` calls with
        # DUPLICATE_OBJECT (400). Same guard as ipfs-upload.sh.
        if ipfs pin remote ls --service="$svc" --cid="$CID" \
               --status=queued,pinning,pinned 2>/dev/null | grep -q .; then
            echo "    OK: ${svc} (already pinned)"
            continue
        fi

        if ipfs pin remote add --service="$svc" --name="$PIN_NAME" $BACKGROUND "$CID" >/dev/null 2>&1; then
            echo "    OK: ${svc}"
        else
            echo "    FAILED: ${svc}" >&2
            REMOTE_FAILED=$((REMOTE_FAILED + 1))
            if ! printf '%s\n' "${REMOTE_FAILED_ENTRIES[@]+"${REMOTE_FAILED_ENTRIES[@]}"}" | grep -qxF "$FILEPATH"; then
                REMOTE_FAILED_ENTRIES+=("$FILEPATH")
            fi
        fi
    done
done < "$MANIFEST"

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

echo ""
echo "==> Batch pin complete: ${TOTAL} CIDs processed."

EXIT_CODE=0

if [[ $LOCAL_FAILED -gt 0 ]]; then
    echo "" >&2
    echo "ERROR: ${LOCAL_FAILED} CID(s) failed local pinning:" >&2
    for entry in "${LOCAL_FAILED_ENTRIES[@]}"; do
        echo "  - ${entry}" >&2
    done
    EXIT_CODE=1
fi

if [[ $REMOTE_FAILED -gt 0 ]]; then
    echo "" >&2
    echo "WARNING: ${REMOTE_FAILED} remote pin submission(s) failed:" >&2
    for entry in "${REMOTE_FAILED_ENTRIES[@]}"; do
        echo "  - ${entry}" >&2
    done
    EXIT_CODE=1
fi

exit $EXIT_CODE
