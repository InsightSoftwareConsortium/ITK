#!/usr/bin/env bash
set -euo pipefail

# Normalize ITK content links: convert .md5 / .shaNNN links to .cid and
# regenerate existing .cid links under the UnixFS v1 2025 profile.
#
# For each content link found, the script:
#   1. Fetches the bytes via the gateway templates declared in
#      CMake/ITKExternalData.cmake (identical order to the build).
#   2. Verifies the bytes against the declared hash or CID.
#   3. Re-materialises the actual file alongside the link, then invokes
#      ipfs-upload.sh on it so a fresh CID is produced under the UnixFS
#      v1 2025 profile, pinned on itk-pinata and itk-filebase, and
#      (optionally) mirrored into ITKTestingData.
#
# Usage:
#   content-link-normalize.sh [options] <path-or-file>
#
# Options:
#   --testing-data-repo <path>   Forwarded to ipfs-upload.sh. Local
#                                ITKTestingData clone to mirror bytes into.
#   --background                 Forwarded to ipfs-upload.sh. Submit remote
#                                pin requests asynchronously; useful for
#                                batch runs where waiting for each pin to
#                                reach 'pinned' status (minutes per file)
#                                is impractical. Verify final pin state
#                                afterwards with `ipfs pin remote ls`.
#   --dry-run                    List what would change without modifying.
#   --hash-only                  Process only .md5 / .shaNNN links
#                                (leave existing .cid links alone).
#   --cid-only                   Process only .cid links
#                                (re-hash under UnixFS v1 2025 profile).
#   -h|--help                    Show this help.
#
# Exit codes:
#   0  — all content links normalized
#   1  — usage / environment error
#   2  — one or more links failed to fetch, verify, or re-upload

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
CMAKE_FILE="$REPO_ROOT/CMake/ITKExternalData.cmake"
UPLOAD_SCRIPT="$SCRIPT_DIR/ipfs-upload.sh"

info() { printf '==> %s\n' "$*"; }
warn() { printf 'WARN: %s\n' "$*" >&2; }
die()  { printf 'ERROR: %s\n' "$*" >&2; exit 1; }

show_help() {
    sed -n '3,/^$/{ s/^# \?//; p }' "$0"
    exit 0
}

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

TESTING_DATA_REPO=""
BACKGROUND=false
DRY_RUN=false
HASH_ONLY=false
CID_ONLY=false
TARGET=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        -h|--help)    show_help ;;
        --dry-run)    DRY_RUN=true; shift ;;
        --hash-only)  HASH_ONLY=true; shift ;;
        --cid-only)   CID_ONLY=true; shift ;;
        --background) BACKGROUND=true; shift ;;
        --testing-data-repo)
            TESTING_DATA_REPO="${2:?--testing-data-repo requires a path}"
            shift 2
            ;;
        --testing-data-repo=*)
            TESTING_DATA_REPO="${1#--testing-data-repo=}"
            shift
            ;;
        -*) die "Unknown option: $1" ;;
        *)
            [[ -z "$TARGET" ]] || die "Unexpected positional arg: $1"
            TARGET="$1"
            shift
            ;;
    esac
done

[[ -n "$TARGET" ]] || die "Path or file required. Example: content-link-normalize.sh Testing/Data/Input"
[[ -e "$TARGET" ]] || die "Not found: $TARGET"
[[ -f "$CMAKE_FILE" ]] || die "Cannot find $CMAKE_FILE"
[[ -x "$UPLOAD_SCRIPT" ]] || die "Cannot find or execute $UPLOAD_SCRIPT"

if $HASH_ONLY && $CID_ONLY; then
    die "--hash-only and --cid-only are mutually exclusive"
fi

# ---------------------------------------------------------------------------
# Prerequisites
# ---------------------------------------------------------------------------

command -v curl >/dev/null 2>&1 || die "curl is required"
command -v ipfs >/dev/null 2>&1 || die "ipfs is required (for CID recomputation)"

# Resolve the local command that computes a digest for an algorithm.
# Returns a command line (possibly multi-word, e.g. "shasum -a 256") whose
# first whitespace-delimited token on stdout is the hex digest.
#
# Prefers GNU coreutils (`md5sum`, `shaNNNsum`) when present; falls back to
# the BSD/macOS tools that ship by default on macOS: `md5 -r` (output format
# matches md5sum) and `shasum -a NNN`. On macOS you can install coreutils
# via `brew install coreutils` to get the `*sum` variants as well.
hash_cmd_for_ext() {
    case "$1" in
        md5)
            if command -v md5sum >/dev/null 2>&1; then
                echo "md5sum"
            elif command -v md5 >/dev/null 2>&1; then
                # BSD md5; -r prints `<hash> <file>` like md5sum.
                echo "md5 -r"
            else
                return 1
            fi
            ;;
        sha1|sha224|sha256|sha384|sha512)
            if command -v "${1}sum" >/dev/null 2>&1; then
                echo "${1}sum"
            elif command -v shasum >/dev/null 2>&1; then
                echo "shasum -a ${1#sha}"
            else
                return 1
            fi
            ;;
        *)
            return 1
            ;;
    esac
}

# ITK content links in practice use `.md5` (legacy) or `.sha512` (current);
# other sha variants are supported for completeness but not pre-checked.
for alg in md5 sha512; do
    hash_cmd_for_ext "$alg" >/dev/null 2>&1 \
        || warn "no tool available to compute ${alg}; any .${alg} content links will fail to verify"
done

# ---------------------------------------------------------------------------
# Parse ExternalData_URL_TEMPLATES from CMake/ITKExternalData.cmake
# ---------------------------------------------------------------------------
#
# Matches the order in the .cmake file exactly. The block we want looks like:
#
#   list(
#     APPEND
#     ExternalData_URL_TEMPLATES
#     # comment
#     "https://.../%(hash)"
#     ...
#   )
#
# Strategy: join the whole file into one logical string, locate the
# `list(... ExternalData_URL_TEMPLATES ... )` invocation by matching
# balanced parentheses, then print every quoted template inside it that
# contains %(hash).

readarray -t URL_TEMPLATES < <(
    awk '
        BEGIN { depth = 0; in_block = 0 }
        {
            line = $0
            # Trim leading whitespace.
            sub(/^[[:space:]]+/, "", line)

            # Enter the block when we see `list(` followed somewhere by
            # `ExternalData_URL_TEMPLATES` at depth 1. We buffer tokens
            # at depth 1 until we are sure.
            if (!in_block && line ~ /^list[[:space:]]*\(/) {
                pending_list = 1
                depth = 1
                next
            }
            if (pending_list) {
                if (line ~ /ExternalData_URL_TEMPLATES/) {
                    in_block = 1
                    pending_list = 0
                    next
                }
                # Track depth so we know when the list(...) we rejected ends.
                n_open  = gsub(/\(/, "(", line)
                n_close = gsub(/\)/, ")", line)
                depth += n_open - n_close
                if (depth <= 0) { pending_list = 0; depth = 0 }
                next
            }

            if (in_block) {
                if (line ~ /^#/) next
                if (line ~ /^\)/) { in_block = 0; depth = 0; next }
                if (match(line, /"[^"]+"/)) {
                    tmpl = substr(line, RSTART + 1, RLENGTH - 2)
                    if (tmpl ~ /%\(hash\)/) print tmpl
                }
            }
        }
    ' "$CMAKE_FILE"
)

if [[ ${#URL_TEMPLATES[@]} -eq 0 ]]; then
    die "Failed to parse ExternalData_URL_TEMPLATES from $CMAKE_FILE"
fi

info "Loaded ${#URL_TEMPLATES[@]} gateway template(s) from CMake/ITKExternalData.cmake"

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Algorithm mapping: extension -> uppercase algorithm name for %(algo).
# Matches CMake ExternalData.cmake's _ExternalData_link_content behaviour.
algo_uc_for_ext() {
    case "$1" in
        md5)    echo "MD5" ;;
        sha1)   echo "SHA1" ;;
        sha224) echo "SHA224" ;;
        sha256) echo "SHA256" ;;
        sha384) echo "SHA384" ;;
        sha512) echo "SHA512" ;;
        # CID uses a lowercase override in ITKExternalData.cmake
        # (ExternalData_URL_ALGO_CID_lower = cid).
        cid)    echo "cid" ;;
        *)      return 1 ;;
    esac
}

# Substitute %(algo) / %(hash) in a URL template.
render_url() {
    local template="$1" algo="$2" hash="$3"
    local url="${template//%(algo)/$algo}"
    url="${url//%(hash)/$hash}"
    printf '%s\n' "$url"
}

# Fetch a content link into a tempfile, verifying the bytes correspond to
# the declared digest or CID. Prints the path of the verified tempfile on
# success.
#
# For CID links: `ipfs cat` is the primary fetch path because the daemon
# verifies the returned bytes server-side against the requested CID.
# Public IPFS HTTP gateways (paths containing /ipfs/) also verify
# server-side, so fetches from those URLs are accepted without local
# recomputation. Local `ipfs add --only-hash` is NOT used for verification
# because it can produce a different CID from the stored one when the
# original upload used non-default chunker or hash parameters — chunker
# drift is exactly what the UnixFS v1 2025 profile is meant to fix, so a
# mismatch would be expected, not an error.
#
# For hash links (.md5, .shaNNN): non-IPFS gateways only serve bytes by
# name, so we recompute the digest locally and compare.
fetch_and_verify() {
    local ext="$1"   # cid / md5 / shaNNN
    local value="$2" # the actual hash or CID
    local out
    out="$(mktemp -t itk-content-link.XXXXXX)"

    local algo_uc
    algo_uc="$(algo_uc_for_ext "$ext")" || {
        warn "Unknown content-link extension: .${ext}"
        rm -f "$out"
        return 1
    }

    # Fast path for .cid: fetch via the running daemon. Verification is
    # implicit — the daemon refuses to return bytes that do not hash back
    # to the CID.
    if [[ "$ext" == "cid" ]]; then
        if ipfs cat "$value" > "$out" 2>/dev/null && [[ -s "$out" ]]; then
            printf '%s\n' "$out"
            return 0
        fi
    fi

    local template rendered
    for template in "${URL_TEMPLATES[@]}"; do
        rendered="$(render_url "$template" "$algo_uc" "$value")"

        # IPFS gateway templates (path contains /ipfs/) only make sense for CIDs.
        if [[ "$ext" != "cid" && "$rendered" == *"/ipfs/"* ]]; then
            continue
        fi

        if ! curl -sfL --connect-timeout 10 --max-time 120 -o "$out" "$rendered"; then
            continue
        fi

        if verify_bytes "$ext" "$value" "$out" "$rendered"; then
            printf '%s\n' "$out"
            return 0
        else
            warn "  content from ${rendered} did not verify; trying next gateway"
        fi
    done

    rm -f "$out"
    return 1
}

# Verify that the fetched bytes at $file correspond to the declared link.
#
# For CID links: trust only fetches from IPFS HTTP gateways, which verify
# server-side (a CID-indexed path the server actually serves is by
# definition a path whose bytes hash to that CID).
#
# For hash links: recompute the digest and compare case-insensitively.
verify_bytes() {
    local ext="$1" expected="$2" file="$3" source_url="${4:-}"
    if [[ "$ext" == "cid" ]]; then
        # IPFS HTTP gateways do server-side verification; accept those.
        if [[ "$source_url" == *"/ipfs/"* ]]; then
            [[ -s "$file" ]]
            return
        fi
        # Non-IPFS origin (e.g. GitHub Pages mirror at .../CID/<cid>) —
        # we cannot verify locally without risking chunker-drift false
        # negatives, so reject. The `ipfs cat` fast path in
        # fetch_and_verify is the canonical way to resolve a .cid.
        return 1
    fi

    local cmd actual
    cmd="$(hash_cmd_for_ext "$ext")" || return 1
    # Word-splitting is intentional — a fallback command like "shasum -a 256"
    # expands to multiple argv entries, while the coreutils "md5sum" stays
    # as a single argv entry.
    # shellcheck disable=SC2086
    actual="$($cmd "$file" | awk '{print $1}')"
    [[ "${actual,,}" == "${expected,,}" ]]
}

# ---------------------------------------------------------------------------
# Enumerate targets
# ---------------------------------------------------------------------------

if [[ -f "$TARGET" ]]; then
    LINKS=("$TARGET")
else
    LINKS=()
    readarray -t LINKS < <(
        find "$TARGET" -type f \( \
             -name "*.cid" \
          -o -name "*.md5" \
          -o -name "*.sha1" \
          -o -name "*.sha224" \
          -o -name "*.sha256" \
          -o -name "*.sha384" \
          -o -name "*.sha512" \
        \) | LC_ALL=C sort
    )
fi

# Filter by --hash-only / --cid-only. Iterate defensively so `set -u` on an
# empty LINKS array (e.g. directory with no content links) does not error
# out on bash versions before 4.4.
FILTERED=()
if [[ ${#LINKS[@]} -gt 0 ]]; then
    for link in "${LINKS[@]}"; do
        ext="${link##*.}"
        if $HASH_ONLY && [[ "$ext" == "cid" ]]; then continue; fi
        if $CID_ONLY  && [[ "$ext" != "cid" ]]; then continue; fi
        FILTERED+=("$link")
    done
fi

if [[ ${#FILTERED[@]} -eq 0 ]]; then
    info "No matching content links under ${TARGET}. Nothing to do."
    exit 0
fi

LINKS=("${FILTERED[@]}")

info "Processing ${#LINKS[@]} content link(s)..."
$DRY_RUN && info "(--dry-run: no files will be modified)"

# ---------------------------------------------------------------------------
# Main loop
# ---------------------------------------------------------------------------

UPLOAD_ARGS=()
if [[ -n "$TESTING_DATA_REPO" ]]; then
    UPLOAD_ARGS+=(--testing-data-repo "$TESTING_DATA_REPO")
fi
if $BACKGROUND; then
    UPLOAD_ARGS+=(--background)
fi

FAIL=0

for link in "${LINKS[@]}"; do
    ext="${link##*.}"
    value="$(tr -d '[:space:]' < "$link")"
    real_file="${link%.${ext}}"

    if [[ -z "$value" ]]; then
        printf 'FAIL     %s  empty-content-link\n' "$link" >&2
        FAIL=$((FAIL + 1))
        continue
    fi

    if $DRY_RUN; then
        printf 'WOULD-NORMALIZE  %s  (%s=%s)  ->  %s.cid\n' \
            "$link" "$ext" "$value" "$real_file"
        continue
    fi

    info "Normalizing ${link} (${ext}=${value})"

    if [[ -e "$real_file" ]]; then
        die "Refusing to normalize: ${real_file} already exists on disk. Delete or move it first."
    fi

    tmp_bytes=""
    if ! tmp_bytes="$(fetch_and_verify "$ext" "$value")"; then
        printf 'FAIL     %s  fetch-or-verify-failed\n' "$link" >&2
        FAIL=$((FAIL + 1))
        continue
    fi

    # Stage the real file next to the link, then re-upload via ipfs-upload.sh.
    mv "$tmp_bytes" "$real_file"

    # Remove the old content link BEFORE running ipfs-upload.sh — the upload
    # script rejects inputs that look like content links (defensive guard),
    # but we also want a clean working tree if the upload fails.
    rm -f "$link"

    if ! "$UPLOAD_SCRIPT" "${UPLOAD_ARGS[@]}" "$real_file"; then
        printf 'FAIL     %s  upload-failed\n' "$link" >&2
        # Best-effort recovery: restore the original link file from its value.
        # ipfs-upload.sh writes the .cid file and removes the data file before
        # updating the manifest, so a failure in the manifest step can leave a
        # .cid orphan alongside the restored original link — clean it up too.
        printf '%s\n' "$value" > "$link"
        rm -f "$real_file" "${real_file}.cid"
        FAIL=$((FAIL + 1))
        continue
    fi

    printf 'NORMALIZE  %s  (%s)  ->  %s.cid\n' "$link" "$ext" "$real_file"
done

if (( FAIL > 0 )); then
    warn "${FAIL} content link(s) failed to normalize."
    exit 2
fi

info "Done. Review changes and commit as a STYLE: commit (see Documentation/AI/git-commits.md)."
