#!/usr/bin/env bash
# cid-normalize.sh — convert .md5 / .shaNNN content-links to .cid
#
# Walks a module tree, finds every non-.cid content-link
# (.md5 / .sha1 / .sha224 / .sha256 / .sha384 / .sha512), resolves
# the referenced content via the ExternalData fetch mirrors, and
# replaces the stub with a .cid file whose contents are the IPFS
# CIDv1 for the same bytes.  Old hash files are removed.
#
# Content is byte-identical across the conversion; only the
# pointer format changes.  ITK's CMake/ExternalData.cmake supports
# all of these algorithms simultaneously, so the content link file
# format is the only thing that differs.
#
# Usage:
#   cid-normalize.sh <path-under-ITK> [options]
#
# Examples:
#   cid-normalize.sh Modules/Filtering/AnisotropicDiffusionLBR
#   cid-normalize.sh Modules/Filtering/AnisotropicDiffusionLBR --dry-run
#
# Options:
#   --dry-run    Report what would change without modifying anything.
#   --verify     After conversion, call verify-cid-access.sh to
#                confirm every .cid resolves through an IPFS gateway.
#   -h|--help    Show this help.
#
# Prerequisites:
#   * `npm install -g @web3-storage/w3cli` and `w3 login <email>`
#     (only needed if a referenced blob is not already pinned
#     to web3.storage; see Documentation/docs/contributing/upload_binary_data.md).
#   * `ipfs-cid` pure-Go tool OR a local `multihash`/`ipfs` CLI for
#     CID computation.  The script prefers the `w3 cid` subcommand
#     if available, then `ipfs add --only-hash`, then a pure-Python
#     fallback using the `multiformats` package.
#   * Network access to an ExternalData mirror to fetch the
#     referenced bytes (ITK's `CMake/ExternalData.cmake` handles
#     several: data.kitware.com, w3.link, dweb.link, …).
#
# Exit codes:
#   0  — success (all content-links are now .cid, or --dry-run finished)
#   1  — usage / environment error
#   2  — action required (a file > 100 MiB needs manual upload)
#   3  — one or more fetches or CID computations failed
#
# Output: a per-file line on stdout of the form:
#   CONVERT  <old-path>  <md5|shaN>  ->  <new-path>  <cidv1>
#   SKIP     <path>  (already .cid)
#   FAIL     <path>  <reason>

set -euo pipefail

info() { printf '==> %s\n' "$*"; }
warn() { printf 'WARN: %s\n' "$*" >&2; }
die()  { printf 'ERROR: %s\n' "$*" >&2; exit 1; }

show_help() {
  sed -n '2,/^$/{ s/^# \?//; p }' "$0"
  exit 0
}

MODULE_PATH=""
DRY_RUN=false
VERIFY=false

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help)   show_help ;;
    --dry-run)   DRY_RUN=true; shift ;;
    --verify)    VERIFY=true; shift ;;
    -*)          die "Unknown option: $1" ;;
    *)
      [[ -z "$MODULE_PATH" ]] || die "Unexpected positional arg: $1"
      MODULE_PATH="$1"
      shift
      ;;
  esac
done

[[ -n "$MODULE_PATH" ]] || die "Module path required.  Example: cid-normalize.sh Modules/Filtering/<Name>"
[[ -d "$MODULE_PATH" ]] || die "Not a directory: $MODULE_PATH"

# ---------------------------------------------------------------------
# Preflight: require a CID backend before fetching anything.
# ---------------------------------------------------------------------
have_cid_backend() {
  if command -v w3 >/dev/null 2>&1 && w3 cid --help >/dev/null 2>&1; then
    return 0
  fi
  if command -v ipfs >/dev/null 2>&1; then
    return 0
  fi
  if command -v python3 >/dev/null 2>&1 && python3 -c "import multiformats" 2>/dev/null; then
    return 0
  fi
  return 1
}

if ! have_cid_backend; then
  cat >&2 <<'EOF'
ERROR: No CID backend found.  Install one of the following and re-run:

  1. Python `multiformats` (lightweight; recommended for small blobs):

       # with uv (fast, no venv pollution):
       uv pip install --system multiformats
       # or into a project venv:
       uv venv && uv pip install multiformats && source .venv/bin/activate

       # with pip (user site):
       python3 -m pip install --user multiformats

     Verify:  python3 -c "import multiformats; print(multiformats.__version__)"

  2. go-ipfs CLI (best fidelity for multi-MiB blobs; chunks + DAG):

       brew install ipfs           # macOS
       # or see https://docs.ipfs.tech/install/command-line/

  3. web3.storage CLI (matches pinning service exactly):

       npm install -g @web3-storage/w3cli
       w3 login <email>

Note: for blobs > ~1 MiB the Python `multiformats` single-hash CID will
NOT match what `ipfs` or `w3 cid` produces, and may not resolve through
any public gateway.  Prefer `ipfs` or `w3` when CIDs must round-trip
through web3.storage / dweb.link.
EOF
  exit 1
fi

# ---------------------------------------------------------------------
# Enumerate non-.cid content-links.
# ---------------------------------------------------------------------
readarray -t TARGETS < <(
  find "$MODULE_PATH" -type f \( \
       -name "*.md5" \
    -o -name "*.sha1" \
    -o -name "*.sha224" \
    -o -name "*.sha256" \
    -o -name "*.sha384" \
    -o -name "*.sha512" \
  \) | sort
)

if (( ${#TARGETS[@]} == 0 )); then
  info "No non-.cid content-links found under $MODULE_PATH."
  info "Nothing to do."
  exit 0
fi

info "Found ${#TARGETS[@]} content-link(s) to convert under $MODULE_PATH."
if $DRY_RUN; then
  info "--dry-run: listing only."
fi

# ---------------------------------------------------------------------
# Conversion helper.  Given a hash file, resolve the bytes, compute
# the CIDv1, write the .cid stub, delete the old hash file.
#
# Uses `w3 cid` when available; falls back to `ipfs add --only-hash`;
# falls back to a pure-Python computation using the `multiformats`
# package if installed.
#
# Actual implementation is intentionally slim: the goal of this
# script shipping in the tree is to document the contract and to
# let a human stub in their local tool chain.  The AI-agent workflow
# in AGENTS.md covers the case where the agent has no network
# access — in that case, the agent should walk the human through
# the manual `w3 up` + `echo <cid> > <path>.cid` steps.
# ---------------------------------------------------------------------
cid_for_bytes() {
  local input_bytes_file="$1"
  if command -v w3 >/dev/null 2>&1 && w3 cid --help >/dev/null 2>&1; then
    w3 cid "$input_bytes_file"
  elif command -v ipfs >/dev/null 2>&1; then
    # `ipfs add --cid-version=1 --only-hash --quieter` prints just the CID.
    ipfs add --cid-version=1 --only-hash --quieter "$input_bytes_file"
  elif command -v python3 >/dev/null && python3 -c "import multiformats" 2>/dev/null; then
    python3 - <<'EOF' "$input_bytes_file"
import sys
from multiformats import CID, multihash
p = sys.argv[1]
with open(p, "rb") as f:
    data = f.read()
mh = multihash.digest(data, "sha2-256")
print(str(CID("base32", 1, "raw", mh)))
EOF
  else
    die "Need one of: \`w3 cid\`, \`ipfs\`, or python3 with the multiformats package."
  fi
}

# ExternalData mirror URL templates.  Matches CMake/ITKExternalData.cmake.
# %(algo) is the uppercase algorithm name (MD5, SHA1, SHA256, ...); %(hash)
# is the lowercase hex digest.  Order is "most reliable first" so we stop
# early on hits.
MIRROR_TEMPLATES=(
  "https://insightsoftwareconsortium.github.io/ITKTestingData/%(algo)/%(hash)"
  "https://itk.org/files/ExternalData/%(algo)/%(hash)"
  "https://data.kitware.com/api/v1/file/hashsum/%(algo)/%(hash)/download"
)

fetch_bytes() {
  local algo_lc="$1"    # md5 | sha1 | sha224 | sha256 | sha384 | sha512
  local hash="$2"
  local out="$3"
  # ITK ExternalData URLs use uppercase algo (MD5, SHA512, ...).
  local algo_uc
  algo_uc=$(printf '%s' "$algo_lc" | tr '[:lower:]' '[:upper:]')
  local url
  for tmpl in "${MIRROR_TEMPLATES[@]}"; do
    url="${tmpl//%(algo)/$algo_uc}"
    url="${url//%(hash)/$hash}"
    if curl -sfL --max-time 60 -o "$out" "$url"; then
      info "  fetched from $url"
      return 0
    fi
  done
  return 1
}

# ---------------------------------------------------------------------
# Main loop.
# ---------------------------------------------------------------------
FAIL=0
for stub in "${TARGETS[@]}"; do
  ext="${stub##*.}"   # md5, sha256, etc.
  algo="$ext"         # same string
  hash="$(tr -d '[:space:]' < "$stub")"
  new_cid_file="${stub%.$ext}.cid"

  if [[ -z "$hash" ]]; then
    warn "Empty hash in $stub; skipping."
    printf 'FAIL     %s  empty-hash\n' "$stub"
    FAIL=$((FAIL+1))
    continue
  fi

  if $DRY_RUN; then
    printf 'CONVERT  %s  %s  ->  %s\n' "$stub" "$algo" "$new_cid_file"
    continue
  fi

  tmp_bytes=$(mktemp -t cid-normalize.XXXXXX)

  if ! fetch_bytes "$algo" "$hash" "$tmp_bytes"; then
    printf 'FAIL     %s  fetch-failed\n' "$stub"
    FAIL=$((FAIL+1))
    rm -f "$tmp_bytes"
    continue
  fi

  # Check file size; abort on > 100 MiB per web3.storage free-tier.
  fsize=$(stat -c '%s' "$tmp_bytes" 2>/dev/null || stat -f '%z' "$tmp_bytes")
  if (( fsize > 100*1024*1024 )); then
    warn "$stub references a ${fsize}-byte blob (> 100 MiB)."
    warn "Upload it out-of-band via \`w3 up <local-file>\` and write the CID to"
    warn "  $new_cid_file"
    warn "manually, then re-run this script."
    rm -f "$tmp_bytes"
    exit 2
  fi

  if ! cid="$(cid_for_bytes "$tmp_bytes")"; then
    printf 'FAIL     %s  cid-compute-failed\n' "$stub"
    FAIL=$((FAIL+1))
    rm -f "$tmp_bytes"
    continue
  fi

  printf '%s\n' "$cid" > "$new_cid_file"
  git -C "$(dirname "$stub")" rm -q -f "$(basename "$stub")" 2>/dev/null || rm -f "$stub"

  printf 'CONVERT  %s  %s  ->  %s  %s\n' "$stub" "$algo" "$new_cid_file" "$cid"
  rm -f "$tmp_bytes"
done

if (( FAIL > 0 )); then
  warn "$FAIL content-link(s) failed to convert."
  exit 3
fi

if $VERIFY; then
  info "Running verify-cid-access.sh to confirm new .cid stubs resolve..."
  SELF_DIR="$(dirname "$(readlink -f "$0")")"
  "$SELF_DIR/verify-cid-access.sh" "$MODULE_PATH"
fi

info "Done.  Commit the tree (git add / git commit) as a single STYLE: commit."
