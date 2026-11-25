#!/usr/bin/env bash

#% NAME
#%     verify-cid-access.sh - check that every .cid link under a tree resolves
#%
#% SYNOPSIS
#%     verify-cid-access.sh <path> [options]
#%     verify-cid-access.sh -h | --help
#%
#% DESCRIPTION
#%     Find every .cid content link under <path> and check that each one
#%     can be downloaded from at least one IPFS gateway. ITK CI fetches the
#%     same data, so a link that fails here will also fail in CI. Run it
#%     before pushing a remote-module ingest.
#%
#%     Gateways are tried in this order, with a one-byte ranged request:
#%         https://itk.mypinata.cloud/ipfs/<cid>
#%         https://w3s.link/ipfs/<cid>
#%         https://ipfs.io/ipfs/<cid>
#%         https://<cid>.ipfs.dweb.link/
#%
#%     Each link prints one line on stdout: "OK" with the gateway used or
#%     "(cached)", "FAIL" with the link path, or "EMPTY" for a file with no
#%     CID in it.
#%
#%     Exit status: 0 when every link resolves or none are found; 1 for a
#%     usage error; 2 when any link did not resolve.
#%
#% OPTIONS
#%     <path>
#%         Directory to search, relative to the current directory, such as
#%         Modules/Filtering/AnisotropicDiffusionLBR.
#%     --quiet
#%         Print only the FAIL and EMPTY lines.
#%     --fail-fast
#%         Meant to stop at the first failure. It passes --halt, which
#%         xargs does not accept, so when any .cid files are found a run
#%         with this option checks no links and exits with status 2.
#%     --jobs N, --jobs=N
#%         Number of links checked in parallel. Default: 16.
#%     --no-cache
#%         Neither read nor write the cache of resolved CIDs.
#%     -h, --help
#%         Print this help and exit.
#%
#% IMPLEMENTATION
#%     Needs curl, xargs, and network access. A resolved CID is recorded as
#%     an empty file named <cid>.ok in XDG_CACHE_HOME/itk-verify-cid (by
#%     default ~/.cache/itk-verify-cid) and is not checked again, since the
#%     data behind a CID cannot change. Output is also copied to a temporary
#%     log in /tmp, which is removed before exit.
#%
#% EXAMPLES
#%     Utilities/Maintenance/RemoteModuleIngest/verify-cid-access.sh \
#%       Modules/Filtering/AnisotropicDiffusionLBR
#%         Check every .cid link in that module.
#%
#%     Utilities/Maintenance/RemoteModuleIngest/verify-cid-access.sh \
#%       Modules/Filtering/AnisotropicDiffusionLBR --quiet --no-cache
#%         Check again without the cache, printing only failures.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.
#%     Documentation/docs/contributing/upload_binary_data.md:
#%         How test data is uploaded and linked by CID.
#%     Utilities/Maintenance/RemoteModuleIngest/cid-normalize.sh:
#%         Convert .md5 and .shaNNN links to .cid.

set -euo pipefail

info() { printf '==> %s\n' "$*"; }
warn() { printf 'WARN: %s\n' "$*" >&2; }
die()  { printf 'ERROR: %s\n' "$*" >&2; exit 1; }

show_help() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
  exit 0
}

MODULE_PATH=""
QUIET=false
FAIL_FAST=false
JOBS=16
USE_CACHE=true

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help)     show_help ;;
    --quiet)       QUIET=true; shift ;;
    --fail-fast)   FAIL_FAST=true; shift ;;
    --jobs)        JOBS="${2:?--jobs requires N}"; shift 2 ;;
    --jobs=*)      JOBS="${1#--jobs=}"; shift ;;
    --no-cache)    USE_CACHE=false; shift ;;
    -*)            die "Unknown option: $1" ;;
    *)
      [[ -z "$MODULE_PATH" ]] || die "Unexpected positional arg: $1"
      MODULE_PATH="$1"
      shift
      ;;
  esac
done

[[ -n "$MODULE_PATH" ]] || die "Module path required."
[[ -d "$MODULE_PATH" ]] || die "Not a directory: $MODULE_PATH"
[[ "$JOBS" =~ ^[0-9]+$ ]] && (( JOBS > 0 )) || die "--jobs must be a positive integer"

CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/itk-verify-cid"
$USE_CACHE && mkdir -p "$CACHE_DIR"

# Gateway order: pinned/fast first, CID-subdomain (slow DNS) last.
# %s is the CID; expanded by the worker via printf.
export GATEWAYS_CSV="https://itk.mypinata.cloud/ipfs/%s,https://w3s.link/ipfs/%s,https://ipfs.io/ipfs/%s,https://%s.ipfs.dweb.link/"
export CACHE_DIR USE_CACHE QUIET

# Per-stub worker. Prints one line:
#   OK   <cid>  via <url>
#   FAIL <cid>  (<stub>)  [no gateway resolved]
#   EMPTY <stub>
# Exit 0 on success, 1 on failure — xargs aggregates.
check_one() {
  local stub="$1"
  local cid
  cid="$(tr -d '[:space:]' < "$stub")"
  if [[ -z "$cid" ]]; then
    printf 'EMPTY %s\n' "$stub"
    return 1
  fi

  if [[ "$USE_CACHE" == "true" && -f "$CACHE_DIR/$cid.ok" ]]; then
    [[ "$QUIET" == "true" ]] || printf 'OK   %s  (cached)\n' "$cid"
    return 0
  fi

  local g_fmt g_url
  IFS=',' read -r -a gws <<<"$GATEWAYS_CSV"
  for g_fmt in "${gws[@]}"; do
    # 1-byte ranged GET: faster & more reliable than HEAD across IPFS gateways.
    # shellcheck disable=SC2059
    g_url=$(printf "$g_fmt" "$cid")
    if curl -sf -r 0-0 --connect-timeout 5 --max-time 10 -o /dev/null "$g_url"; then
      [[ "$QUIET" == "true" ]] || printf 'OK   %s  via %s\n' "$cid" "$g_url"
      [[ "$USE_CACHE" == "true" ]] && : > "$CACHE_DIR/$cid.ok"
      return 0
    fi
  done

  printf 'FAIL %s  (%s)  [no gateway resolved]\n' "$cid" "$stub"
  return 1
}
export -f check_one

readarray -t STUBS < <(find "$MODULE_PATH" -type f -name "*.cid" | sort)

if (( ${#STUBS[@]} == 0 )); then
  info "No .cid content-links found under $MODULE_PATH."
  info "Either the module has no test data, or conversion has not run yet."
  exit 0
fi

$QUIET || info "Checking ${#STUBS[@]} .cid content-link(s) under $MODULE_PATH (jobs=$JOBS)..."

XARGS_HALT=()
$FAIL_FAST && XARGS_HALT=(--halt now,fail=1)

# NUL-delimit to survive any path oddities.
set +e
printf '%s\0' "${STUBS[@]}" \
  | xargs -0 -n1 -P "$JOBS" "${XARGS_HALT[@]}" \
      bash -c 'check_one "$@"' _ \
  | tee /tmp/verify-cid-access.$$.log
rc=${PIPESTATUS[1]}
set -e

FAIL=$(grep -cE '^(FAIL|EMPTY) ' /tmp/verify-cid-access.$$.log || true)
rm -f /tmp/verify-cid-access.$$.log

if (( FAIL > 0 )); then
  warn "$FAIL of ${#STUBS[@]} .cid content-link(s) did not resolve."
  exit 2
fi

# xargs exits non-zero if any child failed; FAIL==0 means all clean.
(( rc == 0 )) || exit 2

$QUIET || info "All ${#STUBS[@]} .cid content-link(s) resolved from at least one gateway."
