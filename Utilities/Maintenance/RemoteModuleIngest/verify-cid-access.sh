#!/usr/bin/env bash
# verify-cid-access.sh — confirm every .cid under a tree resolves
#
# Walks a module tree, finds every .cid content-link, and confirms
# each resolves to retrievable bytes via at least one ExternalData
# mirror / IPFS gateway.  Intended as a mandatory pre-push gate
# for remote-module ingests: the ITK CI will try to fetch the same
# content, and a .cid that the agent can't fetch from here is a
# .cid that CI can't fetch either.
#
# Usage:
#   verify-cid-access.sh <path-under-ITK> [options]
#
# Example:
#   verify-cid-access.sh Modules/Filtering/AnisotropicDiffusionLBR
#
# Options:
#   --quiet        Only print failing files.
#   --fail-fast    Exit on first failure.
#   -h|--help      Show this help.
#
# Gateways tried, in order (matches ITK's ExternalData config):
#   https://{cid}.ipfs.dweb.link/
#   https://w3s.link/ipfs/{cid}
#   https://ipfs.io/ipfs/{cid}
#   https://itk.mypinata.cloud/ipfs/{cid}
#
# Exit codes:
#   0  — every .cid resolves from at least one gateway
#   1  — usage / environment error
#   2  — one or more .cid stubs did not resolve

set -euo pipefail

info() { printf '==> %s\n' "$*"; }
warn() { printf 'WARN: %s\n' "$*" >&2; }
die()  { printf 'ERROR: %s\n' "$*" >&2; exit 1; }

show_help() {
  sed -n '2,/^$/{ s/^# \?//; p }' "$0"
  exit 0
}

MODULE_PATH=""
QUIET=false
FAIL_FAST=false

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help)     show_help ;;
    --quiet)       QUIET=true; shift ;;
    --fail-fast)   FAIL_FAST=true; shift ;;
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

GATEWAYS=(
  "https://%s.ipfs.dweb.link/"
  "https://w3s.link/ipfs/%s"
  "https://ipfs.io/ipfs/%s"
  "https://itk.mypinata.cloud/ipfs/%s"
)

readarray -t STUBS < <(find "$MODULE_PATH" -type f -name "*.cid" | sort)

if (( ${#STUBS[@]} == 0 )); then
  info "No .cid content-links found under $MODULE_PATH."
  info "Either the module has no test data, or conversion has not run yet."
  exit 0
fi

$QUIET || info "Checking ${#STUBS[@]} .cid content-link(s) under $MODULE_PATH..."

OK=0
FAIL=0
for stub in "${STUBS[@]}"; do
  cid="$(tr -d '[:space:]' < "$stub")"
  if [[ -z "$cid" ]]; then
    warn "$stub has empty content"
    FAIL=$((FAIL+1))
    $FAIL_FAST && exit 2
    continue
  fi

  resolved=false
  for g_fmt in "${GATEWAYS[@]}"; do
    # shellcheck disable=SC2059
    g_url=$(printf "$g_fmt" "$cid")
    if curl -sfI --max-time 15 -o /dev/null "$g_url"; then
      $QUIET || printf 'OK   %s  via %s\n' "$cid" "$g_url"
      resolved=true
      break
    fi
  done

  if $resolved; then
    OK=$((OK+1))
  else
    printf 'FAIL %s  (%s)  [no gateway resolved]\n' "$cid" "$stub"
    FAIL=$((FAIL+1))
    $FAIL_FAST && exit 2
  fi
done

if (( FAIL > 0 )); then
  warn "$FAIL of ${#STUBS[@]} .cid content-link(s) did not resolve."
  exit 2
fi

$QUIET || info "All ${#STUBS[@]} .cid content-link(s) resolved from at least one gateway."
