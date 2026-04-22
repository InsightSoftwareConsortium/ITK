#!/usr/bin/env bash
# verify-whitelist-history.sh — confirm no scaffolding patterns leaked
# into an ingested module's history.
#
# The whitelist in ingest-remote-module.sh admits everything under
# `include/`, `src/`, `test/`, `wrapping/` + the two root CMake
# files.  Directory-level admission is not tight enough on its own:
# some upstream repos put scaffolding (CI configs, Dockerfiles,
# packaging files) inside those directories, and the whitelist
# cannot tell "foo/azure-pipelines.yml" apart from "foo/*.cxx".
#
# This helper scans EVERY commit reachable from the current branch,
# restricted to the supplied module path, for any basename that
# matches a known scaffolding pattern.  Prints each leak on stdout
# and exits with code 2 if any are found.
#
# Intended uses:
#  * As the final step of ingest-remote-module.sh (driver embeds it).
#  * As a standalone audit of an already-merged ingest:
#      verify-whitelist-history.sh Modules/Filtering/AnisotropicDiffusionLBR
#  * As a CI check on the ingest branch before push.
#
# Usage:
#   verify-whitelist-history.sh <module-path> [options]
#
# Options:
#   --git-dir DIR    Use a different git dir (default: inferred).
#   --revision REV   Scan history reachable from REV rather than HEAD.
#   --extra-pattern REGEX   Add a custom pattern to the scan.
#   -h|--help        Show this help.
#
# Exit codes:
#   0  — no scaffolding basenames found in any commit of the history
#   1  — usage / environment error
#   2  — one or more scaffolding paths leaked into the history

set -euo pipefail

info() { printf '==> %s\n' "$*"; }
warn() { printf 'WARN: %s\n' "$*" >&2; }
die()  { printf 'ERROR: %s\n' "$*" >&2; exit 1; }

show_help() {
  sed -n '2,/^$/{ s/^# \?//; p }' "$0"
  exit 0
}

MODULE_PATH=""
GIT_DIR_OVERRIDE=""
REVISION="HEAD"
EXTRA_PATTERN=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help)         show_help ;;
    --git-dir)         GIT_DIR_OVERRIDE="$2"; shift 2 ;;
    --revision)        REVISION="$2"; shift 2 ;;
    --extra-pattern)   EXTRA_PATTERN="$2"; shift 2 ;;
    -*)                die "Unknown option: $1" ;;
    *)
      [[ -z "$MODULE_PATH" ]] || die "Unexpected positional arg: $1"
      MODULE_PATH="$1"
      shift
      ;;
  esac
done

[[ -n "$MODULE_PATH" ]] || die "Module path required.  Example: Modules/Filtering/AnisotropicDiffusionLBR"

GIT=(git)
[[ -n "$GIT_DIR_OVERRIDE" ]] && GIT=(git --git-dir="$GIT_DIR_OVERRIDE")

# Anchored regex: matches scaffolding BASENAMES in any subdirectory,
# not just at the module root.  The `(^|/)` opening lets the pattern
# fire for paths like `Modules/X/test/Docker/Dockerfile` — i.e., the
# directory-nested leaks the PR #6093 audit surfaced.
SCAFFOLDING_PATTERNS='(^|/)(CTestConfig\.cmake|azure-pipelines[^/]*\.yml|Dockerfile([.-][^/]*)?|\.dockerignore|Jenkinsfile|circle\.yml|\.travis\.yml|appveyor\.yml|\.(cirun|gitlab-ci|clang-format|clang-tidy|pre-commit-config|codecov)\.ya?ml|tox\.ini|pyproject\.toml|setup\.py|setup\.cfg|MANIFEST\.in|requirements[^/]*\.txt|environment[^/]*\.yml)$|(^|/)(\.github|\.circleci|\.[Dd]ocker|[Dd]ocker|\.azure-pipelines)/'

if [[ -n "$EXTRA_PATTERN" ]]; then
  SCAFFOLDING_PATTERNS="$SCAFFOLDING_PATTERNS|$EXTRA_PATTERN"
fi

info "Scanning commits reachable from $REVISION restricted to $MODULE_PATH ..."

LEAKS=$(
  "${GIT[@]}" log --name-only --pretty='' "$REVISION" -- "$MODULE_PATH" 2>/dev/null \
  | grep -v '^$' \
  | sort -u \
  | grep -E "$SCAFFOLDING_PATTERNS" || true
)

if [[ -z "$LEAKS" ]]; then
  info "OK: no scaffolding basenames found in any commit of $MODULE_PATH history."
  exit 0
fi

warn ""
warn "================================================================="
warn " WHITELIST VIOLATION: scaffolding files found in ingested history"
warn " of $MODULE_PATH (reachable from $REVISION)."
warn ""
warn " These paths should not be in ITK's git pack.  Either:"
warn "   (a) re-ingest with a tighter filter-repo deny-pattern pass,  "
warn "       or"
warn "   (b) rewrite history via filter-repo --invert-paths for each "
warn "       leaked path."
warn ""
printf '  %s\n' $LEAKS >&2
warn "================================================================="
warn ""
exit 2
