#!/usr/bin/env bash

#% NAME
#%     verify-whitelist-history.sh - find scaffolding files in module history
#%
#% SYNOPSIS
#%     verify-whitelist-history.sh <module-path> [options]
#%     verify-whitelist-history.sh -h | --help
#%
#% DESCRIPTION
#%     Check that no CI, container, or packaging file ever entered the
#%     history of an ingested module. The ingest whitelist admits whole
#%     directories such as include, src, test, and wrapping, and some
#%     upstream modules keep such files inside them.
#%
#%     Every path that git log lists for the revision inside <module-path>
#%     is matched against these names, in any
#%     subdirectory: CTestConfig.cmake, azure-pipelines*.yml, Dockerfile
#%     and Dockerfile.* or Dockerfile-*, .dockerignore, Jenkinsfile,
#%     circle.yml, .travis.yml, appveyor.yml, tox.ini, pyproject.toml,
#%     setup.py, setup.cfg, MANIFEST.in, requirements*.txt,
#%     environment*.yml, and .cirun, .gitlab-ci, .clang-format,
#%     .clang-tidy, .pre-commit-config, or .codecov followed by .yml or
#%     .yaml. Any path inside a .github, .circleci, .docker, docker (either
#%     case), or .azure-pipelines directory also matches. Default git log
#%     history simplification applies, so a side branch that a merge made
#%     redundant is not scanned. Because of the required
#%     suffix, plain .clang-format and .clang-tidy files are not reported.
#%
#%     Matching paths are listed on stderr. Nothing is modified.
#%
#%     Exit status: 0 when nothing matches; 1 for a usage error; 2 when any
#%     path matches. Errors from git are hidden, so a bad revision or a
#%     directory outside a repository also reports no matches and exits 0.
#%
#% OPTIONS
#%     <module-path>
#%         Module directory, such as Modules/Filtering/AnisotropicDiffusionLBR,
#%         relative to the current directory.
#%     --git-dir DIR
#%         Repository to read. Default: the one containing the current
#%         directory.
#%     --revision REV
#%         Scan history reachable from REV. Default: HEAD.
#%     --extra-pattern REGEX
#%         Also report paths matching this extended regular expression.
#%     -h, --help
#%         Print this help and exit.
#%
#% IMPLEMENTATION
#%     Needs git. Run from the root of the ITK checkout so that
#%     <module-path> names the module.
#%
#% EXAMPLES
#%     Utilities/Maintenance/RemoteModuleIngest/verify-whitelist-history.sh \
#%       Modules/Filtering/AnisotropicDiffusionLBR
#%         Audit the history of an ingested module on the current branch.
#%
#%     Utilities/Maintenance/RemoteModuleIngest/verify-whitelist-history.sh \
#%       Modules/IO/IOMeshSTL --revision upstream/main
#%         Audit the module as it is on upstream/main.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.
#%     Utilities/Maintenance/RemoteModuleIngest/ingest-module-v4.sh:
#%         The ingest driver whose deny pass removes these files.
#%     Utilities/Maintenance/RemoteModuleIngest/whitelists/default.list:
#%         The default ingest whitelist.

set -euo pipefail

info() { printf '==> %s\n' "$*"; }
warn() { printf 'WARN: %s\n' "$*" >&2; }
die()  { printf 'ERROR: %s\n' "$*" >&2; exit 1; }

show_help() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
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
