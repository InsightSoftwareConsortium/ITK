#!/usr/bin/env bash
# ingest-module-v4.sh — Phase A driver for v4 remote-module ingestion
#
# Phase A is the LOCAL, REVERSIBLE half of the ingestion process.  It
# rewrites the upstream remote-module's history into a form suitable
# for a Mode-A merge into ITK proper.  Phase A makes NO commits or
# pushes to the upstream remote — the local clone is throwaway.  The
# subsequent upstream-archival publish step lives in Phase B
# (archive-remote-module.sh) and is gated on the Phase A PR being
# merged into ITK main.
#
# See INGESTION_STRATEGY_v4.md for the full design.
#
# Usage:
#   ingest-module-v4.sh <Module> <DestGroup> [options]
#
# Options:
#   --upstream-url URL    Override the GIT_REPOSITORY parsed from
#                         Modules/Remote/<Module>.remote.cmake.
#   --whitelist FILE      Override the default whitelist location
#                         (whitelists/<Module>.list, this directory).
#   --dry-run             Run filter-repo + sanitize passes but do
#                         NOT merge into ITK.  Reports what would
#                         land.
#   --keep-tempdir        Don't delete the rewritten clone after
#                         finishing (useful with --dry-run for
#                         post-mortem inspection).
#   --skip-format         Skip sanitize-history.py.  ONLY for debugging
#                         the filter-repo passes; the resulting branch
#                         will not pass ITK's pre-commit gate.
#   -h, --help            Show this help.
#
# Exit codes:
#   0  — success
#   1  — argument or environment failure
#   2  — filter-repo, sanitize, or merge failure
#   3  — post-merge verification failed (pre-commit, prefix scan)

set -euo pipefail

# --------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
info() { printf '==> %s\n'    "$*"; }
warn() { printf 'WARN: %s\n'  "$*" >&2; }
die()  { printf 'ERROR: %s\n' "$*" >&2; exit 1; }

show_help() {
  sed -n '2,/^$/{ s/^# \?//; p }' "$0"
  exit 0
}

# --------------------------------------------------------------------
# Argument parsing
# --------------------------------------------------------------------
MODULE=""
DEST_GROUP=""
UPSTREAM_URL=""
WHITELIST=""
DRY_RUN=false
KEEP_TEMPDIR=false
SKIP_FORMAT=false

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help)        show_help ;;
    --upstream-url)   UPSTREAM_URL="$2"; shift 2 ;;
    --whitelist)      WHITELIST="$2"; shift 2 ;;
    --dry-run)        DRY_RUN=true; shift ;;
    --keep-tempdir)   KEEP_TEMPDIR=true; shift ;;
    --skip-format)    SKIP_FORMAT=true; shift ;;
    -*)               die "Unknown option: $1" ;;
    *)
      if   [[ -z "$MODULE"     ]]; then MODULE="$1"
      elif [[ -z "$DEST_GROUP" ]]; then DEST_GROUP="$1"
      else die "Unexpected positional argument: $1"
      fi
      shift
      ;;
  esac
done

[[ -n "$MODULE"     ]] || die "Module name required (e.g., IOMeshSTL)"
[[ -n "$DEST_GROUP" ]] || die "Destination group required (e.g., IO, Filtering, Segmentation)"

# --------------------------------------------------------------------
# Preflight
# --------------------------------------------------------------------
command -v git-filter-repo >/dev/null \
  || die "git-filter-repo required.  Install with: pixi global install git-filter-repo"

if ! $SKIP_FORMAT; then
  command -v clang-format >/dev/null \
    || die "clang-format required for sanitize step (or pass --skip-format)"
  command -v black >/dev/null \
    || die "black required for sanitize step (or pass --skip-format)"
  command -v gersemi >/dev/null \
    || die "gersemi required for sanitize step (or pass --skip-format)"
fi

ITK_SRC="$(git rev-parse --show-toplevel 2>/dev/null || true)"
[[ -n "$ITK_SRC" ]] || die "Must be run from inside a git checkout of ITK"
[[ -f "$ITK_SRC/itk-module.cmake" || -f "$ITK_SRC/CMakeLists.txt" ]] \
  || die "Current git root does not look like ITK: $ITK_SRC"

if ! $DRY_RUN && [[ -n "$(git -C "$ITK_SRC" status --porcelain)" ]]; then
  die "Working tree not clean; commit or stash first (or use --dry-run)"
fi

# Resolve whitelist
if [[ -z "$WHITELIST" ]]; then
  WHITELIST="$SCRIPT_DIR/whitelists/$MODULE.list"
fi
[[ -r "$WHITELIST" ]] || die "Whitelist not readable: $WHITELIST
  Create it from a sibling module's whitelist as a starting point."

# Resolve clang-format / gersemi config files from the destination ITK tree
CLANG_FORMAT_STYLE="$ITK_SRC/.clang-format"
[[ -f "$CLANG_FORMAT_STYLE" ]] \
  || die "ITK .clang-format missing at $CLANG_FORMAT_STYLE"
GERSEMI_CONFIG="$ITK_SRC/.gersemi.config"
[[ -f "$GERSEMI_CONFIG" ]] \
  || warn "ITK .gersemi.config missing at $GERSEMI_CONFIG; gersemi will use defaults"

# Infer upstream URL from the .remote.cmake if not provided
if [[ -z "$UPSTREAM_URL" ]]; then
  REMOTE_FILE="$ITK_SRC/Modules/Remote/$MODULE.remote.cmake"
  [[ -f "$REMOTE_FILE" ]] \
    || die "$REMOTE_FILE not found; pass --upstream-url explicitly"
  UPSTREAM_URL=$(awk '/GIT_REPOSITORY/ { print $2 }' "$REMOTE_FILE" | head -1)
  [[ -n "$UPSTREAM_URL" ]] || die "Could not parse GIT_REPOSITORY from $REMOTE_FILE"
fi

# Destination collision check
DEST_DIR="$ITK_SRC/Modules/$DEST_GROUP/$MODULE"
if ! $DRY_RUN && [[ -e "$DEST_DIR" ]]; then
  die "Destination $DEST_DIR already exists; module already ingested?"
fi

info "Phase A — Ingestion (LOCAL, throwaway)"
info "  Module:        $MODULE"
info "  DestGroup:     $DEST_GROUP"
info "  Upstream URL:  $UPSTREAM_URL"
info "  Whitelist:     $WHITELIST"
info "  ITK source:    $ITK_SRC"
info "  Style file:    $CLANG_FORMAT_STYLE"
[[ -f "$GERSEMI_CONFIG" ]] && info "  Gersemi cfg:   $GERSEMI_CONFIG"
$DRY_RUN     && info "  Mode:          --dry-run"
$KEEP_TEMPDIR && info "  Tempdir:       will be preserved"
$SKIP_FORMAT && warn "  --skip-format: result will NOT pass pre-commit"

# --------------------------------------------------------------------
# Work area
# --------------------------------------------------------------------
WORKDIR=$(mktemp -d "${TMPDIR:-/tmp}/ingest-v4-$MODULE.XXXXXX")
LOG_DIR="$WORKDIR/logs"
mkdir -p "$LOG_DIR"

cleanup() {
  if $KEEP_TEMPDIR; then
    info "Tempdir preserved at: $WORKDIR"
  else
    rm -rf "$WORKDIR"
  fi
}
trap cleanup EXIT

# --------------------------------------------------------------------
# Step 1: mirror-clone upstream into a NON-bare clone (filter-repo
# operates on either, but we want a working tree available for
# sanitize-history.py's helper paths).
# --------------------------------------------------------------------
CLONE="$WORKDIR/$MODULE"
info "Cloning upstream into $CLONE ..."
git clone --quiet --no-local "$UPSTREAM_URL" "$CLONE"

UPSTREAM_SHA=$(git -C "$CLONE" rev-parse HEAD)
UPSTREAM_DEFAULT_BRANCH=$(git -C "$CLONE" symbolic-ref --short HEAD 2>/dev/null || echo main)
UPSTREAM_COMMIT_COUNT=$(git -C "$CLONE" rev-list --count HEAD)
UPSTREAM_MERGE_COUNT=$(git -C "$CLONE" rev-list --count --merges HEAD)
info "  Upstream tip:           $UPSTREAM_SHA"
info "  Upstream default branch: $UPSTREAM_DEFAULT_BRANCH"
info "  Total commits:          $UPSTREAM_COMMIT_COUNT"
info "  Merge commits:          $UPSTREAM_MERGE_COUNT"

# --------------------------------------------------------------------
# Step 2: filter-repo whitelist pass — read paths from the whitelist
# file.  --paths-from-file accepts shell globs, one per line, # comments.
# --------------------------------------------------------------------
info "Running filter-repo whitelist pass (--paths-from-file)..."
(
  cd "$CLONE"
  git filter-repo --force \
    --paths-from-file "$WHITELIST" \
    --prune-empty always
) || die "filter-repo whitelist pass failed"

# --------------------------------------------------------------------
# Step 2b: scaffolding deny-pass.  The whitelist admits whole
# directories (test/, wrapping/, ...) but upstream remote modules
# sometimes place CI / packaging scaffolding inside those (e.g.
# test/Docker/, wrapping/azure-pipelines.yml, .github/ inside test/).
# Strip those out across all history with a per-commit invert-glob
# pass.  Without this, scaffolding leaks into ITK's history; bug
# discovered on the first Cuberille v4 ingest (test/Docker/* leaked).
# --------------------------------------------------------------------
info "Running scaffolding deny-pattern strip pass..."
(
  cd "$CLONE"
  git filter-repo --force \
    --invert-paths \
    --path-glob '**/CTestConfig.cmake' \
    --path-glob '**/azure-pipelines*.yml' \
    --path-glob '**/azure-pipelines/*' \
    --path-glob '**/Dockerfile' \
    --path-glob '**/Dockerfile.*' \
    --path-glob '**/Dockerfile-*' \
    --path-glob '**/.dockerignore' \
    --path-glob '**/[Dd]ocker/*' \
    --path-glob '**/.[Dd]ocker/*' \
    --path-glob '**/Jenkinsfile' \
    --path-glob '**/.circleci/*' \
    --path-glob '**/circle.yml' \
    --path-glob '**/.travis.yml' \
    --path-glob '**/appveyor.yml' \
    --path-glob '**/.appveyor.yml' \
    --path-glob '**/.cirun.yml' \
    --path-glob '**/.gitlab-ci.yml' \
    --path-glob '**/.github/*' \
    --path-glob '**/codecov.yml' \
    --path-glob '**/.codecov.yml' \
    --path-glob '**/tox.ini' \
    --path-glob '**/pyproject.toml' \
    --path-glob '**/setup.py' \
    --path-glob '**/setup.cfg' \
    --path-glob '**/MANIFEST.in' \
    --path-glob '**/requirements*.txt' \
    --path-glob '**/environment*.yml' \
    --path-glob '**/.clang-format' \
    --path-glob '**/.clang-tidy' \
    --path-glob '**/.pre-commit-config.yaml' \
    --path-glob '**/*.orig' \
    --path-glob '**/*.rej' \
    --path-glob '**/*.BACKUP.*' \
    --path-glob '**/*.LOCAL.*' \
    --path-glob '**/*.REMOTE.*' \
    --path-glob '**/*.BASE.*' \
    --path-glob '**/.ExternalData_*' \
    --prune-empty always
) || die "filter-repo deny-pass failed"

# --------------------------------------------------------------------
# Step 3: subdirectory move into Modules/<DestGroup>/<Module>/
# --------------------------------------------------------------------
info "Moving into Modules/$DEST_GROUP/$MODULE/ ..."
(
  cd "$CLONE"
  git filter-repo --force \
    --to-subdirectory-filter "Modules/$DEST_GROUP/$MODULE" \
    --prune-empty always
) || die "filter-repo subdirectory pass failed"

POST_FILTER_COMMITS=$(git -C "$CLONE" rev-list --count HEAD)
POST_FILTER_MERGES=$(git -C "$CLONE" rev-list --count --merges HEAD)
info "  After filter-repo: $POST_FILTER_COMMITS commits, $POST_FILTER_MERGES merges"

# --------------------------------------------------------------------
# Step 4: sanitize-history.py — clang-format every C++ blob, black
# every .py blob, prefix every commit subject with a valid ITK prefix.
# --------------------------------------------------------------------
if $SKIP_FORMAT; then
  warn "Skipping sanitize-history.py (per --skip-format)"
else
  info "Running sanitize-history.py (clang-format + black + gersemi + ws/eof + prefix)..."
  GERSEMI_ARG=()
  [[ -f "$GERSEMI_CONFIG" ]] && GERSEMI_ARG=(--gersemi-config "$GERSEMI_CONFIG")
  python3 "$SCRIPT_DIR/sanitize-history.py" \
    --repo "$CLONE" \
    --clang-format-style "$CLANG_FORMAT_STYLE" \
    "${GERSEMI_ARG[@]}" \
    --log-dir "$LOG_DIR" \
    || die "sanitize-history.py failed"
  info "  Sanitize logs in: $LOG_DIR/"
fi

# --------------------------------------------------------------------
# Step 5: topology verification.  If upstream had merges, the rewritten
# branch MUST still have merges (filter-repo preserves merge topology).
# This guards against regressions where a future change to filter-repo
# args silently linearizes — see feedback_ingest_merge_topology.md.
# --------------------------------------------------------------------
POST_SANITIZE_COMMITS=$(git -C "$CLONE" rev-list --count HEAD)
POST_SANITIZE_MERGES=$(git -C "$CLONE" rev-list --count --merges HEAD)
info ""
info "Final rewritten history:"
info "  Commits:  $UPSTREAM_COMMIT_COUNT -> $POST_SANITIZE_COMMITS"
info "  Merges:   $UPSTREAM_MERGE_COUNT -> $POST_SANITIZE_MERGES"

if (( UPSTREAM_MERGE_COUNT > 0 )) && (( POST_SANITIZE_MERGES == 0 )); then
  die "Topology violation: upstream had $UPSTREAM_MERGE_COUNT merge(s) but rewritten history has 0.
  This indicates linearization, which is forbidden per feedback_ingest_merge_topology.md."
fi

# --------------------------------------------------------------------
# Step 6: prefix-validation scan.  Every commit subject on the rewritten
# branch must start with a valid ITK prefix after sanitize-history.py
# ran.  Catch any escapes.
# --------------------------------------------------------------------
if ! $SKIP_FORMAT; then
  info "Verifying every commit subject has a valid ITK prefix..."
  BAD_SUBJECTS=$(
    git -C "$CLONE" log --format='%H %s' \
      | awk '$2 !~ /^(BUG|COMP|DOC|ENH|PERF|STYLE|WIP):/ { print }'
  )
  if [[ -n "$BAD_SUBJECTS" ]]; then
    warn ""
    warn "================================================================="
    warn " PREFIX VIOLATION: commits without a valid prefix found.         "
    warn " sanitize-history.py should have caught these — investigate.    "
    warn ""
    printf '%s\n' "$BAD_SUBJECTS" >&2 | head -10
    warn "================================================================="
    die "Refusing to merge into ITK with non-conforming subjects."
  fi
  info "  OK: every commit has a valid prefix."
fi

# --------------------------------------------------------------------
# Step 7: dry-run stops here
# --------------------------------------------------------------------
if $DRY_RUN; then
  info ""
  info "Dry-run complete.  No changes made to ITK working tree."
  info "Rewritten clone is at: $CLONE"
  info "Sanitize logs at:      $LOG_DIR/"
  info ""
  info "To inspect:"
  info "  cd $CLONE"
  info "  git log --oneline --graph | head -50"
  info "  cat $LOG_DIR/commit-prefix-log.txt | head -30"
  exit 0
fi

# --------------------------------------------------------------------
# Step 8: collect authors for Co-authored-by trailers in merge message
# --------------------------------------------------------------------
info "Collecting author list for Co-authored-by trailers..."
PRIMARY_AUTHOR=$(
  git -C "$CLONE" log --format='%an <%ae>' HEAD \
    | sort | uniq -c | sort -rn | head -1 | sed 's/^ *[0-9]* *//'
)
readarray -t ALL_AUTHORS < <(git -C "$CLONE" log --format='%an <%ae>' HEAD | sort -u)
CO_AUTHOR_LINES=""
for a in "${ALL_AUTHORS[@]}"; do
  [[ "$a" != "$PRIMARY_AUTHOR" ]] && CO_AUTHOR_LINES+="Co-authored-by: $a"$'\n'
done

# --------------------------------------------------------------------
# Step 9: Mode-A merge into the current ITK branch
# --------------------------------------------------------------------
MERGE_MSG=$(cat <<EOF
ENH: Ingest ITK$MODULE into Modules/$DEST_GROUP

Brings $MODULE from a configure-time remote fetch into the ITK
source tree at Modules/$DEST_GROUP/$MODULE/ using the v4 ingestion
pipeline (whitelist filter-repo + per-commit clang-format + black +
commit-prefix sanitization).

Upstream repo:  $UPSTREAM_URL
Upstream tip:   $UPSTREAM_SHA
Ingest date:    $(date -u +%Y-%m-%d)
Whitelist:      $(basename "$WHITELIST")

Per-commit transforms applied across all $POST_SANITIZE_COMMITS commits:
  - filter-repo --paths-from-file (whitelist)
  - filter-repo --to-subdirectory-filter Modules/$DEST_GROUP/$MODULE
  - clang-format -style=file (ITK main's .clang-format) for *.cxx/.h/.hxx/...
  - black for *.py
  - heuristic ITK prefix added to commit subjects without one

Merge topology preserved: $UPSTREAM_MERGE_COUNT -> $POST_SANITIZE_MERGES merge(s).

Primary author: $PRIMARY_AUTHOR

$CO_AUTHOR_LINES
EOF
)

info "Merging filter-repo+sanitize output into $(git -C "$ITK_SRC" rev-parse --short HEAD)..."
(
  cd "$ITK_SRC"
  git remote add ingest-v4-tmp "$CLONE" 2>/dev/null || true
  git fetch --quiet ingest-v4-tmp \
    "$UPSTREAM_DEFAULT_BRANCH:ingest-v4-tmp-default"
  git merge --allow-unrelated-histories --no-ff \
    -m "$MERGE_MSG" \
    ingest-v4-tmp-default
  git remote remove ingest-v4-tmp
  git branch -D ingest-v4-tmp-default
) || die "Merge failed"

# --------------------------------------------------------------------
# Step 10: post-merge verification
# --------------------------------------------------------------------
info ""
info "Post-merge verification..."
if ! pre-commit run --all-files >/dev/null 2>&1; then
  warn "pre-commit reported issues — re-run interactively to inspect:"
  warn "  pre-commit run --all-files"
  exit 3
fi
info "  OK: pre-commit run --all-files exits 0."

info ""
info "Phase A complete.  Required follow-up commits (in this PR):"
info "  1. DOC:   Add Modules/$DEST_GROUP/$MODULE/README.md pointing at archived upstream"
info "  2. COMP:  Remove Modules/Remote/$MODULE.remote.cmake"
info "  3. ENH:   Add -DModule_$MODULE:BOOL=ON to pyproject.toml configure-ci"
info "  4. Verify local build + tests pass:"
info "     pixi run -e cxx configure-ci && pixi run -e cxx build"
info "     ctest --test-dir build -R $MODULE"
info ""
info "After this PR has merged into ITK main, run Phase B:"
info "  $SCRIPT_DIR/archive-remote-module.sh $MODULE"
info "  (Phase B publishes the upstream removal commit + flips archived=true.)"
