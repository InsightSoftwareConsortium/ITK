#!/usr/bin/env bash
# ingest-remote-module.sh — v3 whitelist-based remote-module ingestion
#
# Moves an ITK remote module from its configure-time fetch declaration
# (Modules/Remote/<Name>.remote.cmake) into the ITK source tree at
# Modules/<DestGroup>/<Name>/, preserving authorship via a filter-repo
# merge of the upstream history restricted to the code/header/test
# whitelist.  Everything else (Old/, examples/, docs/, paper/, CI
# scaffolding, packaging) remains in the archived upstream repo.
#
# Strategy document: INGESTION_STRATEGY.md (this directory).
# Agent guidance:    AGENTS.md             (this directory).
#
# Usage:
#   ingest-remote-module.sh <Module> <DestGroup> [options]
#
# Options:
#   --upstream-url URL    Override the GIT_REPOSITORY parsed from
#                         Modules/Remote/<Module>.remote.cmake.
#   --dry-run             Run the filter-repo passes into a tempdir
#                         and report what would land, without
#                         modifying the current ITK checkout.
#   --keep-tempdir        Don't delete the filter-repo output after
#                         finishing (useful for inspection).
#   -h, --help            Show this help.
#
# Prerequisites:
#   * git-filter-repo in PATH (pixi global install git-filter-repo)
#   * Clean working tree in the current ITK checkout.
#   * Current branch off upstream/main (or wherever you want the
#     merge to land).
#
# Commits created (non-dry-run):
#   1. (merge) ENH: Ingest ITK<Module> into Modules/<DestGroup>
#
# Not created by this script — the caller adds them as follow-on
# commits (see the AGENTS.md decision tree):
#   * DOC: Add README.md pointing at archived upstream
#   * COMP: Remove <Module>.remote.cmake
#   * ENH: Enable <Module> in CI via configure-ci
#   * (optional) any CID-normalization commits
#
# Exit codes:
#   0  — success (merge created, or dry-run reported)
#   1  — any failure (argument, environment, filter-repo, or merge)

set -euo pipefail

# --------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------
info() { printf '==> %s\n'  "$*"; }
warn() { printf 'WARN: %s\n' "$*" >&2; }
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
DRY_RUN=false
KEEP_TEMPDIR=false

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help)        show_help ;;
    --upstream-url)   UPSTREAM_URL="$2"; shift 2 ;;
    --dry-run)        DRY_RUN=true; shift ;;
    --keep-tempdir)   KEEP_TEMPDIR=true; shift ;;
    -*)               die "Unknown option: $1" ;;
    *)
      if   [[ -z "$MODULE"    ]]; then MODULE="$1"
      elif [[ -z "$DEST_GROUP" ]]; then DEST_GROUP="$1"
      else die "Unexpected positional argument: $1"
      fi
      shift
      ;;
  esac
done

[[ -n "$MODULE"     ]] || die "Module name required (e.g., AnisotropicDiffusionLBR)"
[[ -n "$DEST_GROUP" ]] || die "Destination group required (e.g., Filtering, IO, Segmentation)"

# --------------------------------------------------------------------
# Preflight
# --------------------------------------------------------------------
command -v git-filter-repo >/dev/null \
  || die "git-filter-repo required.  Install with: pixi global install git-filter-repo"

ITK_SRC="$(git rev-parse --show-toplevel 2>/dev/null || true)"
[[ -n "$ITK_SRC" ]] || die "Must be run from inside a git checkout of ITK"
[[ -f "$ITK_SRC/itk-module.cmake" || -f "$ITK_SRC/CMakeLists.txt" ]] \
  || die "Current git root does not look like ITK: $ITK_SRC"

if ! $DRY_RUN && [[ -n "$(git -C "$ITK_SRC" status --porcelain)" ]]; then
  die "Working tree not clean; commit or stash first (or use --dry-run)"
fi

# Infer upstream URL from the .remote.cmake if not provided
if [[ -z "$UPSTREAM_URL" ]]; then
  REMOTE_FILE="$ITK_SRC/Modules/Remote/$MODULE.remote.cmake"
  [[ -f "$REMOTE_FILE" ]] \
    || die "$REMOTE_FILE not found; pass --upstream-url explicitly"
  UPSTREAM_URL=$(awk '/GIT_REPOSITORY/ { print $2 }' "$REMOTE_FILE" | head -1)
  [[ -n "$UPSTREAM_URL" ]] || die "Could not parse GIT_REPOSITORY from $REMOTE_FILE"
fi

info "Module:       $MODULE"
info "DestGroup:    $DEST_GROUP"
info "Upstream URL: $UPSTREAM_URL"
info "ITK source:   $ITK_SRC"
$DRY_RUN && info "Mode:         --dry-run"

# --------------------------------------------------------------------
# Work area
# --------------------------------------------------------------------
WORKDIR=$(mktemp -d "/tmp/ingest-$MODULE.XXXXXX")
cleanup() {
  if $KEEP_TEMPDIR; then
    info "Tempdir preserved at: $WORKDIR"
  else
    rm -rf "$WORKDIR"
  fi
}
trap cleanup EXIT

# --------------------------------------------------------------------
# Step 1: mirror-clone upstream
# --------------------------------------------------------------------
info "Cloning upstream (mirror) into $WORKDIR/upstream.git ..."
git clone --quiet --no-local --mirror "$UPSTREAM_URL" "$WORKDIR/upstream.git"

UPSTREAM_SHA=$(git -C "$WORKDIR/upstream.git" rev-parse HEAD)
UPSTREAM_COMMIT_COUNT=$(git -C "$WORKDIR/upstream.git" rev-list --count HEAD)
# Detect the upstream's default branch from the mirror's HEAD symref so that
# ingests work on `master`-default remotes as well as `main`-default ones.
UPSTREAM_DEFAULT_BRANCH=$(git -C "$WORKDIR/upstream.git" symbolic-ref --short HEAD 2>/dev/null \
  || echo main)
info "Upstream default branch detected as: $UPSTREAM_DEFAULT_BRANCH"
info "Upstream tip: $UPSTREAM_SHA"
info "Upstream commits (all refs): $UPSTREAM_COMMIT_COUNT"

# --------------------------------------------------------------------
# Step 2: whitelist + subdirectory filter
# --------------------------------------------------------------------
info "Running filter-repo whitelist pass..."
(
  cd "$WORKDIR/upstream.git"
  git filter-repo --force \
    --path include \
    --path src \
    --path test \
    --path wrapping \
    --path CMakeLists.txt \
    --path itk-module.cmake \
    --to-subdirectory-filter "Modules/$DEST_GROUP/$MODULE" \
    --prune-empty always
) || die "filter-repo whitelist pass failed"

# --------------------------------------------------------------------
# Step 3: deny-pattern pass — scaffolding that slipped through the
# directory-level whitelist.
#
# Why this pass exists: the whitelist admits anything under `test/`,
# `include/`, `src/`, `wrapping/` — but upstream repos sometimes place
# non-ITK scaffolding inside those directories (e.g., a top-level
# `test/azure-pipelines.yml` for standalone CI, or a `test/Docker/`
# subtree).  The whitelist by itself did not catch those, and they
# leaked into history on PR #6093 before being caught by the whitelist
# verification.  This pass strips any commit-introduced path whose
# basename matches a well-known scaffolding pattern, anywhere in the
# whitelisted tree.
#
# Patterns are structural CI / packaging scaffolding that never has a
# place inside an ITK module source tree, regardless of what directory
# the upstream chose to put it in.
# --------------------------------------------------------------------
info "Running scaffolding deny-pattern strip pass..."
(
  cd "$WORKDIR/upstream.git"
  git filter-repo --force \
    --invert-paths \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/CTestConfig.cmake" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/azure-pipelines*.yml" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/azure-pipelines/*" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/Dockerfile" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/Dockerfile.*" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/Dockerfile-*" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/.dockerignore" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/[Dd]ocker/*" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/.[Dd]ocker/*" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/Jenkinsfile" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/.circleci/*" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/circle.yml" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/.travis.yml" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/appveyor.yml" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/.appveyor.yml" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/.cirun.yml" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/.gitlab-ci.yml" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/.github/*" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/codecov.yml" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/.codecov.yml" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/tox.ini" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/pyproject.toml" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/setup.py" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/setup.cfg" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/MANIFEST.in" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/requirements*.txt" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/environment*.yml" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/.clang-format" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/.clang-tidy" \
    --path-glob "Modules/$DEST_GROUP/$MODULE/**/.pre-commit-config.yaml" \
    --prune-empty always
)

# --------------------------------------------------------------------
# Step 3b: whitelist-verification scan.  After both the whitelist
# pass (step 2) and the deny-pattern pass (step 3), no file under the
# ingested tree should match any scaffolding pattern in ANY commit of
# the filtered history — not just at tip.  This scan confirms that
# invariant and fails loudly if something slipped through (which is
# the class of bug found on #6093).
# --------------------------------------------------------------------
info "Verifying whitelist holds across the entire ingested history..."
SCAFFOLDING_PATTERNS='(^|/)(CTestConfig\.cmake|azure-pipelines[^/]*\.yml|Dockerfile([.-][^/]*)?|\.dockerignore|Jenkinsfile|circle\.yml|\.travis\.yml|appveyor\.yml|\.(cirun|gitlab-ci|clang-format|clang-tidy|pre-commit-config|codecov)\.ya?ml|tox\.ini|pyproject\.toml|setup\.py|setup\.cfg|MANIFEST\.in|requirements[^/]*\.txt|environment[^/]*\.yml)$|(^|/)(\.github|\.circleci|\.[Dd]ocker|[Dd]ocker)/'
LEAKS=$(
  git -C "$WORKDIR/upstream.git" log --all --name-only --pretty='' 2>/dev/null \
  | grep -v '^$' \
  | sort -u \
  | grep -E "$SCAFFOLDING_PATTERNS" || true
)
if [ -n "$LEAKS" ]; then
  warn ""
  warn "================================================================="
  warn " WHITELIST VIOLATION: scaffolding files survived history-wide.  "
  warn " These were present in some commit of the filtered tree despite  "
  warn " the deny-pattern pass — add new --path-glob entries and re-run."
  warn ""
  printf '%s\n' "$LEAKS" >&2
  warn "================================================================="
  warn ""
  die "Aborting.  Do NOT push this ingest; history is still bloated."
fi
info "  OK: no scaffolding patterns remain in any commit of the filtered history."

# --------------------------------------------------------------------
# Step 4: inventory + CID-normalization warning
# --------------------------------------------------------------------
POST_COMMITS=$(git -C "$WORKDIR/upstream.git" rev-list --count HEAD)
POST_FILES=$(  git -C "$WORKDIR/upstream.git" ls-tree -r --name-only HEAD | wc -l)
MD5_COUNT=$(   git -C "$WORKDIR/upstream.git" ls-tree -r --name-only HEAD | grep -c '\.md5$'              || true)
SHA_COUNT=$(   git -C "$WORKDIR/upstream.git" ls-tree -r --name-only HEAD | grep -cE '\.sha(1|224|256|384|512)$' || true)
CID_COUNT=$(   git -C "$WORKDIR/upstream.git" ls-tree -r --name-only HEAD | grep -c '\.cid$'              || true)

info ""
info "Filter results:"
info "  upstream commits -> surviving : $UPSTREAM_COMMIT_COUNT -> $POST_COMMITS"
info "  files in whitelisted tree     : $POST_FILES"
info "  content-links by algorithm    : .md5=$MD5_COUNT  .shaNNN=$SHA_COUNT  .cid=$CID_COUNT"

NON_CID=$(( MD5_COUNT + SHA_COUNT ))
if (( NON_CID > 0 )); then
  warn ""
  warn "================================================================="
  warn "  CID normalization pending: $NON_CID non-.cid content-link(s)    "
  warn "  These should be converted to .cid before this PR merges.        "
  warn "  See Documentation/docs/contributing/upload_binary_data.md for   "
  warn "  the @web3-storage/w3cli workflow (ITK's own precedent in commit "
  warn "  f3899ce8c6).  Can be deferred to a tree-wide sweep PR.          "
  warn "================================================================="
  warn ""
fi

# --------------------------------------------------------------------
# Step 5: dry-run stops here
# --------------------------------------------------------------------
if $DRY_RUN; then
  info "Dry-run: stopping before merge."
  info "Filtered upstream is at $WORKDIR/upstream.git (will be cleaned unless --keep-tempdir)"
  exit 0
fi

# --------------------------------------------------------------------
# Step 6: collect authors + craft merge commit message
# --------------------------------------------------------------------
info "Collecting author list for Co-authored-by trailers..."
readarray -t AUTHORS < <(
  git -C "$WORKDIR/upstream.git" log --format='%an <%ae>' HEAD | sort -u
)
PRIMARY_AUTHOR=$(
  git -C "$WORKDIR/upstream.git" log --format='%an <%ae>' HEAD | sort | uniq -c | sort -rn | head -1 | sed 's/^ *[0-9]* *//'
)
CO_AUTHOR_LINES=""
for a in "${AUTHORS[@]}"; do
  [[ "$a" != "$PRIMARY_AUTHOR" ]] && CO_AUTHOR_LINES+="Co-authored-by: $a"$'\n'
done

CID_STATUS_LINE=""
if (( NON_CID > 0 )); then
  CID_STATUS_LINE="TODO before merge: convert $NON_CID non-.cid content-link(s) to .cid."
fi

MERGE_MSG=$(cat <<EOF
ENH: Ingest ITK$MODULE into Modules/$DEST_GROUP

Brings $MODULE from a configure-time remote fetch into the ITK
source tree at Modules/$DEST_GROUP/$MODULE/ using the v3 whitelist
filter-repo pipeline.

Upstream repo: $UPSTREAM_URL
Upstream tip:  $UPSTREAM_SHA
Ingest date:   $(date -u +%Y-%m-%d)

Whitelist passes (git filter-repo):
  - --path include --path src --path test --path wrapping
  - --path CMakeLists.txt --path itk-module.cmake
  - --to-subdirectory-filter Modules/$DEST_GROUP/$MODULE
  - --prune-empty always
  - (if present) second pass: invert CTestConfig.cmake

Outcome: $UPSTREAM_COMMIT_COUNT upstream commits -> $POST_COMMITS surviving;
${#AUTHORS[@]} distinct authors preserved; git blame walks across the
merge boundary to original authors.

Content-link inventory: .md5=$MD5_COUNT  .shaNNN=$SHA_COUNT  .cid=$CID_COUNT
$CID_STATUS_LINE

Primary author: $PRIMARY_AUTHOR

$CO_AUTHOR_LINES
EOF
)

# --------------------------------------------------------------------
# Step 7: merge into current ITK branch
# --------------------------------------------------------------------
info "Merging filter-repo output into $(git -C "$ITK_SRC" rev-parse --short HEAD)..."
(
  cd "$ITK_SRC"
  git remote add ingest-src-tmp "$WORKDIR/upstream.git" 2>/dev/null || true
  git fetch --quiet ingest-src-tmp \
    "$UPSTREAM_DEFAULT_BRANCH:ingest-src-tmp-default"
  git merge --allow-unrelated-histories --no-ff \
    -m "$MERGE_MSG" \
    ingest-src-tmp-default
  git remote remove ingest-src-tmp
  git branch -D ingest-src-tmp-default
) || die "Merge failed"

info ""
info "Ingest merge complete.  Required follow-up commits (in order):"
info "  1. DOC:   Add Modules/$DEST_GROUP/$MODULE/README.md pointing at archived upstream"
info "  2. COMP:  Remove Modules/Remote/$MODULE.remote.cmake"
info "  3. ENH:   Add -DModule_$MODULE:BOOL=ON to pyproject.toml configure-ci"
if (( NON_CID > 0 )); then
  info "  4. STYLE: Convert $NON_CID .md5/.shaNNN content-links to .cid"
  info "           (run cid-normalize.sh Modules/$DEST_GROUP/$MODULE)"
fi
info "  5. Verify every .cid resolves via the IPFS gateway:"
info "     verify-cid-access.sh Modules/$DEST_GROUP/$MODULE"
info "  6. Local build + test (MUST pass before pushing the PR):"
info "     pixi run -e cxx configure-ci && pixi run -e cxx build"
info "     ctest --test-dir build -R $MODULE"
info "  7. After the ITK ingest PR merges, open a follow-up PR on the"
info "     original upstream repo that (a) deletes the whitelisted files,"
info "     (b) adds MIGRATION_README.md pointing at ITK, (c) states"
info "     intent to archive the repository.  See AGENTS.md decision 7."
info ""
info "See AGENTS.md in this directory for the per-module review checklist."

# --------------------------------------------------------------------
# Step 8: enforce the pre-push gates — do not silently let an agent
# push an ingest PR with non-.cid content-links still in the tree.
# --------------------------------------------------------------------
if (( NON_CID > 0 )); then
  warn ""
  warn "================================================================="
  warn " ACTION REQUIRED (exit code 2): CID conversion is mandatory."
  warn " $NON_CID non-.cid content-link(s) remain in the ingested tree."
  warn ""
  warn " Run before committing any follow-on commits:"
  warn "   $(dirname "$0")/cid-normalize.sh Modules/$DEST_GROUP/$MODULE"
  warn ""
  warn " Then:"
  warn "   $(dirname "$0")/verify-cid-access.sh Modules/$DEST_GROUP/$MODULE"
  warn ""
  warn " The ingest PR must NOT be pushed until this is complete."
  warn "================================================================="
  warn ""
  exit 2
fi
