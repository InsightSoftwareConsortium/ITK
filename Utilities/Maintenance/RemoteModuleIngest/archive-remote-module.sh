#!/usr/bin/env bash
# archive-remote-module.sh — Phase B driver for v4 ingestion
#
# Phase B is the IRREVERSIBLE, UPSTREAM-PUBLISH half.  It runs ONLY
# AFTER the Phase A ingest PR has merged into ITK main.  Phase B:
#
#   1. Clones the upstream remote module fresh (NEVER reuses Phase A's
#      filter-repo'd clone — that history is rewritten and unsafe to
#      push back).
#   2. Removes the whitelisted files (the ones now living in ITK).
#   3. Promotes MIGRATION_README.md to README.md so the GitHub-rendered
#      landing page shows the migration notice (per
#      feedback_ingest_archive_readme.md).
#   4. Pushes the resulting two commits to upstream main.
#   5. Verifies via gh api that the rendered README contains the
#      migration notice.
#   6. Flips archived=true on the GitHub repo.
#
# See INGESTION_STRATEGY_v4.md for the full design.
#
# Usage:
#   archive-remote-module.sh <Module> [options]
#
# Options:
#   --upstream-url URL    Override the GIT_REPOSITORY parsed from
#                         Modules/Remote/<Module>.remote.cmake.
#   --whitelist FILE      Override the default whitelist location
#                         (whitelists/<Module>.list, this directory).
#   --itk-pr-number N     The merged Phase A PR number, included in
#                         the upstream removal-commit message.
#   --no-archive-flip     Push the deletion + README commits, but do
#                         NOT flip archived=true.  Use when the
#                         upstream still needs hand-tweaks.
#   --dry-run             Build the commits in a tempdir and show
#                         what would be pushed; do not push.
#   --keep-tempdir        Don't delete the throwaway clone.
#   -h, --help            Show this help.
#
# Exit codes:
#   0  — success
#   1  — argument or environment failure
#   2  — clone, commit, or push failure
#   3  — README verification failed (rendered README is not the migration notice)

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
UPSTREAM_URL=""
WHITELIST=""
ITK_PR_NUMBER=""
NO_ARCHIVE_FLIP=false
DRY_RUN=false
KEEP_TEMPDIR=false

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help)           show_help ;;
    --upstream-url)      UPSTREAM_URL="$2"; shift 2 ;;
    --whitelist)         WHITELIST="$2"; shift 2 ;;
    --itk-pr-number)     ITK_PR_NUMBER="$2"; shift 2 ;;
    --no-archive-flip)   NO_ARCHIVE_FLIP=true; shift ;;
    --dry-run)           DRY_RUN=true; shift ;;
    --keep-tempdir)      KEEP_TEMPDIR=true; shift ;;
    -*)                  die "Unknown option: $1" ;;
    *)
      [[ -z "$MODULE" ]] && MODULE="$1" || die "Unexpected positional argument: $1"
      shift
      ;;
  esac
done

[[ -n "$MODULE" ]] || die "Module name required (e.g., IOMeshSTL)"

# --------------------------------------------------------------------
# Preflight
# --------------------------------------------------------------------
command -v gh  >/dev/null || die "gh CLI required"
command -v git >/dev/null || die "git required"

# Resolve whitelist
if [[ -z "$WHITELIST" ]]; then
  WHITELIST="$SCRIPT_DIR/whitelists/$MODULE.list"
fi
[[ -r "$WHITELIST" ]] || die "Whitelist not readable: $WHITELIST"

# Try to infer upstream URL from the now-removed remote.cmake by looking
# back in git history of an ITK checkout, if available.
if [[ -z "$UPSTREAM_URL" ]]; then
  ITK_SRC="$(git rev-parse --show-toplevel 2>/dev/null || true)"
  if [[ -n "$ITK_SRC" ]]; then
    REMOTE_FILE="$ITK_SRC/Modules/Remote/$MODULE.remote.cmake"
    if [[ -f "$REMOTE_FILE" ]]; then
      UPSTREAM_URL=$(awk '/GIT_REPOSITORY/ { print $2 }' "$REMOTE_FILE" | head -1)
    else
      # remote.cmake already deleted by Phase A — try the file's last
      # version before deletion.
      UPSTREAM_URL=$(
        git -C "$ITK_SRC" log --all --pretty=format: --diff-filter=D \
            -p -- "Modules/Remote/$MODULE.remote.cmake" 2>/dev/null \
          | awk '/^-\s*GIT_REPOSITORY/ { print $2 }' \
          | head -1
      ) || true
    fi
  fi
fi
[[ -n "$UPSTREAM_URL" ]] \
  || die "Could not infer upstream URL; pass --upstream-url explicitly."

# Parse owner/repo for gh-api calls
OWNER_REPO=$(echo "$UPSTREAM_URL" \
  | sed -E 's#^(git@github.com:|https://github.com/)##; s#\.git$##')
[[ "$OWNER_REPO" =~ ^[^/]+/[^/]+$ ]] \
  || die "Could not parse owner/repo from $UPSTREAM_URL (got: $OWNER_REPO)"

info "Phase B — Upstream archival (fresh clone, irreversible publish)"
info "  Module:        $MODULE"
info "  Upstream URL:  $UPSTREAM_URL"
info "  Owner/repo:    $OWNER_REPO"
info "  Whitelist:     $WHITELIST"
[[ -n "$ITK_PR_NUMBER" ]] && info "  ITK PR ref:    #$ITK_PR_NUMBER"
$DRY_RUN          && info "  Mode:          --dry-run (no push, no archive flip)"
$NO_ARCHIVE_FLIP && info "  --no-archive-flip: will push but not archive"

# --------------------------------------------------------------------
# Operator gate: confirm the Phase A PR has merged.
# --------------------------------------------------------------------
if [[ -n "$ITK_PR_NUMBER" ]] && ! $DRY_RUN; then
  PR_STATE=$(gh pr view "$ITK_PR_NUMBER" \
    --repo InsightSoftwareConsortium/ITK \
    --json state,mergedAt --jq '"\(.state)  merged=\(.mergedAt // "no")"' \
    2>/dev/null) || PR_STATE="(could not query)"
  info "  ITK PR #$ITK_PR_NUMBER state: $PR_STATE"
  if [[ "$PR_STATE" != *"MERGED"* ]]; then
    die "ITK PR #$ITK_PR_NUMBER does not appear merged.  Phase B requires
the Phase A PR to be merged on ITK main first.  Aborting."
  fi
fi

# --------------------------------------------------------------------
# Work area
# --------------------------------------------------------------------
WORKDIR=$(mktemp -d "${TMPDIR:-/tmp}/archive-v4-$MODULE.XXXXXX")
cleanup() {
  if $KEEP_TEMPDIR; then
    info "Tempdir preserved at: $WORKDIR"
  else
    rm -rf "$WORKDIR"
  fi
}
trap cleanup EXIT

# --------------------------------------------------------------------
# Step 1: shallow clone of upstream (fresh — never the Phase A clone)
# --------------------------------------------------------------------
CLONE="$WORKDIR/$MODULE"
info "Cloning upstream into $CLONE (depth 50, fresh — not Phase A's clone)..."
git clone --quiet --depth 50 "$UPSTREAM_URL" "$CLONE"
DEFAULT_BRANCH=$(git -C "$CLONE" symbolic-ref --short HEAD 2>/dev/null || echo main)
info "  Default branch: $DEFAULT_BRANCH"

# --------------------------------------------------------------------
# Step 2: build the removal commit.  Walk the whitelist line-by-line,
# `git rm -rf` each path that exists.  Commit with a clear subject.
# --------------------------------------------------------------------
info "Removing whitelisted (now-migrated) files..."
REMOVED_COUNT=0
(
  cd "$CLONE"
  while IFS= read -r line; do
    line="${line%%#*}"           # strip comment
    line="${line//[$'\t\r ']/}"  # strip whitespace (paths shouldn't contain spaces)
    [[ -z "$line" ]] && continue
    # Globs are evaluated relative to the repo root.
    matches=( $(eval "ls -d $line 2>/dev/null" || true) )
    if (( ${#matches[@]} > 0 )); then
      git rm -rf "${matches[@]}" >/dev/null 2>&1 || true
      REMOVED_COUNT=$(( REMOVED_COUNT + ${#matches[@]} ))
    fi
  done < "$WHITELIST"
)

# Commit only if anything was removed
if (( REMOVED_COUNT == 0 )); then
  warn "No whitelisted paths matched in upstream; nothing to remove."
  warn "Skipping removal commit — proceeding to README promotion."
else
  COMMIT_MSG_RM="MIGRATE: Remove files migrated to ITK main"
  if [[ -n "$ITK_PR_NUMBER" ]]; then
    COMMIT_MSG_RM="$COMMIT_MSG_RM (ITK PR #$ITK_PR_NUMBER)"
  fi
  (
    cd "$CLONE"
    git -c user.name="ITK Migration Bot" \
        -c user.email="itk-migration@itk.org" \
        commit -m "$COMMIT_MSG_RM" \
        -m "These files have been ingested into the ITK main repository.
This repository is being archived; see README for details."
  ) || die "Removal commit failed"
  info "  Removed $REMOVED_COUNT path(s); commit: $COMMIT_MSG_RM"
fi

# --------------------------------------------------------------------
# Step 3: promote MIGRATION_README.md to README.md so the rendered
# landing page on GitHub shows the migration notice
# (per feedback_ingest_archive_readme.md).
#
# Cases handled:
#   (a) MIGRATION_README.md exists, README.rst (or other non-.md) exists
#       → mv README.rst -> info.rst ; mv MIGRATION_README.md -> README.md
#   (b) MIGRATION_README.md exists, README.md exists (already .md)
#       → mv MIGRATION_README.md README.md  (overwrites the old README;
#         keeping the original around as info.md preserves its history)
#   (c) MIGRATION_README.md does not exist
#       → warn — operator must add it manually before re-running.
# --------------------------------------------------------------------
PROMOTED=false
(
  cd "$CLONE"
  if [[ ! -f MIGRATION_README.md ]]; then
    warn "No MIGRATION_README.md in upstream root."
    warn "Add one (e.g., copying the LabelErodeDilate template) and re-run."
    exit 0
  fi
  if [[ -f README.rst ]]; then
    git mv README.rst info.rst
  elif [[ -f README.md ]]; then
    git mv README.md info.md
  fi
  git mv MIGRATION_README.md README.md

  COMMIT_MSG_DOC="DOC: Promote migration notice to README.md"
  git -c user.name="ITK Migration Bot" \
      -c user.email="itk-migration@itk.org" \
      commit -m "$COMMIT_MSG_DOC" \
      -m "GitHub renders README.md (or README.rst) on the landing page,
not MIGRATION_README.md.  Promoting the migration notice to README.md
ensures visitors to the archived repo see the redirect."
)
if [[ -f "$CLONE/README.md" ]] && grep -qi "migrat" "$CLONE/README.md" 2>/dev/null; then
  PROMOTED=true
fi

# --------------------------------------------------------------------
# Step 4: dry-run stops here
# --------------------------------------------------------------------
if $DRY_RUN; then
  info ""
  info "Dry-run complete.  Commits prepared in $CLONE."
  info "Inspect with:"
  info "  cd $CLONE && git log --oneline | head -5"
  exit 0
fi

# --------------------------------------------------------------------
# Step 5: push to upstream main (the irreversible step)
# --------------------------------------------------------------------
info "Pushing to upstream $DEFAULT_BRANCH..."
(
  cd "$CLONE"
  git push origin "HEAD:$DEFAULT_BRANCH"
) || die "Push to upstream failed.  Check authentication and write access."

# --------------------------------------------------------------------
# Step 6: verify GitHub renders the migration notice
# --------------------------------------------------------------------
info "Verifying GitHub-rendered README is the migration notice..."
RENDERED=$(gh api "repos/$OWNER_REPO/readme" --jq '.content' 2>/dev/null \
  | base64 -d 2>/dev/null | head -20 || true)
if [[ -z "$RENDERED" ]]; then
  warn "Could not fetch rendered README via gh api."
  warn "Manual check needed: https://github.com/$OWNER_REPO"
elif ! echo "$RENDERED" | grep -qi "migrat\|archive\|moved to ITK"; then
  warn "Rendered README does not appear to be the migration notice."
  warn "First lines:"
  printf '%s\n' "$RENDERED" | sed 's/^/  /' >&2
  die "README precedence verification failed (per feedback_ingest_archive_readme.md)."
else
  info "  OK: rendered README contains migration language."
fi

# --------------------------------------------------------------------
# Step 7: flip archived=true (skip if --no-archive-flip)
# --------------------------------------------------------------------
if $NO_ARCHIVE_FLIP; then
  info "Skipping archived=true flip per --no-archive-flip."
  info "When ready, run:"
  info "  gh api -X PATCH repos/$OWNER_REPO -F archived=true"
else
  info "Flipping archived=true on $OWNER_REPO..."
  gh api -X PATCH "repos/$OWNER_REPO" -F archived=true >/dev/null \
    || die "Failed to flip archived=true.  Check permissions on $OWNER_REPO."
  info "  OK: $OWNER_REPO is now archived."
fi

info ""
info "Phase B complete."
info "  Upstream removal commit pushed to $UPSTREAM_URL"
info "  README precedence: rendered README is the migration notice"
$NO_ARCHIVE_FLIP || info "  Repo state: archived"
