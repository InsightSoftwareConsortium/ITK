# AGENTS.md — Guidance for AI agents running remote-module ingestion

This file is written for AI coding agents (Claude Code, Cursor, GPT
Codex, similar) that are asked to ingest a remote ITK module into
the main source tree.  It complements `README.md` (human-focused)
and `INGESTION_STRATEGY.md` (policy-focused).  Read this before
running `ingest-remote-module.sh`.

## When to invoke this workflow

The user has asked you to:

- "Ingest `<Module>` into `Modules/<Group>/`"
- "Move `<Module>` from `Modules/Remote/` into the main tree"
- "Bring `<Module>` in-tree, preserving history"

If the user only says "build `<Module>`" or "enable `<Module>`",
they want `-DModule_<Module>:BOOL=ON` — not an ingest.  Don't start
an ingest unless the destination is `Modules/<Group>/<Module>/`.

## Pre-flight (mandatory)

1. **Confirm `git-filter-repo` is available**:
   ```
   command -v git-filter-repo
   ```
   If missing: `pixi global install git-filter-repo`.

2. **Confirm the working tree is clean** on an ITK checkout:
   ```
   git status --porcelain  # must be empty
   git rev-parse --abbrev-ref HEAD  # current branch
   ```

3. **Confirm you are NOT on `main`**.  Create a feature branch:
   ```
   git checkout -b ingest-<Module> upstream/main
   ```

4. **Confirm the upstream URL**.  The script reads it from
   `Modules/Remote/<Module>.remote.cmake` by default, but sanity-
   check by eyeball:
   ```
   grep GIT_REPOSITORY Modules/Remote/<Module>.remote.cmake
   ```

5. **Always dry-run first** for any module you haven't ingested before:
   ```
   Utilities/Maintenance/RemoteModuleIngest/ingest-remote-module.sh \
       <Module> <Group> --dry-run --keep-tempdir
   ```
   Inspect the tempdir output before the real run.  In particular:
   - `ls Modules/<Group>/<Module>/` — does the file set look right?
   - `grep -c '\.md5$' \| grep -c '\.cid$'` — content-link inventory?
   - `git log --oneline | wc -l` — surviving commit count sane?

## Decision points during an ingest

### 1. Does the upstream have a `LICENSE` that is NOT Apache-2.0?

Whitelist auto-excludes `LICENSE`.  If the upstream's license is not
Apache-2.0 (e.g., MIT, BSD-3-Clause, a dual license, or something
unusual), **stop and escalate to the human**.  ITK's root-level
license applies in-tree; non-Apache modules need a per-module
decision and possibly a `NOTICE` entry.  Do not silently strip a
non-Apache license.

### 2. Does the upstream have files outside the whitelist that
look necessary?

The whitelist is `include/`, `src/`, `test/`, `wrapping/`,
`CMakeLists.txt`, `itk-module.cmake`.  If the upstream relies on
e.g. `CMake/<Module>Targets.cmake.in` or a custom `<Module>Config.cmake`
at root, and the module won't build without it, widen the whitelist
for that invocation.  Document the widening in the merge commit body:

#### Important: directory-level whitelisting is not enough

`--path test` admits **everything** under `test/` in every historical
commit, including files with scaffolding-ish names that don't belong
in ITK.  Example caught on PR #6093: upstream
`ITKAnisotropicDiffusionLBR` had `test/azure-pipelines.yml` and
`test/Docker/Dockerfile` in several historical commits.  Those leaked
through the directory-level whitelist and were only caught by a
follow-up history-wide scan.

The driver handles this automatically via a **deny-pattern pass**
(step 3 in the script) that strips scaffolding filenames anywhere in
the filtered tree, followed by a **whitelist-verification scan**
(step 3b) that aborts the ingest if any scaffolding pattern is still
reachable in any commit.  You don't normally need to do anything
extra — but if the verification scan trips on a NEW pattern the
driver doesn't know about yet, add it to the `--path-glob` list in
step 3 and re-run.  Do not push an ingest where the verification
scan triggered a warning.

```
Whitelist passes (git filter-repo):
  - --path include --path src --path test --path wrapping
  - --path CMakeLists.txt --path itk-module.cmake
  - --path CMake/<Module>Config.cmake.in  # needed to resolve the
                                          # find_package() chain
```

### 3. Does the `test/` tree reference raw binary files (not
content-links)?

If `test/` contains `.png`, `.nrrd`, `.mha`, `.vtk`, etc., directly
(without a sibling `.md5` / `.cid`), those are raw binaries that
should either (a) be moved to the ExternalData fetch path (uploaded
to web3.storage, replaced with a `.cid` stub) or (b) be stripped
entirely.

**Do not merge an ingest that ships raw binary test assets.**
Either upload them via `w3 up` per
`Documentation/docs/contributing/upload_binary_data.md` and commit
the resulting `.cid` stubs, or escalate to the human.

### 4. Does the ingest produce any `.md5` or `.shaNNN`
content-links?

**CID conversion is mandatory.  It MUST complete before the PR is
pushed.  It is not optional, and it cannot be deferred to a
follow-up PR.**  The ingestion is incomplete until every
content-link is `.cid`.

Timing: the conversion may happen either immediately after the
merge commit (preferred: same session, same branch) or as a
fixup folded into the merge commit.  Either is fine.  What is
**not** fine is pushing the PR with `.md5` or `.shaNNN` stubs still
in the tree.

When the `ingest-remote-module.sh` driver finishes and
non-`.cid` content-links remain, it exits with code `2`
("action required") and prints the list of files.  Don't treat that
as a warning; treat it as a blocking pre-push gate.

The conversion workflow:

1. **If the agent has network access to web3.storage + the data
   mirrors:** run `Utilities/Maintenance/RemoteModuleIngest/cid-normalize.sh`
   on the ingested module path.  It reads each `.md5` / `.shaNNN`
   file, resolves the content via the ExternalData mirror, uploads
   the bytes via `w3 up` if not already pinned, writes the resulting
   `.cid` stub, and deletes the old hash file.  Commit as a
   single `STYLE: Convert content-links to .cid` commit.

2. **If the agent lacks network access** (common in sandboxed
   environments): do not push.  Instead, walk the human through the
   manual flow:
   - Install `@web3-storage/w3cli` per
     `Documentation/docs/contributing/upload_binary_data.md`.
   - Run `w3 login <email>` and `w3 up <file>` for each content
     referenced by the `.md5` / `.shaNNN` stubs.
   - Write each returned CID to the sibling `.cid` file; delete the
     old hash file.
   - Re-invoke the driver's post-merge verification to confirm all
     content-links are now `.cid`.

3. **Before pushing the PR**, the agent must run
   `verify-cid-access.sh` (in this directory) which fetches each
   `.cid` through an IPFS gateway to confirm the content is
   actually retrievable.  A local build + test cycle must also
   succeed — the ingested module's test targets must fetch and
   pass using the new `.cid` stubs.  No shortcuts here; a passing
   CI run on an in-tree module that can't resolve its test data
   is worse than a red CI check.

### 5. Does the upstream have an `examples/` directory?

The whitelist auto-excludes `examples/`.  Per @dzenanz on
PR #6093, per-module examples do NOT ingest inline.  Three options
for the human to pick from:

- **Archive only** (default): leave `examples/` in the archived
  upstream repo.
- **Relocate**: open a separate follow-up PR that copies useful
  examples into `InsightSoftwareConsortium/ITK/Examples/<Module>/`,
  with their own CMake test registration.
- **Ignore**: if the examples are clearly obsolete, do nothing.

The AI agent should **never decide (b) unilaterally** — present the
options to the human and get direction.  The ingest PR itself does
not relocate examples.

### 6. Is the audit's recommended mode `squash`?

Under the v3 whitelist, `squash` is rare.  If the audit recommends
it, double-check the audit output.  Likely causes:

- Whitelisted paths legitimately contain a big file (e.g., a
  `test/Baseline/huge_file.nrrd` without a content-link — escalate
  per decision 3).
- Upstream has hundreds of automated commits in whitelisted paths
  (e.g., a dependabot loop that churned `test/CMakeLists.txt`).
  Escalate to human for a threshold discussion.

Do not squash silently; always get human confirmation first.

### 7. After the ITK ingest PR merges — upstream archival PR

**The ingestion workflow is not complete when the ITK ingest PR
merges.**  The final step, which must happen on the ORIGINAL
upstream repo, enforces the one-definition rule: any file that
now lives at `Modules/<Group>/<Module>/` in ITK should not also
live at the upstream's tree tip.

Open a PR on the upstream repo that:

1. **Deletes every whitelisted file** from the upstream tree tip
   (i.e., `include/`, `src/`, `test/`, `wrapping/`, `CMakeLists.txt`,
   `itk-module.cmake` — the same set the ingest transferred).
2. **Adds a `MIGRATION_README.md`** at the upstream repo root that
   directs future readers to the new in-tree location.  Template:

   ```markdown
   # Migrated to ITK main

   As of <date>, the `<Module>` module has been ingested into
   the main ITK source tree.  The authoritative location is now:

     https://github.com/InsightSoftwareConsortium/ITK/tree/main/Modules/<Group>/<Module>

   See `Modules/<Group>/<Module>/README.md` in the ITK tree for
   details on what moved and what remains in this archived repo.

   This repository is retained read-only for historical reference
   (deep git history, paper material, example assets not migrated
   to ITK).  It will be marked ARCHIVED after this PR merges.

   Related:
   - ITK ingest PR: InsightSoftwareConsortium/ITK#<NNNN>
   - Consolidation issue: InsightSoftwareConsortium/ITK#6060
   ```
3. **Explicitly states the post-merge intent to archive** the
   repository via GitHub's repository-settings → Danger Zone →
   "Archive this repository".

When the upstream maintainer merges that PR and archives the repo,
the ingestion is complete: deep history remains reachable at a
read-only URL, ITK carries the whitelisted authoritative copy, and
users who clone either side see an unambiguous pointer to the
other.

AI agents **must prompt the human to open this upstream PR** as
the final step of the workflow.  The agent cannot open the PR
itself (different repo, different permissions in most cases) but
should draft the `MIGRATION_README.md` text and the removal diff
for the human to push.

## Post-ingest validation

After the merge commit lands, run:

```bash
# 1. `git blame` walks across the merge boundary to upstream authors.
git blame Modules/<Group>/<Module>/include/<first-header>.h | head

# 2. Author set is preserved.
git log --format='%an <%ae>' Modules/<Group>/<Module>/ | sort -u

# 3. No upstream-only paths leaked in.
find Modules/<Group>/<Module> -name '.github' -o -name 'pyproject.toml' \
     -o -name 'paper' -o -name 'Old' -o -name 'CTestConfig.cmake'
# Expect: no output.

# 4. CI builds the module with the configure-ci opt-in.
pixi run -e cxx configure-ci  # must succeed
pixi run -e cxx build          # must succeed
```

## What NOT to do

- **Don't `git filter-repo --to-subdirectory-filter` without the
  whitelist first.**  That reintroduces the v1 bloat.
- **Don't run multiple ingests in one PR.**  One module per PR.
- **Don't edit the merge commit message after the fact to add
  authorship.**  `Co-authored-by:` trailers are generated from
  upstream git log by the script; if an author is missing, they're
  missing from upstream too.
- **Don't force-push an ingest PR.**  Once the merge commit is in,
  amend via fixup commits or add follow-on commits instead.  Rewrites
  break the `git blame` walk for anyone who has the old SHA cached.
- **Don't silently widen the whitelist.**  If you need to admit an
  extra path, document it in the merge commit body with a reason.
- **Don't push the PR with `.md5` / `.shaNNN` content-links still
  in the tree.**  CID conversion is mandatory pre-push.  If you
  can't complete it because of network restrictions, stop and hand
  back to the human.
- **Don't skip the local build + test gate.**  The ingested module
  must configure, build, and run its tests locally — with the
  new `.cid` stubs resolving to actual content — before the PR is
  pushed.  A green CI that can't resolve test data is a trap for
  the next reviewer, not a pass.
- **Don't skip the upstream-archival PR.**  The workflow is
  not complete when ITK's ingest PR merges; the upstream repo
  needs its own follow-up PR that deletes the migrated files and
  adds `MIGRATION_README.md` before being archived (decision 7).

## Escalation triggers

Hand back to the human immediately if:

- The upstream license is not Apache-2.0.
- `test/` contains raw binary assets (not behind content-links).
- The audit recommends `squash` mode.
- `git blame` fails to walk across the merge boundary after the
  merge commit (indicates a filter-repo misconfiguration).
- Any `.pdf`, `.mp4`, or `> 1 MiB` image survives the whitelist.
- The inferred upstream URL doesn't match what the human expects.
- Network access is unavailable and `.md5` / `.shaNNN` content-links
  need CID conversion.  The agent cannot push an ingest PR with
  non-`.cid` stubs present — stop and hand back.
- Local build or test fails after the module is in-tree with the
  new `.cid` stubs.  A broken ingest must not be pushed; diagnose
  (likely a `.cid` that never resolved, a missing dependency in
  `itk-module.cmake`, or a whitelist widening that's needed) before
  retrying.
- The human has not been shown the draft upstream-archival PR
  content (`MIGRATION_README.md` + removal diff) — decision 7
  requires explicit human action on a different repo, so the agent
  must surface the draft rather than silently complete.

## References

- `INGESTION_STRATEGY.md` in this directory — policy document.
- `AUDIT_DESIGN.md` in this directory — what the audit reports.
- `CLEANUP_CHECKLIST.md` in this directory — what to strip.
- `Documentation/docs/contributing/upload_binary_data.md` — the
  `@web3-storage/w3cli` workflow for CID normalization.
- ITK commit [`f3899ce8c6`](https://github.com/InsightSoftwareConsortium/ITK/commit/f3899ce8c6)
  — `.md5`/`.sha512` → `.cid` migration precedent.
