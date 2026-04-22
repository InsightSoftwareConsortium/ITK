# RemoteModuleIngest — tooling for bringing remote modules in-tree

This directory holds the scripts and planning documents used to move
ITK remote modules (configure-time `itk_fetch_module` declarations
under `Modules/Remote/`) into the ITK source tree at
`Modules/<Group>/<Module>/`, while preserving authorship and keeping
ITK's git pack small.

## Files

| File | Role |
|---|---|
| `ingest-remote-module.sh` | The driver.  One invocation per module: clones upstream, runs the whitelist `filter-repo` passes, writes the merge commit with `Co-authored-by:` trailers for every upstream contributor. |
| `INGESTION_STRATEGY.md` | The current-version strategy document.  Lives here so changes to the strategy are reviewable and versioned with the tree rather than scattered in PR comments. |
| `AUDIT_DESIGN.md` | Design notes for the pre-ingest audit pass (blob-size histogram, strip-candidate paths, copyright-review flag, recommended-mode logic). |
| `CLEANUP_CHECKLIST.md` | What to strip (history-wide or working-tree).  Mostly a safety net once the whitelist is in effect; still used for copyright review and for mode B residual blobs. |
| `AGENTS.md` | Guidance for AI coding agents running this workflow.  Describes preflight, decision points, and escalation criteria. |

## Quick start (human)

Prerequisites:

```bash
pixi global install git-filter-repo
# w3cli is only needed for CID normalization (step 4 below):
npm install -g @web3-storage/w3cli
```

Typical ingest:

```bash
cd <ITK-checkout>
git checkout -b ingest-<Module> upstream/main

# 1. Run the driver (creates the merge commit).
Utilities/Maintenance/RemoteModuleIngest/ingest-remote-module.sh \
    <Module> <DestGroup>

# 2. Add the module-level README that points at the archived upstream.
#    Template: see INGESTION_STRATEGY.md "Examples policy" section.
$EDITOR Modules/<DestGroup>/<Module>/README.md
git add Modules/<DestGroup>/<Module>/README.md
git commit -m "DOC: Add README.md pointing at archived upstream for <Module>"

# 3. Delete the remote fetch declaration.
git rm Modules/Remote/<Module>.remote.cmake
git commit -m "COMP: Remove <Module>.remote.cmake; now in-tree"

# 4. Opt the module into CI.
$EDITOR pyproject.toml  # add -DModule_<Module>:BOOL=ON to configure-ci
git add pyproject.toml
git commit -m "ENH: Enable <Module> in CI via configure-ci"

# 5. CID-normalize EVERY remaining content-link.  Mandatory before
#    push: the PR cannot ship with .md5 or .shaNNN stubs.  Use the
#    helper (guides you through `w3 login` + `w3 up` if needed):
Utilities/Maintenance/RemoteModuleIngest/cid-normalize.sh \
    Modules/<Group>/<Module>
git add Modules/<Group>/<Module>
git commit -m "STYLE: Convert <Module> content-links to .cid"

# 6. Verify every .cid content-link actually resolves via the
#    IPFS gateway — blocks pushes where test data is unreachable.
Utilities/Maintenance/RemoteModuleIngest/verify-cid-access.sh \
    Modules/<Group>/<Module>

# 7. Local build + test must pass (includes ExternalData fetch of
#    the converted .cid stubs, so this is the real end-to-end gate):
pixi run -e cxx configure-ci
pixi run -e cxx build
ctest --test-dir build -R <Module>

# 8. Only now: push and open the ITK ingest PR (one PR per module).
git push origin ingest-<Module>
gh pr create --draft --base main --title "ENH: Ingest ITK<Module> into Modules/<Group>"

# 9. After the ITK PR merges, open a follow-up PR on the ORIGINAL
#    upstream repo that (a) deletes the whitelisted files (one-
#    definition rule), (b) adds MIGRATION_README.md pointing at
#    the new ITK location, (c) states intent to archive the repo.
#    Template for the upstream MIGRATION_README.md:
#
#      # Migrated to ITK main
#      ...
#      https://github.com/InsightSoftwareConsortium/ITK/tree/main/Modules/<Group>/<Module>
#      ...
#
#    After the upstream PR merges, mark the repo "Archived" in
#    GitHub settings.  That completes the ingestion.
```

## Dry-run mode

Before actually running an ingest — especially for an unfamiliar
upstream — run the driver with `--dry-run`:

```bash
Utilities/Maintenance/RemoteModuleIngest/ingest-remote-module.sh \
    <Module> <DestGroup> --dry-run --keep-tempdir
```

This clones upstream, runs the whitelist passes into a tempdir, and
prints the post-filter inventory (surviving commit count, file count,
content-link count by algorithm) without touching the current ITK
checkout.  Inspect the tempdir before proceeding.

## One PR per module

Ingest PRs are predictably shaped.  One module per PR, four or five
commits:

1. `ENH: Ingest ITK<Module> into Modules/<Group>` (the whitelist-filtered merge)
2. `DOC: Add README.md pointing at archived upstream for <Module>`
3. `COMP: Remove <Module>.remote.cmake; now in-tree`
4. `ENH: Enable <Module> in CI via configure-ci`
5. *(optional)* `STYLE: Convert <Module> content-links from .md5 to .cid`

Reviewing these is then a matter of:

- Spot-checking that the whitelist transferred the right files.
- Verifying `git blame` walks across the merge boundary to original
  upstream authors (proves authorship preservation).
- Checking that the README accurately enumerates what was excluded.
- Confirming `configure-ci` builds the new module under CI.

## Where this came from

- **PR #6061** — first attempt: category repos.  Rejected.
- **PR #6085 / #6086** — second attempt: `Modules/Beta/` staging.
  Rejected.
- **PR #6093** (v1) — third attempt: direct-into-group with
  full-history merge.  Feedback from @blowekamp and @dzenanz
  (see comments) drove the v2/v3 revisions.
- **PR #6093** (v3) — current approach: whitelist + CID
  normalization + archived-upstream pointer README.
- **This PR** — extracts the tooling into tree.

## Open items

- **Examples relocation.**  Per @dzenanz, per-module `examples/` is
  routed to ITK's top-level `Examples/` through a separate follow-up
  PR, not ingested inline.  This directory does not yet include a
  relocator script; for the handful of modules that have meaningful
  examples, the relocation can be manual.
- **`cid-normalize.sh` automation depth.**  The current script walks
  the operator through `w3 login` + `w3 up` interactively for each
  `.md5`/`.shaNNN` content-link.  A future pass could (a) hash-
  check against already-pinned CIDs on web3.storage before
  uploading, and (b) batch the uploads rather than one-at-a-time.
  Not critical for the first wave of ingests.
