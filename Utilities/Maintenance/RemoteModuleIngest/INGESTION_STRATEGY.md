# Remote Module Ingestion Strategy

Transition plan for converting ITK remote modules (configure-time fetch)
into inline source under their appropriate module group
(`Modules/Filtering/`, `Modules/IO/`, etc.), while protecting the main
ITK git history from object bloat.

## Approach history

| Attempt | Destination | Outcome |
|---------|-------------|---------|
| PR #6061 | Category repos (`ITKRemoteAnalysis`) | Rejected — thewtex: "complicates ownership" |
| PR #6085/6086 | `Modules/Beta/<Name>/` staging | Rejected — thewtex: "unstable location, not logically connected" |
| PR #6093 (v1) | `Modules/<Group>/<Name>/` + full-history merge | **Revised** — blowekamp: full-history import risks git-object bloat; papers, "Old/" trees, and demo assets from upstream dev history get dragged into ITK permanently |
| **Current (v2)** | `Modules/<Group>/<Name>/` + **audited ingest**, with selective filter-repo or squash based on pre-ingest bloat metrics | Agreed |

## What changed from v1 → v2

Driven by feedback from @blowekamp on PR #6093:

> *I don't think merging the full history is a good idea to all remote
> repositories. The remote repos have not had the same rigor as the main
> repo, larger file (such as those used for papers may have been
> included, or perhaps some copyright material). There could also have
> been a long initial development period too. If this is done with most
> remotes then I think the ITK git history may become too bloated.*

v2 adds a **pre-ingest audit** step that measures what a merge would
cost and picks one of three ingest modes per module based on thresholds.

## Decisions (updated 2026-04-22)

1. **Destination (unchanged):** Each module goes directly into its
   existing group — `Modules/Filtering/`, `Modules/IO/`,
   `Modules/Segmentation/`, `Modules/Registration/`, etc. No staging
   directory.
2. **Pre-ingest audit (new).** Before any merge, run
   `filter-repo --analyze` on the upstream clone and emit a bloat
   report. The report determines which ingest mode runs.
3. **Three ingest modes (new).** `full-history`, `filtered-history`,
   or `squash-to-one-commit` — the mode is picked automatically from
   the audit, or overridden on the CLI after human review.
4. **Attribution floor (new).** Whichever mode runs, every ingest
   preserves at least:
   - primary author (top contributor by commits, in the commit author)
   - co-authors (every other contributor as a `Co-authored-by:` trailer)
   - upstream URL + upstream tip SHA in the commit body
   - upstream repo stays archived (read-only) on GitHub after ingest so
     the full history is reachable by anyone who needs it
5. **Upstream-tip first, then ingest (unchanged).** Bump `GIT_TAG` to
   the current upstream tip before ingesting so the structural change
   has no behavior delta.
6. **One PR per module (unchanged).** Predictable, boring, reviewable.

## Bloat thresholds (defaults — aggressive: keep ITK's pack small)

The audit compares measured quantities against these defaults; any
single failure escalates the mode.  Values are deliberately low so the
default ingest mode skews toward **squash**, because protecting ITK's
pack matters more to us than preserving upstream commit granularity.

| Metric | Full-history OK if ≤ | Filtered OK if ≤ | Else |
|---|---|---|---|
| Total pack size delta (estimated from blob set) | 700 KiB | 3 MiB | → squash |
| Largest single blob introduced | 85 KiB | 700 KiB | → squash or strip |
| Blobs over 341 KiB | 0 | ≤ 1 (only if a test fixture with `.md5` + ExternalData hooks) | → strip or squash |
| Commits touching only `Old/`, `paper/`, `doc/figures/` | n/a | n/a | structurally excluded by the whitelist (these paths never enter ITK's history) |

**Commit count is not a threshold.**  Agreed with @dzenanz on PR
#6093: once the whitelist has stripped `Old/`, `paper/`, `docs/`,
demos, CI scaffolding, and packaging scaffolding from history, the
surviving commit count reflects real module authorship and should
not by itself push a module into filtered or squash mode.  Modules
with hundreds of genuine upstream commits (bug fixes, maintenance,
multiple authors over years) retain that granularity under
Mode A as long as the size metrics above are satisfied.

*(Starting values, tightened on 2026-04-22 per `#6093` direction to
"keep thresholds very low".  Every size bar is roughly 1/3 of what it
was in the first draft.  Revisit after the first ~5 ingests with real
measured pack deltas and tune.)*

## The transfer whitelist (all modes share this)

**Only the paths in this whitelist cross from the archived upstream
repo into ITK's history.  Everything else stays in the upstream repo
permanently.**  The whitelist is applied as a `git filter-repo --paths`
pass — *not* an invert-paths pass — so a surprise new path in some
future upstream never leaks in.  This answers the PR #6093 feedback
from @dzenanz ("only code/headers/tests") and @blowekamp ("protect
ITK history from bloat") at the structural level rather than via
thresholds on the old blacklist approach.

| Whitelist entry | Rationale |
|---|---|
| `include/` | The module's public headers — what downstream ITK consumers compile against |
| `src/` | Non-template source, if the module ships any |
| `test/` | GoogleTest / CTest drivers.  Test *data* is normalized to `.cid` content-links in a separate pass (see "CID normalization" below); raw binary test assets never land in git. |
| `wrapping/` | ITK's Python / Java wrapping descriptors; keep if present |
| `CMakeLists.txt` at module root | Build description |
| `itk-module.cmake` at module root | Module registration (`DEPENDS`, `TEST_DEPENDS`, `COMPLIANCE_LEVEL`) |
| `*.cmake` at module root | `<Name>Config.cmake.in`, custom module helpers — if referenced from the two above |

Explicitly **not** in the whitelist (stays in the archived upstream
forever):

- `README*`, `CHANGELOG*`, `HISTORY*` — superseded by git log + ITK root docs
- `LICENSE*` — ITK root covers Apache-2.0 modules; non-Apache modules are caught by the copyright-review flag and handled per-case
- `.github/`, `.gitlab-ci.yml`, `.circleci/`, `.travis.yml`, Azure pipelines
- `pyproject.toml`, `setup.py`, `setup.cfg`, `requirements*.txt`
- `CTestConfig.cmake`, `.clang-format`, `.clang-tidy`, `.pre-commit-config.yaml`
- `docs/`, `doc/`, `paper/`, `papers/`, `publication/`, `presentations/`
- `Old/`, `legacy/`, `archive/` — pre-refactor trees
- `example/` or `examples/` — routed separately (see "Examples policy" below)
- `Dockerfile*`, `docker/`, `.dockerignore`
- `demo*/`, `media/`, `movies/`, `screencasts/`

Because the whitelist is narrow, the per-module post-merge `STYLE:
Remove non-ITK artifacts` commit collapses to a no-op in the common
case.  It only survives as a safety net for edge cases (e.g., a whitelisted
`test/` directory that happens to contain an upstream-only `README.md`).

### Deny-pattern pass (mandatory second filter-repo pass)

The directory-level whitelist is necessary but not sufficient.
`--path test` admits **everything** under `test/` — including
scaffolding files that some upstreams place inside whitelisted
directories.  Example caught on PR #6093: upstream
`ITKAnisotropicDiffusionLBR` had `test/azure-pipelines.yml` (6
historical commits) and `test/Docker/Dockerfile` (8 commits) living
under the whitelisted `test/` tree.  Both survived the whitelist
pass and were only caught by a follow-up history-wide audit.

The driver applies a second `git filter-repo` pass immediately after
the whitelist that strips well-known scaffolding filenames from any
path inside the module tree:

```
git filter-repo --invert-paths \
  --path-glob 'Modules/<Group>/<Module>/**/CTestConfig.cmake' \
  --path-glob 'Modules/<Group>/<Module>/**/azure-pipelines*.yml' \
  --path-glob 'Modules/<Group>/<Module>/**/Dockerfile' \
  --path-glob 'Modules/<Group>/<Module>/**/Dockerfile.*' \
  --path-glob 'Modules/<Group>/<Module>/**/.dockerignore' \
  --path-glob 'Modules/<Group>/<Module>/**/docker/*' \
  --path-glob 'Modules/<Group>/<Module>/**/.docker/*' \
  --path-glob 'Modules/<Group>/<Module>/**/Jenkinsfile' \
  --path-glob 'Modules/<Group>/<Module>/**/.circleci/*' \
  --path-glob 'Modules/<Group>/<Module>/**/circle.yml' \
  --path-glob 'Modules/<Group>/<Module>/**/.travis.yml' \
  --path-glob 'Modules/<Group>/<Module>/**/appveyor.yml' \
  --path-glob 'Modules/<Group>/<Module>/**/.cirun.yml' \
  --path-glob 'Modules/<Group>/<Module>/**/.gitlab-ci.yml' \
  --path-glob 'Modules/<Group>/<Module>/**/.github/*' \
  --path-glob 'Modules/<Group>/<Module>/**/codecov.yml' \
  --path-glob 'Modules/<Group>/<Module>/**/tox.ini' \
  --path-glob 'Modules/<Group>/<Module>/**/pyproject.toml' \
  --path-glob 'Modules/<Group>/<Module>/**/setup.py' \
  --path-glob 'Modules/<Group>/<Module>/**/setup.cfg' \
  --path-glob 'Modules/<Group>/<Module>/**/MANIFEST.in' \
  --path-glob 'Modules/<Group>/<Module>/**/requirements*.txt' \
  --path-glob 'Modules/<Group>/<Module>/**/environment*.yml' \
  --path-glob 'Modules/<Group>/<Module>/**/.clang-format' \
  --path-glob 'Modules/<Group>/<Module>/**/.clang-tidy' \
  --path-glob 'Modules/<Group>/<Module>/**/.pre-commit-config.yaml' \
  --prune-empty always
```

### History-wide whitelist verification (mandatory)

After both filter-repo passes, the driver scans the entire ingested
history (not just the tree tip) for any remaining path whose basename
matches a scaffolding pattern.  If any match is found, the driver
aborts with a non-zero exit and prints the leaked paths — the ingest
must not be pushed.  The scan is equivalent to running the standalone
`verify-whitelist-history.sh` helper and is what would have caught
the PR #6093 leaks before the first push.

## CID normalization (mandatory; runs on every ingest)

Every test-data content-link inside `test/` is normalized to `.cid`
format before the merge lands in ITK.  Per direction on PR #6093:

> All md5 / sha256 / sha512 content links → convert to the preferred
> `.cid` format.

Hash algorithms seen in upstream remote modules today:

- `.md5` — ITK's legacy format
- `.sha1`, `.sha224`, `.sha256`, `.sha384`, `.sha512` — rarer but supported by `CMake/ExternalData.cmake`
- `.cid` — the current preferred format (IPFS Content Identifier v1 /
  CIDv1), adopted in ITK 5.4 (`f3899ce8c6`)

Conversion pipeline, applied as a `git filter-repo --blob-callback`:

1. **Existing hash content-link** (`.md5` / `.shaNNN`) — resolve against
   the current ExternalData fetch mirrors to retrieve the referenced
   blob, compute the IPFS CIDv1 (base32, raw codec), write a new
   `<path>.cid` content-link beside (or in place of) the old one,
   and delete the old hash file.  Content is byte-identical; only
   the pointer format changes.
2. **Raw binary test asset with no content-link** — upload to IPFS via
   `npm install -g @web3-storage/w3cli && w3 up <file>` (the "npm way"
   referenced in @dzenanz's comment on #6093 and documented in
   `Documentation/docs/contributing/upload_binary_data.md`); write
   the resulting CID to `<path>.cid`; delete the raw binary.
3. **Already `.cid`** — verify the CID resolves and carry it forward
   unchanged.

**Timing: CID conversion must complete before the ingest PR is
pushed.**  It is not optional, not deferrable, and not left as a
"TODO before merge" note.  Acceptable timings:

- Run `cid-normalize.sh` immediately after the merge commit lands
  (same session, same branch) — produces a single `STYLE: Convert
  content-links to .cid` commit.
- Fold the conversion directly into the merge commit as part of the
  ingest (harder to review but cleaner history).

*Not* acceptable: pushing the ingest PR with `.md5` / `.shaNNN`
stubs still present with the expectation that a later tree-wide
sweep will clean them up.  That leaves a window where ITK's tree
tip mixes old and new content-link formats, which confuses
downstream consumers and ExternalData fetch logic.

Pre-push gate: `verify-cid-access.sh` walks every `.cid` stub in
the ingested module and confirms it resolves via the configured
IPFS gateway.  A local `pixi run -e cxx configure-ci && build` +
`ctest -R <Module>` cycle must also succeed — the ingested test
targets must actually resolve their data through the converted
`.cid` stubs.  Pushing a PR that CI will go green on only because
the data-fetch step gets skipped is worse than pushing a red PR.

If `cid-normalize.sh` would need to upload a file > 100 MiB
(web3.storage free-tier ceiling), it stops and asks the human to
upload out-of-band and paste the resulting CID.

## Upstream archival PR (mandatory final step)

The ingestion workflow is **not complete** when the ITK ingest PR
merges.  The final step — opened on the ORIGINAL upstream remote-
module repository — enforces the one-definition rule: any file
that now lives at `Modules/<Group>/<Name>/` in ITK should no
longer live at the upstream repo's tree tip.

The upstream archival PR:

1. **Deletes every whitelisted file** from upstream's tree tip
   (the same set that transferred during ingest: `include/`,
   `src/`, `test/`, `wrapping/`, `CMakeLists.txt`,
   `itk-module.cmake`).
2. **Adds `MIGRATION_README.md`** at upstream's root, pointing at
   the authoritative in-tree ITK location and linking the ingest
   PR.  Template:

   ```markdown
   # Migrated to ITK main

   As of <date>, the `<Module>` module has been ingested into
   the main ITK source tree.  The authoritative location is:

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

3. **States intent to archive** the repository once the PR merges.
   The upstream maintainer then goes to GitHub → repository
   settings → Danger Zone → "Archive this repository".

What users see after both PRs merge and the upstream is archived:

- Cloning ITK gets the authoritative module in
  `Modules/<Group>/<Name>/`.
- Cloning the archived upstream gets only `MIGRATION_README.md`
  pointing back at ITK, plus everything that was deliberately left
  behind (`Old/`, `paper/`, `examples/`, docs, full pre-ingest
  history, etc. still reachable for anyone who needs it).

The upstream archival PR typically ships in the same session as the
ITK ingest PR — once the ingest is pushed, the agent drafts the
archival PR content (removal diff + `MIGRATION_README.md`) and
hands it to the human for pushing to the upstream repo.  The agent
should not push to the upstream repo itself (different repo,
different permissions).

## Examples policy

Upstream `examples/` / `example/` directories are **not** whitelisted
into `Modules/<Group>/<Name>/`.  Per @dzenanz's suggestion on PR #6093,
per-module example directories live at ITK's top-level
`Examples/` tree.  For each module, the ingestor picks:

- **(a) Archive only** — leave `examples/` in the archived upstream
  repo; no ingestion.  This is the default.
- **(b) Relocate** — open a *separate* follow-up PR that moves the
  `examples/` contents into `InsightSoftwareConsortium/ITK/Examples/<Name>/`
  with their own CMake test registration and any test data also
  normalized to `.cid`.

The ingest PR itself never relocates examples — that would confuse the
reviewer about what changed.  The follow-up example-relocation PR, if
any, ships on its own merits.

## The three ingest modes

### Mode A — full-history merge

Use when the audit is clean after the whitelist + CID normalization
passes have run: ≤ 700 KiB pack delta, no surviving blob > 85 KiB.
(Commit count is no longer a gate — see the threshold table above.)

```
1. COMP:  Bump <Name> to upstream/<branch> tip
2. ENH:   Ingest ITK<Name> into Modules/<Group>   (merge commit of whitelisted + CID-normalized history)
3. COMP:  Remove <Name>.remote.cmake
4. COMP:  Fix pre-commit hook failures
5. STYLE: Remove non-ITK artifacts from ingested <Name>   (usually empty; kept as safety net)
```

Commit 2 is a `git merge --allow-unrelated-histories --no-ff` of a
`filter-repo`-rewritten clone whose passes are:

1. `--paths include/ src/ test/ wrapping/` (plus root `CMakeLists.txt`,
   `itk-module.cmake`, any `*.cmake`).  This is the **whitelist**.
2. `--to-subdirectory-filter Modules/<Group>/<Name>/`
3. `--blob-callback` — CID normalization per the section above.

`git blame` walks across the merge into original authors on every
whitelisted file.

### Mode B — filtered-history merge

Use when the whitelist + CID normalization alone leave too many
commits or too much pack data (exceeds full-history caps but stays
under filtered caps).  Adds one more filter-repo pass:

4. `--strip-blobs-bigger-than <cap>` on whatever survives the
   whitelist (default 341 KiB — anything bigger than that in code or
   test-driver paths is almost certainly a copy-pasted screenshot
   that shouldn't be there).

```
1. COMP:  Bump <Name> to upstream/<branch> tip
2. ENH:   Ingest ITK<Name> into Modules/<Group>
          (merge commit body lists each filter pass; whitelist set;
           CID-normalization summary; blob cap)
3. COMP:  Remove <Name>.remote.cmake
4. COMP:  Fix pre-commit hook failures
5. STYLE: Remove non-ITK artifacts from ingested <Name>   (usually empty)
```

`git blame` still walks across the merge on surviving files.

### Mode C — squash-to-one-commit

Used when the audit shows the upstream history is too noisy / too
large / too ownership-ambiguous to carry into ITK even with filter-repo
(large standalone-dev period, thousands of commits, mixed authorship
where some authors never appear in the latest tip).

```
1. COMP: Bump <Name> to upstream/<branch> tip
2. ENH:  Ingest ITK<Name> into Modules/<Group>
         (single commit; author = primary upstream contributor;
          body lists every other contributor as Co-authored-by;
          body links the archived upstream repo + tip SHA)
3. STYLE: Remove non-ITK artifacts from ingested <Name>
4. COMP: Remove <Name>.remote.cmake
5. COMP: Fix pre-commit hook failures
```

`git blame` on ingested files lands on the squash commit for the final
author and reports the ingester as the committer; original attribution
lives in the commit body and in the archived upstream repo.

Squash-commit body template:

```
ENH: Ingest ITK<Name> into Modules/<Group>

Imports the <Name> remote module from
<upstream_url> at tip <upstream_sha> (ingest-date 2026-MM-DD).
Upstream history is squashed into this single commit to avoid
introducing <N> commits / <size> of pack data that the audit flagged
as mostly paper/demo/Old-tree material with no ongoing maintenance
value.  The archived upstream repo remains read-only at the URL above
for anyone who needs the deep history.

Contributors surfaced from the upstream git log of the ingested tree
(commits / lines-touched):

  Jane Author <jane@example.org>           (primary; 143 commits, 8.2 kLOC)
  Bob Helper <bob@example.org>              (23 commits, 1.1 kLOC)
  ...

Co-authored-by: Jane Author <jane@example.org>
Co-authored-by: Bob Helper <bob@example.org>
...
```

## Pre-ingest audit — what it emits

The audit runs on a freshly-cloned upstream repo (before any
`to-subdirectory-filter`). Human-readable output plus a machine-readable
`audit.json`. Example (hypothetical ITKAnisotropicDiffusionLBR):

```
=== Upstream audit: ITKAnisotropicDiffusionLBR @ 203260b9 ===
Commits:                 136
Distinct authors:        12
Tree size (tip):         2.1 MiB
Pack estimate (all):     7.8 MiB  ← would be added to ITK
Largest blobs ever introduced:
   4.1 MiB  paper/figures/brain_section.png      (commit a1b2c3d)
   2.2 MiB  Old/legacy_impl/DiffusionOldTest.mha (commit 9f8e7d6)
   0.8 MiB  examples/Data/Fiber.png              (commit c4d5e6f)
   ... (17 more ≥ 85 KiB)
Size histogram of added blobs:
   0–10 KiB   :  487 blobs
   10–100 KiB :   42 blobs
   100 KiB–1 MiB :  18 blobs
   1–10 MiB   :    4 blobs
   > 10 MiB   :    0 blobs
Paths that exist only in pre-tip history (candidate strip set):
   Old/              (87 commits, 1.4 MiB pack)
   paper/figures/    (12 commits, 4.3 MiB pack)
   docs/anim/        ( 5 commits, 0.6 MiB pack)

Recommended mode: filtered-history
  rationale: pack delta 7.8 MiB > 3 MiB filtered cap before stripping;
             after stripping Old/ paper/ docs/anim/ the post-filter
             pack delta is 1.3 MiB (still > 700 KiB full-history cap
             but < 3 MiB filtered cap), so filtered-history mode
             applies.  Surviving commit count (108) is not a gate.
  proposed squash commit author: Jean-Marie Mirebeau <...>
  proposed Co-authored-by trailers: 11 others
```

## Post-ingest metrics

Every ingest appends a block to `INGEST_LOG.md` capturing actual
measured impact on the ITK repo:

```
## AnisotropicDiffusionLBR — 2026-04-22
  mode:              filtered-history
  upstream_url:      https://github.com/InsightSoftwareConsortium/ITKAnisotropicDiffusionLBR.git
  upstream_sha:      203260b9...
  upstream_commits:  136 (108 surviving after filter)
  authors_preserved: 12 (all surfaced in git log)
  pack_size_before:  <byte count>
  pack_size_after:   <byte count>
  pack_delta:        +1.28 MiB
  largest_added_blob: include/itkStructureTensorImageFilter.hxx (12.3 KiB)
  filter_passes:
    - invert-paths: Old/ paper/ docs/anim/
    - strip-blobs-bigger-than: 1M
  stripped_files:    <count>, <cumulative size>
  stripped_commits:  <count>
  pr:                #6093
```

## Automated workflow

`ingest-remote-module.sh` grows a pre-ingest audit phase and the three
modes.

```bash
./ingest-remote-module.sh <ModuleName> <DestGroup> [OPTIONS]

# Run the audit and print the recommendation without modifying anything:
./ingest-remote-module.sh AnisotropicDiffusionLBR Filtering --audit-only

# Accept the audit's recommendation:
./ingest-remote-module.sh AnisotropicDiffusionLBR Filtering

# Override the recommendation (after human review):
./ingest-remote-module.sh AnisotropicDiffusionLBR Filtering --mode=full-history
./ingest-remote-module.sh AnisotropicDiffusionLBR Filtering --mode=filtered-history \
    --strip-paths 'Old/' 'paper/' \
    --strip-blobs-bigger-than=1M
./ingest-remote-module.sh AnisotropicDiffusionLBR Filtering --mode=squash
```

Flag additions:

- `--audit-only` — print the report + recommendation; exit 0.
- `--mode={full-history,filtered-history,squash}` — override the
  auto-recommendation. Refuses to proceed silently if the mode
  disagrees with the recommendation — must be accompanied by `--force`
  or an explanation in `--mode-justification "..."` which ends up in
  the merge commit body.
- `--strip-paths <paths...>` — filter-repo `--invert-paths --paths`
  pass (mode B only).
- `--strip-blobs-bigger-than N` — filter-repo blob cap (mode B only);
  default 1M, set `none` to disable.
- `--max-pack-delta N` — hard limit; abort if the post-filter pack
  delta exceeds this (default 10M).

## Manual steps (before and after script)

**Before:**
1. Create branch: `git checkout -b ingest-<Name> upstream/main`
2. Ensure git-filter-repo is installed
3. Ensure working tree is clean
4. **(new)** Run `--audit-only` first; eyeball the recommendation
   and the strip-path list before committing to a mode

**After:**
1. Local build: `pixi run -e cxx build` (or cmake --build)
2. Run module tests: `ctest --test-dir <build> -R <Name>`
3. **(new)** Verify `INGEST_LOG.md` block was appended and the
   `pack_delta` is consistent with the audit estimate
4. Push and open PR (one module per PR)
5. After merge: request upstream repo be archived on GitHub

## Verification after each ingest

```bash
# Authors preserved regardless of mode:
git log --format='%an <%ae>' -- Modules/<Group>/<Name>/ | sort -u
# For modes A/B: expect the upstream contributor set.
# For mode C: expect ingester as author with Co-authored-by trailers
#             in the squash commit body listing the full set.

# Blame walks across the merge boundary (modes A/B):
git blame Modules/<Group>/<Name>/include/<some-header>.h | head

# No itk_fetch_module remains:
! git grep -n "itk_fetch_module" Modules/Remote/ | grep -i <Name>

# Pack delta consistent with the audit (all modes):
grep -A6 "^## <Name>" INGEST_LOG.md
```

## Module destination map (Tier A — pure ITK, no external deps)

*(Map unchanged; only the ingest mode per row is new, and is filled in
after each audit runs. "TBD" means audit hasn't been run yet.)*

| Module | Group | Priority | Audit | Mode |
|--------|-------|----------|-------|------|
| AnisotropicDiffusionLBR | Filtering | Wave 1 | pending | TBD |
| FastBilateral | Filtering | Wave 1 | pending | TBD |
| LabelErodeDilate | Filtering | Wave 1 | pending | TBD |
| GenericLabelInterpolator | Filtering | Wave 1 | pending | TBD |
| SplitComponents | Filtering | Wave 1 | pending | TBD |
| PolarTransform | Filtering | Wave 1 | pending | TBD |
| MultipleImageIterator | Filtering | Wave 1 | pending | TBD |
| HigherOrderAccurateGradient | Filtering | Wave 1 | pending | TBD |
| ParabolicMorphology | Filtering | Wave 1 | pending | TBD |
| MorphologicalContourInterpolation | Filtering | Wave 1 | pending | TBD |
| SmoothingRecursiveYvvGaussianFilter | Filtering | Wave 1 | pending | TBD |
| Cuberille | Filtering | Wave 1 | pending | TBD |
| MeshNoise | Filtering | Wave 1 | pending | TBD |
| SubdivisionQuadEdgeMeshFilter | Filtering | Wave 1 | pending | TBD |
| IOMeshSTL | IO | Wave 1 | pending | TBD |
| IOMeshMZ3 | IO | Wave 1 | pending | TBD |
| IOFDF | IO | Wave 1 | pending | TBD |
| BoneEnhancement | Filtering | Wave 2 | pending | TBD |
| BoneMorphometry | Filtering | Wave 2 | pending | TBD |
| TextureFeatures | Filtering | Wave 2 | pending | TBD |
| IsotropicWavelets | Filtering | Wave 2 | pending | TBD |
| Montage | Filtering | Wave 2 | pending | TBD |
| GrowCut | Segmentation | Wave 2 | pending | TBD |
| RANSAC | Registration | Wave 2 | pending | TBD |
| VariationalRegistration | Registration | Wave 2 | pending | TBD |
| Thickness3D | Filtering | Wave 2 | pending | TBD |
| Strain | Filtering | Wave 2 | pending | TBD |
| PhaseSymmetry | Filtering | Wave 2 | pending | TBD |
| SimpleITKFilters | Filtering | Wave 2 | pending | TBD |
| IOScanco | IO | Wave 2 | pending | TBD |
| MGHIO | IO | Wave 2 | pending | TBD |

## References

- `ingest-remote-module.sh` — automated ingestion script (adds audit + mode)
- `CLEANUP_CHECKLIST.md` — artifact removal details (extended with bloat-specific paths)
- `INGEST_LOG.md` — post-ingest metrics, one block per module
- Issue #6060 — original consolidation discussion
- PR #6061, #6085, #6086 — prior rejected approaches
- PR #6093 — v1 ingest demo (feedback that motivated v2)
