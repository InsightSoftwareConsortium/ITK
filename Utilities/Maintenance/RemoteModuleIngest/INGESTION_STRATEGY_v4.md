# Remote Module Ingestion Strategy — v4 (two-phase split)

This is the next iteration of the ingestion process. v3 (see
`INGESTION_STRATEGY.md`) folded the upstream-archival step into the
ingestion driver, which made the ingest script harder to re-run and
coupled an irreversible upstream operation (publishing a deletion
commit + flipping `archived=true`) to a reversible local one (rewriting
history into ITK).

**v4 splits these into two independent phases**, with separate scripts
and separate operator gates between them.

## TL;DR

| Phase | Script | Local-clone fate | Upstream side-effects |
|---|---|---|---|
| **A — Ingestion** | `ingest-module-v4.sh` | **Throwaway** — destroy after PR merges | None (no pushes to upstream remote) |
| **B — Upstream archival** | `archive-remote-module.sh` | **Fresh clone** — built from scratch | Pushes one removal commit + promotes `MIGRATION_README.md` to `README.md` + flips `archived=true` |

The phases share **only one persistent artifact** between them: the
**whitelist** that defines what files migrated to ITK vs. what files
remain in the upstream archive.  Every other piece of state is
reproducible.

## Why two phases

| v3 problem | v4 solution |
|---|---|
| Ingest script orchestrated upstream archival as a bonus step → re-running the ingest after a CI failure risked re-touching upstream | Phase A never touches the upstream remote at all; failures are 100% local |
| Per-commit pre-commit + ghostflow validity was best-effort; some historical commits failed style hooks during reviewer testing | Phase A applies clang-format + black + commit-prefix sanitization to **every** commit, so every commit independently passes ITK's gates |
| Upstream archival was a manual checklist (per `feedback_ingest_archive_readme.md`) → easy to forget steps, ordering errors | Phase B is a single script with deterministic ordering and post-conditions verifiable via `gh api .../readme` |
| `git blame` on imported history showed mixed formatting from various upstream eras → noise on subsequent maintenance work | Phase A formats every blob across history with the destination ITK's `.clang-format` so blame is uniform |

## Phase A — Ingestion (local, throwaway)

### Operator command

```bash
ingest-module-v4.sh <Module> <Group> [--dry-run] [--keep-tempdir]
```

Examples:

```bash
ingest-module-v4.sh IOMeshSTL  IO        # Modules/IO/MeshSTL/
ingest-module-v4.sh Cuberille  Filtering # Modules/Filtering/Cuberille/
```

### What Phase A does

```
┌─────────────────────────────────────────────────────────────────┐
│ 1. Pre-flight                                                   │
│    - git-filter-repo, clang-format, black available             │
│    - clean ITK working tree on a feature branch                 │
│    - whitelist file readable                                    │
│    - destination directory does not collide                     │
├─────────────────────────────────────────────────────────────────┤
│ 2. Mirror-clone upstream into ${TMPDIR}/<Module>-mirror         │
│    - throwaway location; deleted on success unless --keep-      │
├─────────────────────────────────────────────────────────────────┤
│ 3. filter-repo: paths-from-file <whitelist>, --tag-rename ''    │
│    - keep only whitelisted files across full history            │
│    - non-whitelisted files vanish from every commit             │
├─────────────────────────────────────────────────────────────────┤
│ 4. filter-repo: subdirectory move into Modules/<Group>/<Module> │
├─────────────────────────────────────────────────────────────────┤
│ 5. sanitize-history.py:                                         │
│    - For every commit:                                          │
│      a. Walk file_changes, rewrite each .cxx/.h/.hxx/... blob   │
│         through `clang-format -style=file` (ITK's .clang-format)│
│      b. Walk file_changes, rewrite each .py blob through `black`│
│      c. Examine commit subject; if no BUG:/COMP:/DOC:/ENH:/     │
│         PERF:/STYLE: prefix → auto-prepend a heuristic prefix   │
│      d. Log every prefix decision to                            │
│         ${LOG_DIR}/commit-prefix-log.txt                        │
├─────────────────────────────────────────────────────────────────┤
│ 6. Topology verification                                        │
│    - require ≥1 merge in upstream/main..HEAD if upstream had any│
│    - reject linearization (per feedback_ingest_merge_topology)  │
├─────────────────────────────────────────────────────────────────┤
│ 7. Mode-A merge into ITK feature branch                         │
│    - --no-ff --allow-unrelated-histories                        │
│    - merge commit subject: "ENH: Ingest <Module> remote module" │
├─────────────────────────────────────────────────────────────────┤
│ 8. Verification                                                 │
│    - pre-commit run --all-files exit 0                          │
│    - every commit on the ingest tail has a valid prefix         │
│    - git blame on a sampled file walks back into upstream       │
└─────────────────────────────────────────────────────────────────┘
```

After Phase A succeeds, the operator opens a draft PR (per
`pr-no-unsolicited` + `pr-always-draft`).  The temp dir is destroyed
unless `--keep-tempdir` was passed.  **Phase A makes no commits or
pushes to the upstream remote — full stop.**

## Phase B — Upstream archival (clean clone, irreversible publish)

Run **only after the Phase A PR has merged into ITK `main`.**  This is
the gate: until ITK has accepted the migration, the upstream archive is
not authorized.

### Operator command

```bash
archive-remote-module.sh <Module> [--no-archive-flip] [--dry-run]
```

### What Phase B does

```
┌─────────────────────────────────────────────────────────────────┐
│ 1. Pre-flight                                                   │
│    - The Phase A ingest PR has merged on ITK main               │
│    - git, gh CLI authenticated, network reachable               │
│    - whitelist file (same one used in Phase A) accessible       │
├─────────────────────────────────────────────────────────────────┤
│ 2. Clean shallow clone of upstream                              │
│    git clone --depth 50 git@github.com:<org>/<Repo>.git ${TMP}  │
├─────────────────────────────────────────────────────────────────┤
│ 3. Build the removal commit on a fresh branch                   │
│    - git rm -rf each whitelisted path                           │
│    - leave non-whitelisted (papers, Old/, examples/, etc.)      │
│    - one commit subject: "MIGRATE: Remove files migrated to ITK │
│      <ITK-PR>"                                                  │
├─────────────────────────────────────────────────────────────────┤
│ 4. Promote MIGRATION_README.md to README.md                     │
│    - per feedback_ingest_archive_readme.md                      │
│    - rename old README.rst → info.rst (history preserved)       │
│    - rename MIGRATION_README.md → README.md                     │
│    - one additional commit:                                     │
│      "DOC: Promote migration notice to README.md"               │
├─────────────────────────────────────────────────────────────────┤
│ 5. Push to upstream main                                        │
│    git push origin HEAD:main                                    │
├─────────────────────────────────────────────────────────────────┤
│ 6. Verify GitHub-rendered README is the migration notice        │
│    gh api repos/<org>/<Repo>/readme --jq .content | base64 -d   │
│    | head -10 must contain "Migrated to ITK main" or equivalent │
├─────────────────────────────────────────────────────────────────┤
│ 7. Flip archived=true (skip if --no-archive-flip)               │
│    gh api -X PATCH repos/<org>/<Repo> -F archived=true          │
├─────────────────────────────────────────────────────────────────┤
│ 8. Cleanup                                                      │
│    - delete the throwaway clone                                 │
└─────────────────────────────────────────────────────────────────┘
```

### Why Phase B uses a fresh clone

The Phase A throwaway clone has been mutated by `filter-repo` and is
**not safe to push back** to the upstream remote — its commit graph and
object pool are no longer 1:1 with the upstream.  A fresh clone is the
only way to safely publish a deletion commit on top of upstream's actual
HEAD without inadvertently force-pushing rewritten history to the
upstream archive (which would destroy the original development history
that the archive is supposed to preserve).

## Shared artifact: the whitelist

Both phases consume the same whitelist.  It lives at:

```
Utilities/Maintenance/RemoteModuleIngest/whitelists/<Module>.list
```

Format: one path/glob per line, relative to the upstream repo root,
shell-style globs.  Lines starting with `#` are comments.  Example for
`IOMeshSTL`:

```
# Public headers
include/**

# Test sources + fixtures
test/**

# CMake / module metadata
CMakeLists.txt
itk-module.cmake

# Wrapping
wrapping/**

# License (always)
LICENSE
```

Phase A uses this with `--paths-from-file`; Phase B inverts it (delete
everything on the whitelist; keep everything else, plus
`MIGRATION_README.md` if present).

## Implementation choices (from 2026-05-04 design discussion)

| Question | Decision |
|---|---|
| Where does the upstream "removal" commit live? | **Phase B publishes it to upstream `main` directly** (replaces the manual archival PR pattern from `feedback_ingest_archive_readme.md`) |
| Auto-prefix vs. report-and-fail for commit subjects? | **Heuristic auto-prefix with sidecar log** — fast, operator can spot-check the log before merge |
| Format-history scope? | **All commits** — uniform `git blame`, one-time wall-time cost is acceptable |
| Python formatter? | **`black`** on every `.py` blob across all history (in addition to clang-format on C++) |
| Where do v3 scripts go? | Kept in tree, renamed `*_v3.{sh,py}` until v4 has been used for two successful ingests, then retired to `Utilities/Maintenance/RemoteModuleIngest/retired/v3/` |

## Operator gate between Phase A and Phase B

```
Phase A: ingest-module-v4.sh IOMeshSTL IO
        → draft PR opened
        → reviewer feedback, fixups, force-pushes (Phase A re-runs OK)
        → PR marked ready for review
        → MAINTAINER MERGES THE PR
        ────────────────────────────────────────
        → Phase B: archive-remote-module.sh IOMeshSTL
        → upstream main now shows MIGRATION_README.md as repo README
        → upstream archived=true
```

This gate is the central correctness property of v4: **upstream is
never touched until ITK has accepted the migration.**

## Heuristic prefix mapping

When `sanitize-history.py` encounters a commit subject without a valid
prefix, it auto-prepends one based on the table below.  Every decision
is logged with `<sha> <chosen-prefix> <original-subject>` so the
operator can audit before merge.

| Subject contains... | Prefix |
|---|---|
| `fix`, `bug`, `crash`, `segfault`, `leak`, `regression` | `BUG:` |
| `cmake`, `compil`, `warning`, `build`, ` ci ` / `ci:`, `clang-format` config | `COMP:` |
| `doc`, `readme`, `comment`, `license`, `header text` | `DOC:` |
| `style`, `whitespace`, `format`, `rename`, `lint`, `clean up` | `STYLE:` |
| `perf`, `optimi`, `speed`, `faster`, `efficien` | `PERF:` |
| anything else | `ENH:` (default) |

These rules are intentionally **conservative** — when in doubt, default
to `ENH:`.  Misclassifications get caught by the operator reading the
sidecar log; the cost of `ENH: STYLE-cleanup-of-headers` slipping past
review is much lower than the cost of `STYLE: Add new IO format
support` mis-prefixed.

## Known artifacts at PR-review time

### Unavoidable: ghostflow root-commit failure

The Mode-A `--allow-unrelated-histories` merge brings the upstream
module's whole lineage into the PR diff, including its first commit
(zero parents).  ITK's `ghostflow-check-main` rejects every PR that
introduces a root commit:

```
commit <sha> not allowed; it is a root commit.
```

This single error is **expected and accepted** for every v4 ingest —
maintainers override at merge time.  Removing the root would force
linearization (squash or rebase-without-merges) and forfeit the
merge-topology requirement (per `feedback_ingest_merge_topology.md`),
which the project considers a worse trade.

If `ghostflow-check-main` reports any error other than the single
root-commit line, that error **is** a real problem for the operator to
fix before maintainer review.  Keep an eye on the message body.

### Patterns Phase A now sanitizes automatically

The following recurring upstream artifacts are stripped or rewritten
by the v4 sanitizer; operators do not need to fix them by hand:

| Artifact | Where handled | Why |
|---|---|---|
| `**/.ExternalData_*` (e.g. `.ExternalData_MD5_<hash>` baseline cache) | `ingest-module-v4.sh` deny-pass | Local fetch-cache from upstream's CTest run; ExternalData regenerates from `.cid` / `.md5` sidecars on demand. (@dzenanz, PR #6206) |
| `if(NOT ITK_SOURCE_DIR) ... find_package(ITK) ... else() itk_module_impl() endif()` standalone-build guard in module `CMakeLists.txt` | `sanitize-history.py:patch_standalone_build_guard` | In-tree, `ITK_SOURCE_DIR` is always defined; the if-branch is dead code. (@dzenanz, PR #6206) |
| `cmake_minimum_required(VERSION X.Y.Z)` line at the top of an ingested module's `CMakeLists.txt` | `sanitize-history.py:patch_drop_cmake_minimum_required` | ITK's top-level CMakeLists pins a higher minimum; per-module declarations are redundant and frequently **lower** than the ITK floor (3.10.2 is common upstream). (@dzenanz, PR #6215 IOFDF) |
| `README.rst` references in CMake `file(READ ...)` calls | `sanitize-history.py:patch_readme_reference` | Phase B archival promotes `MIGRATION_README.md` to `README.md`; in-tree consumers read the markdown form |
| `get_filename_component(...) / file(READ README.md DOCUMENTATION)` preamble + `DESCRIPTION "${DOCUMENTATION}"` in `itk-module.cmake` | `sanitize-history.py:patch_dynamic_description` | Archival `README.md` contains semicolons and `[` characters that CMake list expansion splits into spurious `itk_module()` arguments, producing `CMake Warning (dev): Unknown argument` on every configure (observed: RLEImage, SplitComponents, IOFDF, IOMeshMZ3, IOMeshSTL; fixed in ITK PR #6220) |
| `*.orig`, `*.rej`, `*.BACKUP.*`, `*.LOCAL.*`, `*.REMOTE.*`, `*.BASE.*` | deny-pass | Leftover merge-conflict artifacts |
| Scaffolding (`Dockerfile*`, `azure-pipelines*.yml`, `.github/`, `.travis.yml`, `.circleci/`, `tox.ini`, `pyproject.toml`, `setup.py`, `.clang-format`, `.pre-commit-config.yaml`, …) | deny-pass | Module's per-repo CI/packaging is irrelevant in-tree |

Each sanitizer prints a `<count> patches` line in the run summary so
the operator can confirm the rule fired (or didn't) on a given module.

### Code-level patterns commonly flagged by Greptile post-ingest

The patterns below still require **human judgment** to fix — they're
semantic, not mechanical, and the v4 sanitizer leaves them alone.
The list is intentionally short: only patterns observed on multiple
v3/v4 ingests are listed.  Treat it as the operator's pre-ready-for-
review checklist.

| Pattern | Where seen | Resolution |
|---|---|---|
| Signed `int32_t` / `int` used for spec-defined unsigned counts (e.g. file-format triangle counts, voxel counts) | IOMeshSTL #6206 | Switch to `uint32_t` / `SizeValueType`; retype `ByteSwapper<>`; verify decrement loops |
| Missing `override` specifier on virtual base-class method overrides | (universal) | Add `override`; if base method is non-virtual, document why the derived method shadows |
| Dead `return;` after `itkExceptionMacro(...)` | IOMeshSTL #6206 | `itkExceptionMacro` throws; the trailing `return;` is unreachable — delete |
| `inputFilename=` in error path that opens the **output** stream (and vice-versa) | IOMeshSTL #6206 | Cosmetic but misleading; correct the label |
| Test pipeline `Update()` not wrapped in `ITK_TRY_EXPECT_NO_EXCEPTION` | IOMeshSTL #6206 | Wrap so failure surfaces with a descriptive message instead of an unhandled-exception crash |
| `GetBuffer() const` returning a non-`const` smart pointer (allows mutation through a `const` receiver) | RLEImage #6208 | Return `BufferType::ConstPointer`; consider whether `m_Buffer`'s `mutable` is necessary |
| Global-namespace forward declaration + `friend class ::Foo;` inside an ITK header, where `Foo` is not part of ITK | RLEImage #6208 | Strip both the forward decl and the `friend` grant; they are remote-module-internal coupling |
| Stray positional argument after the function name in `itk_add_test(... COMMAND ...Driver fnName <stray>)` | RLEImage #6208 | Test driver passes `argv[2..n]` to `fnName` as arguments — it does not invoke a second function. Remove the stray, register the second test separately if needed |
| Allocator overrides whose `bool initialize` parameter is silently ignored | RLEImage #6208 | Verify the override has an internal invariant requiring unconditional init **before** "fixing" — for some module types (e.g. RLEImage's RLE-line buffer) the unconditional fill IS the contract; document it instead |

When an upcoming ingest hits a new pattern that recurs across modules,
extend this table; it is the durable channel for v4 review-knowledge.

## What this strategy explicitly does NOT change from v3

- The Mode-A (`--no-ff --allow-unrelated-histories`) merge into ITK
- Merge-topology preservation requirement (per
  `feedback_ingest_merge_topology.md`)
- The Mode-A subject convention for the merge commit (`ENH: Ingest …`)
- The whitelist as the source of truth for what migrates
- The directory layout (`Modules/<Group>/<Module>/`)

## Provenance

- 2026-05-04 — Initial v4 design (Hans Johnson + Claude Code session) in
  response to coupled-archival pain in v3 ingests (LabelErodeDilate,
  GLI, MGHIO, FastBilateral, MeshNoise, Montage)
- Smoke-tested on `IOMeshSTL` before being applied to any other module
