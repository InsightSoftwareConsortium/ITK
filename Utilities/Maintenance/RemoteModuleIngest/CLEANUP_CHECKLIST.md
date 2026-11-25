# Post-Graft Cleanup Checklist

**Note: this checklist is largely superseded by the whitelist in
`INGESTION_STRATEGY.md`.**  As of v3, the graft uses a `filter-repo
--paths` whitelist (not a blacklist), so files not on the whitelist
never enter ITK's history in the first place.  The checklist below is
retained as:

1. A **safety net** for edge cases where an excluded item somehow
   makes it past the whitelist (e.g., a whitelisted `test/` subtree
   happens to contain an upstream-only `README.md`).
2. A **reference** explaining why each category is excluded, so a
   future reviewer proposing to widen the whitelist can weigh the
   tradeoffs.
3. The **bloat-specific history-wide removal section** (below) still
   applies when a module lands in Mode B and needs extra
   `--strip-blobs-bigger-than` beyond the whitelist.
4. The **copyright-review section** (below) still applies: even
   items inside whitelisted paths (e.g., a PDF accidentally dropped
   into `test/`) must be flagged to a human rather than silently
   stripped.

## What an ingested module SHOULD contain

A normal ITK in-tree module looks like:

```
Modules/<Group>/<Name>/
├── CMakeLists.txt
├── itk-module.cmake
├── include/
├── src/            (if any)
├── test/
├── wrapping/       (if wrapping exists upstream)
└── examples/       (optional; some ITK modules ship these)
```

Where `<Group>` is the appropriate ITK group (Filtering, IO,
Segmentation, Registration, Numerics, Core, etc.).

That's it. Anything else upstream shipped is a cleanup candidate.

## Remove if present (always)

These are standalone-repo scaffolding with no role in the monorepo:

- [ ] `build/` — local build tree accidentally committed
- [ ] `ITK-source/` — nested ITK checkout used for standalone CI
- [ ] `.github/` — upstream's own GitHub Actions (ITK has its own CI)
- [ ] `.gitlab-ci.yml`, `.circleci/`, `.travis.yml`, `appveyor.yml`,
      `azure-pipelines.yml` — foreign CI configs
- [ ] `pyproject.toml` — standalone-wheel packaging (ITK's wrapping
      infra handles Python)
- [ ] `requirements-dev.txt`, `requirements.txt` — standalone Python deps
- [ ] `setup.py`, `setup.cfg`, `MANIFEST.in` — standalone packaging
- [ ] `CTestConfig.cmake` — points at a standalone CDash project;
      ITK's top-level CTestConfig applies instead
- [ ] `.readthedocs.yml`, `readthedocs.yaml`, `docs/conf.py` (only if
      it's standalone Sphinx, not module-specific documentation)
- [ ] `Dockerfile`, `docker/`, `.dockerignore`
- [ ] `.clang-format`, `.clang-tidy` — ITK root versions govern
- [ ] `.pre-commit-config.yaml`
- [ ] `codecov.yml`, `.codecov.yml`
- [ ] `tox.ini`
- [ ] `environment.yml`, `environment-dev.yml`

## Bloat-specific removal candidates (NEW — history-wide, via filter-repo)

These aren't just working-tree files — the audit step in
`INGESTION_STRATEGY.md` checks for them **across the full upstream
history** and feeds them to `git filter-repo --invert-paths --paths`
so they don't enter the ITK pack at all. Unlike the items above (which
are removed in a post-merge `STYLE:` commit), these must be stripped
*before* the merge, otherwise the blobs remain reachable in the
merged history and bloat the pack permanently.

Cited in blowekamp's PR #6093 feedback as specific concerns:

- [ ] `Old/` — legacy-implementation trees that upstream kept around
      for reference but which are not part of the current module.
      Example: `ITKAnisotropicDiffusionLBR/Old/` is unambiguously a
      pre-refactor archive.
- [ ] `paper/`, `papers/`, `publication/` — source material for
      associated publications (LaTeX, figures, supplementary PDFs).
      These contribute most of the pack-size bloat in historically
      academic modules.
- [ ] `docs/figures/`, `doc/figures/`, `docs/images/` when contents
      are PNG/JPG/TIFF > 100 KiB AND not referenced from Doxygen.
      Small diagrams (< 50 KiB) referenced from Doxygen source are
      fine to keep — they're part of the documented API.
- [ ] `docs/anim/`, `demo/`, `demo-video/`, `screencast/` —
      demo videos and animations.
- [ ] `presentations/`, `talks/`, `slides/` — slide decks.
- [ ] `media/`, `movie/`, `animations/`.
- [ ] `example/` or `examples/` subtree when it contains image assets
      > 256 KiB that aren't test fixtures. Small `.cxx` usage examples
      are fine; full-resolution sample images are not. If the module
      genuinely needs a sample image for a usage example, it should
      go through ITK's ExternalData/`.md5` mechanism just like tests.

## Copyright-review candidates (must be human-checked)

Flagged by the audit but never stripped automatically — decision is
case-by-case because the content may be licensed compatibly or may
need pre-ingest permission from the author:

- [ ] Any PDF (papers, theses, supplementary material)
- [ ] Any video/audio file (.mp4, .mov, .avi, .ogg, .webm)
- [ ] Any image file > 1 MiB (possible copyrighted figure)
- [ ] Any `COPYING_*` / `AUTHORS_*` / `CREDITS` file whose text
      references a non-Apache license or a third-party institution
- [ ] Any `README` section that mentions academic publication
      figure reuse

Audit output lists each of these; the ingestor must decide
**strip** / **keep** / **defer-to-reviewer** before proceeding.

## Remove if present (usually)

- [ ] `README.rst` / `README.md` — upstream readme; replace with a
      short `README.md` pointing at the beta manifest OR fold the
      useful parts into module-level Doxygen. Keep if it documents
      usage; drop if it's just standalone-repo badges/install
      instructions.
- [ ] `LICENSE` — if Apache-2.0 matching ITK's, redundant; if a
      different compatible license (MIT, BSD), KEEP and note in
      the `.beta.cmake` manifest.
- [ ] `CHANGELOG.md` / `HISTORY.rst` — not ITK convention; git log
      is the record. Consider folding highlights into the `.beta.cmake`
      manifest as `NOTES` before deleting.

## Keep

- [ ] `include/`, `src/`, `test/`, `wrapping/`, `examples/`
- [ ] `CMakeLists.txt`, `itk-module.cmake`
- [ ] Any `*.md` inside `include/` or `test/` that is Doxygen source
- [ ] Test baseline data (`test/Baseline/`, `test/Input/`, `.md5` files)

## Verify after cleanup

```bash
# Files left should look like a native ITK module:
find Modules/<Group>/<Name> -maxdepth 2 -type f | sort

# No foreign CI artifacts remain:
find Modules/<Group>/<Name> -name '.github' -o -name 'pyproject.toml' \
     -o -name 'build' -o -name 'ITK-source'
# Expect: no output.

# Blame on surviving files still walks to upstream authors:
git blame Modules/<Group>/<Name>/include/*.h | head
```

## Commit shape

```
STYLE: Remove non-ITK artifacts from ingested <Name>

Removes standalone-repo scaffolding with no role in the ITK monorepo:
  - build/ and ITK-source/ (local build trees from upstream dev)
  - .github/ (foreign CI; ITK's workflows cover this now)
  - pyproject.toml, requirements-dev.txt (standalone Python packaging)
  - CTestConfig.cmake (pointed at a standalone CDash project)
  - .clang-format, .clang-tidy, .pre-commit-config.yaml (ITK root versions govern)

No edits to ingested source files; structural cleanup only.
```
