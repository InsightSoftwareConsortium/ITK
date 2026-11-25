# MGHIO

In-tree ITK module providing an `itk::MGHImageIO` plugin for the
`.mgh` / `.mgz` (MGH) volume format used by the FreeSurfer
neuroimaging toolchain. The factory `itk::MGHImageIOFactory`
auto-registers with `itk::ImageIOFactory` so any
`itk::ImageFileReader` / `itk::ImageFileWriter` transparently picks
up `.mgh` / `.mgz` files when the module is enabled.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/itkMGHImageIO**](https://github.com/InsightSoftwareConsortium/itkMGHImageIO)
on 2026-04-27, at upstream commit
[`af74507c`](https://github.com/InsightSoftwareConsortium/itkMGHImageIO/commit/af74507c1ddc82722637c37d1f5d169e3000553a).
The upstream repository will be archived read-only after this PR
merges; it remains reachable at the URL above.

## What lives here

Per the v3 ingestion strategy (see
`Utilities/Maintenance/RemoteModuleIngest/INGESTION_STRATEGY.md`), only
paths matching the narrow whitelist (code, headers, tests, wrapping,
module CMake) crossed the merge boundary:

- `include/` — public C++ headers (`itkMGHImageIO.h`, `itkMGHImageIOFactory.h`).
- `src/` — non-template implementation.
- `test/` — CTest drivers and content-link stubs.
- `wrapping/` — Python wrapping descriptors.
- `CMakeLists.txt`, `itk-module.cmake` — build + module descriptors.

Every surviving commit preserves original authorship; `git blame`
walks across the merge boundary to upstream authors going back to the
module's earliest history.

## What was intentionally left upstream

Everything outside the whitelist stays in the archived upstream repo.
If you need any of it, clone
<https://github.com/InsightSoftwareConsortium/itkMGHImageIO>.

| Content in upstream | Why it did not ingest |
|---|---|
| `.github/`, `azure-pipelines.yml`, `Dockerfile` | Standalone-build CI scaffolding; not useful in-tree. |
| `CTestConfig.cmake`, `pyproject.toml`, `LICENSE`, `.clang-format` | Packaging / CI / style scaffolding superseded by ITK root. |
| Historical `Old/`, `paper/`, `docs/`, demo asset trees | Stripped by the whitelist to protect ITK's git pack from bloat. |

## Long-form documentation

- **Format reference** — see the FreeSurfer documentation:
  <https://surfer.nmr.mgh.harvard.edu/fswiki/FsTutorial/MghFormat>.
- **Standalone build + history** — see the archived upstream at
  <https://github.com/InsightSoftwareConsortium/itkMGHImageIO>.

## Compliance level

Compliance Level 2 (Alpha — niche execution environment dependence:
FreeSurfer-format I/O). Carried forward from the
`MODULE_COMPLIANCE_LEVEL 2` declaration in the previous
`Modules/Remote/MGHIO.remote.cmake`.

## Content-link status

The 3 input content-links under `test/MD5/` are already in `.cid`
(IPFS Content Identifier) form; no `.md5` → `.cid` normalization is
required for this module.
