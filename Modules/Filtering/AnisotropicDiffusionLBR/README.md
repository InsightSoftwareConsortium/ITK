# AnisotropicDiffusionLBR

In-tree ITK module providing coherence-enhancing (CED) and
edge-enhancing (EED) anisotropic diffusion filters built on the
Lattice-Basis-Reduction (LBR) stencil scheme by Jean-Marie Mirebeau.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/ITKAnisotropicDiffusionLBR**](https://github.com/InsightSoftwareConsortium/ITKAnisotropicDiffusionLBR)
on 2026-04-22, at upstream commit
[`203260b9`](https://github.com/InsightSoftwareConsortium/ITKAnisotropicDiffusionLBR/commit/203260b929acda68a1f64b1267b0d89e825904ec).
The upstream repository will be archived read-only after this PR
merges; it remains reachable at the URL above.

## What lives here

Per the v3 ingestion strategy (see
`Utilities/Maintenance/RemoteModuleIngest/INGESTION_STRATEGY.md`), only
paths matching the narrow whitelist (code, headers, tests, wrapping,
module CMake) crossed the merge boundary:

- `include/` — public C++ headers.
- `test/` — Google-test / CTest drivers and content-link stubs.
- `wrapping/` — Python wrapping descriptors.
- `CMakeLists.txt`, `itk-module.cmake` — build + module descriptors.

Every surviving commit preserves original authorship; `git blame`
walks across the merge boundary to upstream authors from 2014.

## What was intentionally left upstream

Everything outside the whitelist stays in the archived upstream repo.
If you need any of it, clone
<https://github.com/InsightSoftwareConsortium/ITKAnisotropicDiffusionLBR>.

| Content in upstream | Why it did not ingest |
|---|---|
| `Old/` | Pre-refactor legacy trees; no current test depends on them. |
| `examples/` + `examples/Data/` | Per-module `examples/` are routed to top-level [`Examples/`](https://github.com/InsightSoftwareConsortium/ITK/tree/main/Examples) via a separate follow-up PR. |
| `README.rst` | Algorithm citations + background are folded into the Doxygen on the filter headers. |
| `test/azure-pipelines.yml`, `test/Docker/` | Standalone-build CI scaffolding; not useful in-tree. |
| `CTestConfig.cmake`, `pyproject.toml`, `.github/`, `.clang-format`, `LICENSE` | Packaging / CI / style scaffolding superseded by ITK root. |

## Long-form documentation

- **Algorithm description** — see the Doxygen on
  `itkAnisotropicDiffusionLBRImageFilter` and
  `itkCoherenceEnhancingDiffusionImageFilter`, plus embedded citations
  (Mirebeau et al.).
- **Standalone build + usage examples** — see the archived upstream at
  <https://github.com/InsightSoftwareConsortium/ITKAnisotropicDiffusionLBR>.

## Content-link status

The 24 baseline and input content-links under `test/Baseline/` and
`test/Input/` are currently `.md5` stubs.  Per the v3 strategy they
should be normalized to `.cid` (IPFS Content Identifier) before this
PR merges; see
`Utilities/Maintenance/RemoteModuleIngest/cid-normalize.sh`.
