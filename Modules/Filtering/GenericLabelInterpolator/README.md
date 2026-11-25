# GenericLabelInterpolator

In-tree ITK module providing a generic interpolator for multi-label
images.  `itk::LabelImageGenericInterpolateImageFunction` interpolates
each label independently with any ordinary image interpolator and
returns, at each query point, the label whose interpolated value is
highest.  Generalizes the Gaussian-only behavior of
`itk::LabelImageGaussianInterpolateImageFunction`.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/ITKGenericLabelInterpolator**](https://github.com/InsightSoftwareConsortium/ITKGenericLabelInterpolator)
on 2026-04-25, at upstream commit
[`081575cd`](https://github.com/InsightSoftwareConsortium/ITKGenericLabelInterpolator/commit/081575cdcc26b1a542d4c90feaf9250df5e104d9).
The upstream repository will be archived read-only after this PR
merges; it remains reachable at the URL above.

## What lives here

Per the v3 ingestion strategy (see
`Utilities/Maintenance/RemoteModuleIngest/INGESTION_STRATEGY.md`), only
paths matching the narrow whitelist (code, headers, tests, wrapping,
module CMake) crossed the merge boundary:

- `include/` — public C++ headers.
- `test/` — CTest drivers and content-link stubs.
- `wrapping/` — Python wrapping descriptors.
- `CMakeLists.txt`, `itk-module.cmake` — build + module descriptors.

Every surviving commit preserves original authorship; `git blame`
walks across the merge boundary to upstream authors back to 2014.

## What was intentionally left upstream

Everything outside the whitelist stays in the archived upstream repo.
If you need any of it, clone
<https://github.com/InsightSoftwareConsortium/ITKGenericLabelInterpolator>.

| Content in upstream | Why it did not ingest |
|---|---|
| `examples/` | Per-module `examples/` are routed to top-level [`Examples/`](https://github.com/InsightSoftwareConsortium/ITK/tree/main/Examples) via a separate follow-up PR. |
| `README.rst` | Algorithm citations + background are folded into the Doxygen on the filter headers and the module DESCRIPTION. |
| `.github/`, `azure-pipelines.yml`, `Dockerfile` | Standalone-build CI scaffolding; not useful in-tree. |
| `CTestConfig.cmake`, `pyproject.toml`, `LICENSE`, `.clang-format` | Packaging / CI / style scaffolding superseded by ITK root. |

## Long-form documentation

- **Algorithm description** — see the Doxygen on
  `itkLabelImageGenericInterpolateImageFunction`, plus the embedded
  citation:

  > Schaerer, J., Roche, F., Belaroussi, B.
  > *A generic interpolator for multi-label images*.
  > The Insight Journal, January–December 2014.
  > <https://hdl.handle.net/10380/3506>

- **Standalone build + usage examples** — see the archived upstream at
  <https://github.com/InsightSoftwareConsortium/ITKGenericLabelInterpolator>.

## Content-link status

The 2 baseline / input content-links under `test/Baseline/` and
`test/Input/` are already in `.cid` (IPFS Content Identifier) form;
no `.md5` → `.cid` normalization is required for this module.
