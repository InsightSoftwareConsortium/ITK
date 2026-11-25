# PolarTransform

In-tree ITK module providing forward and inverse Cartesian↔polar
coordinate transforms (`itk::CartesianToPolarTransform` and
`itk::PolarToCartesianTransform`) for use in registration, resampling,
and geometric reasoning over polar / cylindrical images.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/ITKPolarTransform**](https://github.com/InsightSoftwareConsortium/ITKPolarTransform)
on 2026-05-05, at upstream commit
[`65b3c950`](https://github.com/InsightSoftwareConsortium/ITKPolarTransform/commit/65b3c9509a8c8570a262e02ca283e412f77bafaa).
The upstream repository will be archived read-only after this PR
merges; it remains reachable at the URL above.

## What lives here

Per the v4 ingestion strategy (see
`Utilities/Maintenance/RemoteModuleIngest/INGESTION_STRATEGY_v4.md`),
only paths matching the narrow whitelist
(`Utilities/Maintenance/RemoteModuleIngest/whitelists/PolarTransform.list`)
crossed the merge boundary:

- `include/` — public C++ headers (header-only module; no `src/`).
- `test/` — CTest drivers and content-link stubs.
- `wrapping/` — Python wrapping descriptors.
- `CMakeLists.txt`, `itk-module.cmake` — module metadata.
- `LICENSE` — original Apache-2.0 license text.

Upstream-only artifacts (CI dashboards, packaging,
`.github/`, Docker scaffolding, `pyproject.toml`, etc.) were
explicitly excluded; the upstream commit graph and authorship history
are otherwise preserved through a `--no-ff --allow-unrelated-histories`
Mode-A merge.

## Building

PolarTransform is enabled by default once `Module_PolarTransform=ON`
is set; in CI it is enabled via `pixi.toml`'s `configure-ci` task.
