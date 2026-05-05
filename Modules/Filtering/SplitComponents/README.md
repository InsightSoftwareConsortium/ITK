# SplitComponents

In-tree ITK module providing `itk::SplitComponentsImageFilter` — a
filter that splits a vector / covariant-vector / symmetric-second-rank-
tensor image into one scalar component image per pixel-component.
Useful for visualising, post-processing, or independently filtering
the channels of multi-component images.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/ITKSplitComponents**](https://github.com/InsightSoftwareConsortium/ITKSplitComponents)
on 2026-05-05, at upstream commit
[`94fde65c`](https://github.com/InsightSoftwareConsortium/ITKSplitComponents/commit/94fde65cae6883c9a0f1508c37c501c2ad989cdd).
The upstream repository will be archived read-only after this PR
merges; it remains reachable at the URL above.

## What lives here

Per the v4 ingestion strategy (see
`Utilities/Maintenance/RemoteModuleIngest/INGESTION_STRATEGY_v4.md`),
only paths matching the narrow whitelist
(`Utilities/Maintenance/RemoteModuleIngest/whitelists/SplitComponents.list`)
crossed the merge boundary:

- `include/` — public C++ headers (header-only module; no `src/`).
- `test/` — CTest drivers and content-link stubs.
- `wrapping/` — Python wrapping descriptors.
- `CMakeLists.txt`, `itk-module.cmake` — module metadata.
- `LICENSE` — original Apache-2.0 license text.

Upstream-only artifacts (CI dashboards, packaging, `apps/`,
`article/`, `binder/`, `examples/`, `images/`, `.github/`,
`pyproject.toml`, etc.) were explicitly excluded; the upstream
commit graph and authorship history are otherwise preserved through
a `--no-ff --allow-unrelated-histories` Mode-A merge.

## Building

SplitComponents is enabled by default once `Module_SplitComponents=ON`
is set; in CI it is enabled via `pyproject.toml`'s `configure-ci` task.
