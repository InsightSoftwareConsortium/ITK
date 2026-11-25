# IOMeshMZ3

In-tree ITK module providing read/write support for the **MZ3** mesh
file format used by [Surf Ice](https://www.nitrc.org/projects/surfice)
and other neuroimaging surface-visualisation tools.  Implements
`itk::MZ3MeshIO` and registers `itk::MZ3MeshIOFactory` so MZ3 files
are auto-detected by `itk::MeshFileReader` / `itk::MeshFileWriter`.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/ITKIOMeshMZ3**](https://github.com/InsightSoftwareConsortium/ITKIOMeshMZ3)
on 2026-05-05, at upstream commit
[`fbeed91e`](https://github.com/InsightSoftwareConsortium/ITKIOMeshMZ3/commit/fbeed91e6800830e84a7ba5e3461addd98a2e772).
The upstream repository will be archived read-only after this PR
merges; it remains reachable at the URL above.

## What lives here

Per the v4 ingestion strategy (see
`Utilities/Maintenance/RemoteModuleIngest/INGESTION_STRATEGY_v4.md`),
only paths matching the narrow whitelist
(`Utilities/Maintenance/RemoteModuleIngest/whitelists/IOMeshMZ3.list`)
crossed the merge boundary:

- `include/` — public C++ headers (`itkMZ3MeshIO.h`,
  `itkMZ3MeshIOFactory.h`).
- `src/` — implementation sources.
- `test/` — CTest drivers and content-link stubs.
- `wrapping/` — Python wrapping descriptors.
- `CMakeLists.txt`, `itk-module.cmake` — module metadata.
- `LICENSE` — original Apache-2.0 license text.

Upstream-only artifacts (CI dashboards, packaging, `examples/`,
`pyproject.toml`, `.github/`, etc.) were explicitly excluded; the
upstream commit graph and authorship history are otherwise preserved
through a `--no-ff --allow-unrelated-histories` Mode-A merge.

## Building

IOMeshMZ3 is enabled by default once `Module_IOMeshMZ3=ON` is set;
in CI it is enabled via `pyproject.toml`'s `configure-ci` task.
