# IOFDF

In-tree ITK module providing read support for the **FDF** (Flexible
Data Format) image files produced by Varian / Agilent MR scanners.
Implements `itk::FDFImageIO` and registers `itk::FDFImageIOFactory`
so FDF files are auto-detected by `itk::ImageFileReader`.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/ITKIOFDF**](https://github.com/InsightSoftwareConsortium/ITKIOFDF)
on 2026-05-05, at upstream commit
[`3a3ff84c`](https://github.com/InsightSoftwareConsortium/ITKIOFDF/commit/3a3ff84c803d4226176078d32ddd0760c71a5b2c).
The upstream repository will be archived read-only after this PR
merges; it remains reachable at the URL above.

## What lives here

Per the v4 ingestion strategy (see
`Utilities/Maintenance/RemoteModuleIngest/INGESTION_STRATEGY_v4.md`),
only paths matching the narrow whitelist
(`Utilities/Maintenance/RemoteModuleIngest/whitelists/IOFDF.list`)
crossed the merge boundary:

- `include/` — public C++ headers (`itkFDFImageIO.h`,
  `itkFDFImageIOFactory.h`, `itkFDFCommonImageIO.h`).
- `src/` — implementation sources.
- `test/` — CTest drivers and content-link stubs.
- `wrapping/` — Python wrapping descriptors.
- `CMakeLists.txt`, `itk-module.cmake` — module metadata.
- `LICENSE` — original Apache-2.0 license text.

Upstream-only artifacts (CI dashboards, packaging,
`pyproject.toml`, `.github/`, etc.) were explicitly excluded; the
upstream commit graph and authorship history are otherwise preserved
through a `--no-ff --allow-unrelated-histories` Mode-A merge.

## Building

IOFDF is enabled by default once `Module_IOFDF=ON` is set; in CI it
is enabled via `pyproject.toml`'s `configure-ci` task.
