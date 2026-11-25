# IOTransformDCMTK

In-tree ITK module providing read support for **DICOM spatial
transforms** via the DCMTK library. Implements
`itk::DCMTKTransformIO` and registers `itk::DCMTKTransformIOFactory`
so DICOM spatial-transform objects are auto-detected by
`itk::TransformFileReader`.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/ITKIOTransformDCMTK**](https://github.com/InsightSoftwareConsortium/ITKIOTransformDCMTK)
on 2026-05-20, at upstream commit
[`9d7f4a56`](https://github.com/InsightSoftwareConsortium/ITKIOTransformDCMTK/commit/9d7f4a56b1287edfc3a412c1dfad76f5c895180d).
The upstream repository will be archived read-only after this PR
merges; it remains reachable at the URL above.

## Dependencies

This module depends on DCMTK and is `EXCLUDE_FROM_DEFAULT`. Enable
it via:

```
-DModule_IOTransformDCMTK:BOOL=ON
```

`Module_ITKDCMTK` is pulled in transitively as a dependency and
does not need to be set explicitly.

Test dependencies additionally pull in `ITKIODCMTK`, `ITKIOGDCM`,
and `ITKImageGrid`.

## What lives here

Per the v4 ingestion strategy (see
`Utilities/Maintenance/RemoteModuleIngest/INGESTION_STRATEGY_v4.md`),
only paths matching the default whitelist crossed the merge
boundary:

- `include/` — public C++ headers.
- `src/` — implementation sources.
- `test/` — CTest drivers and content-link stubs (`.cid` form).
- `wrapping/` — Python wrapping descriptors.
- `CMakeLists.txt`, `itk-module.cmake` — module metadata.

Upstream-only artifacts (CI dashboards, packaging, `.github/`, etc.)
were explicitly excluded; the upstream commit graph and authorship
history are otherwise preserved through a
`--no-ff --allow-unrelated-histories` Mode-A merge.
