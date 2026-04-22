# Montage

Mosaic-stitching and 3D reconstruction of large datasets from a
collection of partially-overlapping 2D slices, built around phase
correlation image registration.  Core classes:

- `itk::PhaseCorrelationImageRegistrationMethod`
- `itk::PhaseCorrelationOperator`
- `itk::PhaseCorrelationOptimizer`
- `itk::TileMontage`
- `itk::TileMergeImageFilter`
- `itk::TileConfiguration`
- `itk::NMinimaMaximaImageCalculator`

## Upstream

This module was originally developed and maintained as a standalone
remote module at
<https://github.com/InsightSoftwareConsortium/ITKMontage>.  The
contents of `include/`, `src/`, `test/`, `wrapping/`, and the two
CMake build descriptors (`CMakeLists.txt`, `itk-module.cmake`) were
ingested into ITK via `Utilities/Maintenance/RemoteModuleIngest/`
(see PR #6098 for the tooling).

The standalone upstream repository should be treated as archived
once this ingest lands.  Refer to its tags and release history for
provenance, but future development happens in-tree here.

## What is intentionally NOT ingested

The whitelist-based ingest deliberately excludes the following
upstream content from ITK:

- `examples/` — applied demonstration code and sample datasets;
  lives with the archived upstream (and selectively relocated to
  [`Examples/`](https://github.com/InsightSoftwareConsortium/ITK/tree/main/Examples)
  in a follow-up PR if broadly useful).
- `.github/workflows/` — upstream-specific CI configuration; ITK's
  in-tree CI covers the module via the central pipeline.
- `CTestConfig.cmake` — pointed at a standalone CDash project that
  no longer applies in-tree.
- `README.md`, `LICENSE`, `requirements.txt`, `pyproject.toml` and
  similar scaffolding — top-level packaging files that are either
  inherited from ITK's own policy (Apache 2.0 license, etc.) or
  irrelevant in-tree.

If you need any of the above, consult the archived upstream.

## Compliance level

Previously tracked as a level-3 remote module (see the superseded
`Modules/Remote/Montage.remote.cmake`).  In-tree placement does not
automatically raise the compliance level; continued stewardship is
needed to reach levels 4 and 5.

## Contact

Originally authored and maintained by Dženan Zukić
&lt;dzenan.zukic@kitware.com&gt;.  In-tree maintenance follows ITK's
standard review process.
