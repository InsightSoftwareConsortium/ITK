# MorphologicalContourInterpolation

In-tree ITK module providing filters that interpolate manually
segmented anatomical contours through volumetric label data, using
morphological reconstruction techniques.  The result is a 3D label
mask reconstructed from a sparse set of 2D contours drawn on
arbitrary slices.

The flagship class is `itk::MorphologicalContourInterpolator`.

## Origin

Ingested from the standalone remote module
[**KitwareMedical/ITKMorphologicalContourInterpolation**](https://github.com/KitwareMedical/ITKMorphologicalContourInterpolation)
on 2026-05-04 via the v4 ingestion pipeline. The upstream repository
will be archived read-only after this PR merges; it remains
reachable at the URL above for historical reference (including the
`examples/`, `wasm/`, and `doc/` directories which were intentionally
left in the upstream archive).

## Test dependency on RLEImage

This module's tests `TEST_DEPENDS` on `RLEImage` (also in-tree at
`Modules/Filtering/RLEImage/`).  Setting both
`Module_MorphologicalContourInterpolation:BOOL=ON` and
`Module_RLEImage:BOOL=ON` enables the full test set; with only the
former the module library still builds and is usable, but the
RLEImage-flavored tests are not configured.

## References

- Zukić D., Vlašić-Cicvarić I., Beichel R.R., Sonka M., Hodgdon T.,
  Aylward S. *Interpolation of label maps from morphological contour
  data.* The Insight Journal. 2016. <https://doi.org/10.54294/ux2obj>
