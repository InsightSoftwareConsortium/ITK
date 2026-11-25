# FixedPointInverseDisplacementField

In-tree ITK module that computes the inverse of a displacement field using the
fixed-point iteration of Chen et al. (*A simple fixed-point approach to invert
a deformation field*, Medical Physics 35(1):81, 2008). Given a field mapping
space A → B, it produces the field mapping B → A.

Flagship class: `itk::FixedPointInverseDisplacementFieldImageFilter`.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/ITKFixedPointInverseDisplacementField**](https://github.com/InsightSoftwareConsortium/ITKFixedPointInverseDisplacementField)
on 2026-06-03 via the v4 ingestion pipeline. The upstream repository will be
archived read-only after this PR merges; it remains reachable at the URL above
for historical reference.
