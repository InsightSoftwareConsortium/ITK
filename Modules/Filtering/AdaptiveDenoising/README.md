# AdaptiveDenoising

In-tree ITK module providing patch-based adaptive denoising filters for
scalar and vector images. The flagship class is
`itk::AdaptiveNonLocalMeansDenoisingImageFilter`, which implements a
non-local-means denoiser whose patch-similarity radius and smoothing
strength adapt to local image content. Supporting infrastructure
includes `itk::NonLocalPatchBasedImageFilter` (the base class for
non-local-means style filters) and `itk::VarianceImageFilter` (a
neighborhood variance estimator used to drive the adaptive parameter
selection).

## Origin

Ingested from the standalone remote module
[**ntustison/ITKAdaptiveDenoising**](https://github.com/ntustison/ITKAdaptiveDenoising)
on 2026-05-08, following the v4 ingestion guidelines defined in
InsightSoftwareConsortium/ITK#6204. The upstream repository will be
archived read-only after this PR merges; it remains reachable at the
URL above for historical reference (notably the `examples/`
directory, which was intentionally left in the upstream archive).

## References

- Manjón J.V., Coupé P., Martí-Bonmatí L., Collins D.L., Robles M.
  *Adaptive non-local means denoising of MR images with spatially
  varying noise levels.* Journal of Magnetic Resonance Imaging.
  2010;31(1):192-203.
- Tustison N., Manjón J.V. *Adaptive Non-Local Means Filtering for
  Image Denoising.* The Insight Journal. 2020.
  <https://doi.org/10.54294/9f5wt3>
