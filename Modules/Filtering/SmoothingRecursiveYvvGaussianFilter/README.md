# SmoothingRecursiveYvvGaussianFilter

In-tree ITK module providing the Vliet/Young/Verbeek recursive
Gaussian smoothing approximation as a drop-in alternative to the
canonical `itk::SmoothingRecursiveGaussianImageFilter`. The flagship
classes are
`itk::SmoothingRecursiveYvvGaussianImageFilter`,
`itk::RecursiveLineYvvGaussianImageFilter`, and the
`ITK_USE_GPU`-gated
`itk::GPUSmoothingRecursiveYvvGaussianImageFilter`.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/ITKSmoothingRecursiveYvvGaussianFilter**](https://github.com/InsightSoftwareConsortium/ITKSmoothingRecursiveYvvGaussianFilter)
on 2026-05-08, following the v4 ingestion guidelines defined in
InsightSoftwareConsortium/ITK#6204. The upstream repository will be
archived read-only after this PR merges; it remains reachable at the
URL above for historical reference (notably the `doc/` directory and
the upstream's bespoke top-level OpenCL detection, which were
intentionally not ingested — ITK's in-tree GPU support is provided
by `ITKGPUCommon`).

## References

- Bouilhol G., Pop C., Sarrut D.
  *Recursive Implementation of Vliet/Young/Verbeek Gaussian Smoothing.*
  The Insight Journal. January-December. 2013.
  <https://doi.org/10.54294/2pejyl>
