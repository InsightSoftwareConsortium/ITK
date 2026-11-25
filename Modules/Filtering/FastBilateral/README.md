# FastBilateral

In-tree ITK module providing
`itk::FastBilateralImageFilter`, a fast approximation to the bilateral
filter that performs edge-preserving smoothing by accumulating samples
into a low-resolution intensity-spatial bilateral grid and then
interpolating back to the input grid. The approximation reduces the
asymptotic cost from `O(N * sigma_s^d * sigma_r)` for the brute-force
bilateral filter to roughly `O(N + L)` where `L` is the size of the
downsampled grid, making it tractable for 3D volumes.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/ITKFastBilateral**](https://github.com/InsightSoftwareConsortium/ITKFastBilateral)
on 2026-04-28, at upstream tag
[`v1.0.1`](https://github.com/InsightSoftwareConsortium/ITKFastBilateral/releases/tag/v1.0.1)
(commit `54931e8c`). The upstream repository will be archived read-only
after this PR merges; it remains reachable at the URL above.

## What lives here

- `include/itkFastBilateralImageFilter.h` — the templated filter.
- `include/itkFastBilateralImageFilter.hxx` — implementation.
- `test/` — three regression tests (Gaussian comparison, Insight Journal
  cake/cake_easy fixtures) plus their CID-addressed baselines.
- `wrapping/` — Python wrapping declarations.

## Reference

- Sylvain Paris and Frédo Durand, *A Fast Approximation of the Bilateral
  Filter using a Signal Processing Approach*, MIT CSAIL TR 2006-073 /
  ECCV 2006. The Insight Journal write-up that this module is derived
  from is at https://doi.org/10.54294/noo5vc.
