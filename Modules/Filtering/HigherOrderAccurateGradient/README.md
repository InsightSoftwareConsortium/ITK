# HigherOrderAccurateGradient

In-tree ITK module providing higher-order accurate numerical
derivative and gradient image filters. The flagship classes
`itk::HigherOrderAccurateDerivativeImageFilter`,
`itk::HigherOrderAccurateDerivativeOperator`, and
`itk::HigherOrderAccurateGradientImageFilter` compute centered finite
differences of arbitrary even order on N-dimensional scalar images.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/ITKHigherOrderAccurateGradient**](https://github.com/InsightSoftwareConsortium/ITKHigherOrderAccurateGradient)
on 2026-05-08, following the v4 ingestion guidelines defined in
InsightSoftwareConsortium/ITK#6204. The upstream repository will be
archived read-only after this PR merges; it remains reachable at the
URL above for historical reference (notably the `doc/` directory,
which was intentionally left in the upstream archive).

## References

- McCormick M.M., Liu X., Jomier J., Marion C., Ibanez L.
  *Higher Order Accurate Derivative and Gradient Calculation in ITK.*
  The Insight Journal. January-December. 2014.
  <https://hdl.handle.net/10380/3231>
