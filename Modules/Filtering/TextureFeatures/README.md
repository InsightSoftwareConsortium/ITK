# TextureFeatures

In-tree ITK module providing N-dimensional textural feature image
filters. The flagship classes compute first-order, run-length, and
co-occurrence (GLCM) texture features over a sliding window for each
voxel of an input image, producing per-feature output maps suitable
for radiomics and computer-vision pipelines.

The flagship filters are
`itk::CoocurrenceTextureFeaturesImageFilter`,
`itk::FirstOrderTextureFeaturesImageFilter`, and
`itk::RunLengthTextureFeaturesImageFilter`, with supporting
`itk::DigitizerFunctor` and `itk::FirstOrderTextureHistogram` building
blocks.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/ITKTextureFeatures**](https://github.com/InsightSoftwareConsortium/ITKTextureFeatures)
on 2026-05-08, following the v4 ingestion guidelines defined in
InsightSoftwareConsortium/ITK#6204. The upstream repository will be
archived read-only after this PR merges; it remains reachable at the
URL above for historical reference (notably the `doc/` and `example/`
directories, which were intentionally left in the upstream archive).

## References

- Vimort J., McCormick M., Budin F., Paniagua B.
  *Computing Textural Feature Maps for N-Dimensional images.*
  The Insight Journal. January-December. 2017.
  <https://doi.org/10.54294/qy48ty>
