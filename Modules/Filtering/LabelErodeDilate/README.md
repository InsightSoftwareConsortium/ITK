# LabelErodeDilate

In-tree ITK module providing morphological erosion and dilation
filters for label images. Label collisions are handled consistently
and the operations execute in approximately constant time with
respect to structuring-element size. Only circular, spherical, and
hyperspherical structuring elements are supported.

The flagship classes are `itk::LabelSetDilateImageFilter` and
`itk::LabelSetErodeImageFilter`, with shared infrastructure in
`itk::LabelSetMorphBaseImageFilter`.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/ITKLabelErodeDilate**](https://github.com/InsightSoftwareConsortium/ITKLabelErodeDilate)
on 2026-04-29. The upstream repository will be archived read-only
after this PR merges; it remains reachable at the URL above.

## References

- Beare R. *A morphological approach to vessel segmentation.* The Insight Journal. 2018. <https://doi.org/10.54294/j8lsa66>
- Beare R. *Histogram-based thresholding — some missing methods.* The Insight Journal. 2011. <https://doi.org/10.54294/aq68pt>
