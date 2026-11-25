# RLEImage

In-tree ITK module providing a run-length-encoded image type for
memory-efficient storage of large label volumes (segmentation masks).
The flagship class `itk::RLEImage<TPixel, VImageDimension, CounterType>`
is a drop-in replacement for `itk::Image` in many label-image
workflows, with iterators and filters that operate directly on the
encoded representation.

## Origin

Ingested from the standalone remote module
[**KitwareMedical/ITKRLEImage**](https://github.com/KitwareMedical/ITKRLEImage)
on 2026-05-04 via the v4 ingestion pipeline.  The upstream repository
will be archived read-only after this PR merges; it remains
reachable at the URL above for historical reference.

## Companion module

`itk::MorphologicalContourInterpolation` (in
`Modules/Filtering/MorphologicalContourInterpolation/`) has an
optional `TEST_DEPENDS` on this module — the
`itkMorphologicalContourInterpolationTestWithRLEImage` test
exercises that integration.

## References

- Zukić D., Vicory J., McCormick M., Wisse L., Gerig G., Yushkevich P.,
  Aylward S. *ND morphological contour interpolation.* The Insight
  Journal. 2016. <https://doi.org/10.54294/achtrg>
