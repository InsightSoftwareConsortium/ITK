set(DOCUMENTATION "This module contains classes related to watershed
segmentation. The watershed algorithm proceeds in several steps. First, an initial
classification of all points into catchment basin regions is done by tracing
each point down its path of steepest descent to a local minima. Next,
neighboring regions and the boundaries between them are analyzed according
to some saliency measure (such as minimum boundary height) to produce a
tree of merges among adjacent regions.  These merges occur at different
maximum saliency values.  Finally, a label images are generated from the merge
tree.

This module also contains classes related to marker-based watershed
segmentation. For more information, see the Insight Journal article:

Beare, R. and Lehmann, G. \"The watershed transform in ITK - discussion
and new developments.\"  The Insight Journal - 2006 January - June.
http://hdl.handle.net/1926/202
http://www.insight-journal.org/browse/publication/92
")

itk_module(ITKWatersheds
  ENABLE_SHARED
  DEPENDS
    ITKImageIntensity
    ITKThresholding
    ITKImageGradient
    ITKSmoothing
    ITKMathematicalMorphology
  TEST_DEPENDS
    ITKTestKernel
    ITKImageFusion
  DESCRIPTION
    "${DOCUMENTATION}"
)
