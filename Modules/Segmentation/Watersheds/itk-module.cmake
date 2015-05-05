set(DOCUMENTATION "This module contains classes related to watershed
segmentation. The watershed algorithm proceeds in several steps. First, an initial
classification of all points into catchment basin regions is done by tracing
each point down its path of steepest descent to a local minima. Next,
neighboring regions and the boundaries between them are analyzed according
to some saliency measure (such as minimum boundary height) to produce a
tree of merges among adjacent regions.  These merges occur at different
maximum saliency values.  Finally, a label images are generated from the merge
tree.")

itk_module(ITKWatersheds
  ENABLE_SHARED
  DEPENDS
    ITKImageIntensity
    ITKThresholding
    ITKImageGradient
    ITKSmoothing
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
