set(DOCUMENTATION "This module contains classes related to level-set
segmentation (new level-set framework from the ITKv4 effort). In level-set
segmentation, the segmentation contour is modeled as the zero level set of a
higher dimensional level-set function. The level-set function evolves according
to a partial differential equation based on image-based features and characteristics
of the level-set function. The segmentation evolves from an initial value for
the segmenting contour.")

itk_module(ITKLevelSetsv4
  DEPENDS
    ITKCommon
    ITKLabelMap
    ITKDistanceMap
  TEST_DEPENDS
    ITKTestKernel
    ITKFastMarching
  DESCRIPTION
    "${DOCUMENTATION}"
)
