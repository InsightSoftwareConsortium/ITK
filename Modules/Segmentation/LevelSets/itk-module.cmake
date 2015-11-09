set(DOCUMENTATION "This module contains classes related to level-set
segmentation.  In level-set segmentation, the segmentation contour is modeled as
the zero level set of a higher dimensional level-set function.  The level-set
function evolves according to a differential equation based on image-based
features and characteristics of the level-set function.  The segmentation
evolves from an initial value for the segmenting contour.")

itk_module(ITKLevelSets
  DEPENDS
    ITKImageFeature
    ITKFiniteDifference
    ITKDistanceMap
    ITKSignedDistanceFunction
    ITKAnisotropicSmoothing
    ITKThresholding
    ITKOptimizers
    ITKImageCompare
    ITKFastMarching
    ITKNarrowBand
  COMPILE_DEPENDS
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
