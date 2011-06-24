set(DOCUMENTATION "This module contains filters that implement variations of
anisotropic smoothing. This is an image denoising technique that strives for
preserving edges on the images while smoothing regions of uniform intensity.
This type of filtering is convenient as a preprocessing stage of segmentation
algorithms. You may find useful as well the filters in the ITK-CurvatureFlow
module and the ITK-SmoothingModule.")

itk_module(ITK-AnisotropicSmoothing
  DEPENDS
    ITK-ImageGrid
    ITK-CurvatureFlow
    ITK-FiniteDifference
  TEST_DEPENDS
    ITK-TestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
