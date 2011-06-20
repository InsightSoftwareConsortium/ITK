set(DOCUMENTATION "This module contains multiple variations of image
thresholding filters. In addition to the classical binary thresholding, you
will find here the thresholding filters based on the Otsu criterion, both for
single and multiple thresholds.")

itk_module(ITK-Thresholding
  DEPENDS
    ITK-ImageIntensity
    ITK-ImageStatistics
  TEST_DEPENDS
    ITK-TestKernel
    ITK-SignedDistanceFunction
    ITK-ImageLabel
  DESCRIPTION
    "${DOCUMENTATION}"
)

# Extra test dependency on ITK-SignedDistanceFunction  is introduced by itkBinaryThresholdSpatialFunctionTest.
# Extra test dependency on ITK-Smoothing is introduced by itkBinaryThresholdProjectionImageFilterTest.
