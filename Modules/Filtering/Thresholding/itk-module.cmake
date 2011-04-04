itk_module(ITK-Thresholding DEPENDS ITK-ImageIntensity ITK-ImageStatistics TEST_DEPENDS ITK-TestKernel ITK-SignedDistanceFunction ITK-ImageLabel)
#extra test dependency on ITK-SignedDistanceFunction  is introduced by  itkBinaryThresholdSpatialFunctionTest.
#extra test dependency on ITK-Smoothing is introduced by  itkBinaryThresholdProjectionImageFilterTest.
