itk_module(ITKRegistrationCommon
  DEPENDS
    ITKOptimizers
    ITKImageIntensity
    ITKImageFunction
    ITKImageGrid
    ITKSpatialObjects
    ITKSmoothing
    ITKImageGradient
    ITKImageFeature
    ITKFiniteDifference
  TEST_DEPENDS
    ITKTestKernel
    ITKDistanceMap
)

# Extra test dependency on ITKDistanceMap is introduced by itkPointSetToPointSetRegistrationTest.
