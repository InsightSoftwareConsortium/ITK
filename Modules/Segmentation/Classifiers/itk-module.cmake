itk_module(ITKClassifiers
  DEPENDS
    ITKImageGrid
    ITKStatistics
    ITKConnectedComponents
  TEST_DEPENDS
    ITKTestKernel
    ITKAnisotropicSmoothing
)

# Extra test dependency on ITKAnisotropicSmoothing is introduced by itkBayesianClassifierImageFilterTest.
