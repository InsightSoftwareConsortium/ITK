itk_module(ITK-Classifiers
  DEPENDS
    ITK-ImageGrid
    ITK-Statistics
    ITK-ConnectedComponents
  TEST_DEPENDS
    ITK-TestKernel
    ITK-AnisotropicSmoothing
)

# Extra test dependency on ITK-AnisotropicSmoothing is introduced by itkBayesianClassifierImageFilterTest.
