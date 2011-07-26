set(DOCUMENTATION "This module contains algorithms to classify pixels in an
image.  It can be used, for example, to identify pixel membership within a set
of tissue types.  Different algorithms are available including Bayesian
classification, Gaussian models, and K-means clustering.  After tissue labels
have been assigned, they can be modified and applied with the \\\\ref
ITKLabelMap.")

itk_module(ITKClassifiers
  DEPENDS
    ITKImageGrid
    ITKStatistics
    ITKConnectedComponents
  TEST_DEPENDS
    ITKTestKernel
    ITKAnisotropicSmoothing
  DESCRIPTION
    "${DOCUMENTATION}"
)

# Extra test dependency on ITKAnisotropicSmoothing is introduced by itkBayesianClassifierImageFilterTest.
