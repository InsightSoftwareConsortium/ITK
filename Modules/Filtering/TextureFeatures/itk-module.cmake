itk_module(
  TextureFeatures
  DEPENDS
    ITKCommon
    ITKStatistics
    ITKImageGrid
    ITKMathematicalMorphology
  TEST_DEPENDS
    ITKTestKernel
    ITKMetaIO
    ITKImageIntensity
    ITKImageNoise
    ITKGoogleTest
  DESCRIPTION
    "N-dimensional textural feature image filters: first-order, run-length, and co-occurrence (GLCM) features computed over a sliding window for radiomics and computer-vision pipelines."
  EXCLUDE_FROM_DEFAULT
)
