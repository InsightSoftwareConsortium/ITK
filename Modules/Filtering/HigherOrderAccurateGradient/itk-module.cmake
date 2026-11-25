# Maintainer: Matt McCormick <matt.mccormick@kitware.com>
itk_module(
  HigherOrderAccurateGradient
  DEPENDS
    ITKCommon
    ITKImageGradient
    ITKImageIntensity
    ITKImageFeature
  TEST_DEPENDS
    ITKTestKernel
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION
    "Filters that compute higher-order accurate numerical derivatives and gradients from a scalar image (Insight Journal: https://www.insight-journal.org/browse/publication/775, https://hdl.handle.net/10380/3231)."
)
