# Maintainer: Matt McCormick <matt.mccormick@kitware.com>
set(
  DOCUMENTATION
  "This module contains a filter to compute higher order
accurate numerical derivatives and gradients from an input scalar image.
Higher Order Accurate Derivative and Gradient Calculation in ITK
https://www.insight-journal.org/browse/publication/775
https://hdl.handle.net/10380/3231
"
)

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
  DESCRIPTION "${DOCUMENTATION}"
)
