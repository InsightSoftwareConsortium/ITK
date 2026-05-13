# Maintainer: Joel Schaerer
itk_module(
  MultipleImageIterator
  DEPENDS
    ITKCommon
  TEST_DEPENDS
    ITKTestKernel
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION
    "An iterator wrapper that walks multiple co-registered images in lockstep, returning a std::vector of pixel values at each position. https://doi.org/10.54294/e5lmyz"
)
