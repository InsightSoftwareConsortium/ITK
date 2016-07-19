set(
  DOCUMENTATION
  "The module provides run-length encoded storage for
image content, iterators for efficient reading and writing, and a
specialization of region of interest filter which can also be used to
convert to and from regular itk::Image."
)

itk_module(
  RLEImage
  DEPENDS
    ITKCommon
    ITKImageGrid
  TEST_DEPENDS
    ITKIOImageBase
    ITKTestKernel
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION "${DOCUMENTATION}"
)
