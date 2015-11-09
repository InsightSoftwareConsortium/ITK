set(DOCUMENTATION "This module contains GPU implementations of the base
classes that support image filters. For the most part, these are classes
that developers will use to create new image filters (and that are the
base classes of existing image filters). You will find here: box filters,
recursive separable filters and the base classes for neighborhood filters.")

itk_module(ITKGPUImageFilterBase
  DEPENDS
    ITKCommon
    ITKGPUCommon
  COMPILE_DEPENDS
    ITKImageFilterBase
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
