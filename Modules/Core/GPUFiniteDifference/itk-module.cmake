set(DOCUMENTATION "This module contains the GPU implementations of base classes
neede for finite difference image filters.  Most of the classes in this module
are abstract and therefore are not intended to be instantiated by themselves.")

itk_module(ITKGPUFiniteDifference
  DEPENDS
    ITKCommon
    ITKGPUCommon
  COMPILE_DEPENDS
    ITKFiniteDifference
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
