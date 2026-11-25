itk_module(
  PolarTransform
  COMPILE_DEPENDS
    ITKTransform
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "Forward and inverse Cartesian/polar coordinate transforms for itk::Image."
  EXCLUDE_FROM_DEFAULT
  ENABLE_SHARED
)
