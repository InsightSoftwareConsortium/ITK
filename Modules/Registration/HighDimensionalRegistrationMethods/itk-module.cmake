set(DOCUMENTATION "This module contains typical examples of regitration methods based upon the high dimensional metrics and high dimensional optimizers.")

itk_module(ITKHighDimensionalRegistrationMethods
  DEPENDS
    ITKHighDimensionalOptimizers
    ITKHighDimensionalMetrics
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
