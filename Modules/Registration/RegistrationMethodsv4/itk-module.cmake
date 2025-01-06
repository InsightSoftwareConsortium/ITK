set(DOCUMENTATION
    "This module contains typical examples of registration methods based upon the high dimensional metrics and high dimensional optimizers."
)

itk_module(
  ITKRegistrationMethodsv4
  ENABLE_SHARED
  DEPENDS
  ITKOptimizersv4
  ITKMetricsv4
  TEST_DEPENDS
  ITKTestKernel
  DESCRIPTION
  "${DOCUMENTATION}")
