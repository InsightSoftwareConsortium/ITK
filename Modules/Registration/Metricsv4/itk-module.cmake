set(DOCUMENTATION "This module contains ITK metric classes using a new hierarchy developed for the needs of registration with high-dimensional transforms. These metrics will NOT work with the optimizers in Numerics/Optimizers, but rather with the new optimizers in Numerics/Optimizersv4.")

itk_module(ITKMetricsv4
  DEPENDS
    ITKCommon
    ITKRegistrationCommon
    ITKOptimizersv4
  TEST_DEPENDS
    ITKTestKernel
    ITKOptimizersv4
  DESCRIPTION
    "${DOCUMENTATION}"
)
