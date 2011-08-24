set(DOCUMENTATION "This module contains ITK metric classes using a new hierarchy developed for the needs of registration with high-dimensional transforms. These metrics will NOT work with the optimizers in Numerics/Optimizers, but rather with the new optimizers in Numerics/HighDimensionalOptimizers.")

itk_module(ITKHighDimensionalMetrics
  DEPENDS
    ITKTransform
    ITKOptimizers
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
