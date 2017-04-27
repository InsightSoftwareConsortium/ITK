set(DOCUMENTATION "This module contains ITK classes that encapsulate numerical
optimizers using a new hierarchy developed for the needs of registration with high-dimensional transforms. These optimizers will NOT work with the metrics in Registration/Common, but rather with the new metrics in Registration/Metricsv4.")

itk_module(ITKOptimizersv4
  ENABLE_SHARED
  DEPENDS
    ITKOptimizers
  PRIVATE_DEPENDS
    ITKLIBLBFGS
  COMPILE_DEPENDS
    ITKCommon
    ITKTransform
    ITKImageGrid
    ITKDisplacementField
  TEST_DEPENDS
    ITKTestKernel
    ITKMetricsv4
  DESCRIPTION
    "${DOCUMENTATION}"
)

# ITKOptimizers dependency added to get itkCostFunction for itkSingleValuedCostFunctionv4
