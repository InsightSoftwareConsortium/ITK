set(DOCUMENTATION "This module contains classes and support classes for the
calculation of artificial neural networks.  This can be used, for instance, for
image classification.")

itk_module(ITKNeuralNetworks
  DEPENDS
    ITKStatistics
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
