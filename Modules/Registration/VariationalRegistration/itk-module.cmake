set(
  DOCUMENTATION
  "This module contains a collection of classes for performing
   variational image registration. See VariationalRegistrationFilter for a
   brief introduction. For further information on the algorithm, please refer to:

   Rene Werner, Alexander Schmidt-Richberg, Heinz Handels and Jan Ehrhardt:
   Estimation of lung motion fields in 4D CT data by variational non-linear
   intensity-based registration: A comparison and evaluation study,
   Phys. Med. Biol., 2014

   Information on the implementation can be found in:

   Alexander Schmidt-Richberg, Rene Werner, Heinz Handels and Jan Ehrhardt:
   A Flexible Variational Registration Framework, Insight Journal, 2014"
  # TODO Add ITK IJ handle
)

itk_module(
  VariationalRegistration
  DEPENDS
    ITKCommon
    ITKIOImageBase
    ITKImageFilterBase
    ITKSmoothing
    ITKFFT
    ITKFiniteDifference
    ITKDisplacementField
    ITKRegistrationCommon
    ITKMathematicalMorphology
    ITKBinaryMathematicalMorphology
    ITKTestKernel #necessary to handle IO in src
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION "${DOCUMENTATION}"
)
