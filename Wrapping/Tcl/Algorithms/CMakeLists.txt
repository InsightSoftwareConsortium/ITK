INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Wrapping/Tcl/Algorithms)

ADD_LIBRARY(ITKAlgorithmsTcl)

ITK_WRAP_TCL(ITKAlgorithmsTcl
  wrap_ITKAlgorithms.cxx
  wrap_itkCurvatureFlowImageFilter.cxx
  wrap_itkHistogramMatchingImageFilter.cxx
  wrap_itkImageRegistrationMethod.cxx
  wrap_itkImageToImageMetric.cxx
  wrap_itkMattesMutualInformationImageToImageMetric.cxx
  wrap_itkMeanSquaresImageToImageMetric.cxx
  wrap_itkMutualInformationImageToImageMetric.cxx
  wrap_itkNormalizedCorrelationImageToImageMetric.cxx
  wrap_itkOtsuThresholdImageCalculator.cxx
  wrap_itkPatternIntensityImageToImageMetric.cxx
  wrap_itkSimpleFuzzyConnectednessImageFilterBase.cxx
  wrap_itkSimpleFuzzyConnectednessScalarImageFilter.cxx
)

TARGET_LINK_LIBRARIES(ITKAlgorithmsTcl
  ITKIO
  ITKBasicFilters
  ${CABLE_TCL_LIBRARY}
)

INSTALL_TARGETS(/lib/InsightToolkit ITKAlgorithmsTcl)
