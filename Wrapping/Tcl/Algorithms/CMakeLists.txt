INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Wrapping/Tcl/Algorithms)
INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Code/BasicFilters)
INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Code/Algorithms)

ADD_LIBRARY(ITKAlgorithmsTcl)

ITK_WRAP_TCL(ITKAlgorithmsTcl
  wrap_ITKAlgorithms.cxx
  wrap_itkImageToImageMetric.cxx
  wrap_itkMeanSquaresImageToImageMetric.cxx
  wrap_itkMutualInformationImageToImageMetric.cxx
  wrap_itkNormalizedCorrelationImageToImageMetric.cxx
  wrap_itkPatternIntensityImageToImageMetric.cxx
)

TARGET_LINK_LIBRARIES(ITKAlgorithmsTcl
  ITKCommon
  ITKIO
  ITKBasicFilters
  ITKNumerics
  VXLNumerics
  ${CABLE_TCL_LIBRARY}
)

