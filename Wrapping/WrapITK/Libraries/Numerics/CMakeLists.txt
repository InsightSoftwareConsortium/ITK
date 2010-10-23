WRAP_LIBRARY(Numerics)

set(WRAPPER_LIBRARY_GROUPS
  ITKCostFunctions
  itkDenseFrequencyContainer
  itkSample
  itkHistogram
  itkListSampleBase
  itkListSample
  itkMembershipFunctionBase
)

set(WRAPPER_LIBRARY_DEPENDS VXLNumerics Base VXLNumerics)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ITKCommon ITKNumerics ITKStatistics)

AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()
