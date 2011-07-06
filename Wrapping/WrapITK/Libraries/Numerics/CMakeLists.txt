WRAP_LIBRARY(Numerics)

set(WRAPPER_LIBRARY_GROUPS
  ITKCostFunctions
  itkShapePriorMAPCostFunctionBase
  itkShapePriorMAPCostFunction
  itkSample
  itkHistogram
  itkListSample
  itkMembershipFunctionBase
)

set(WRAPPER_LIBRARY_DEPENDS VXLNumerics Base VXLNumerics)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES})

AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()
