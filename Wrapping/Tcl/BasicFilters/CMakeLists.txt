INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Wrapping/Tcl/BasicFilters)
INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Code/BasicFilters)

ADD_LIBRARY(ITKBasicFiltersTcl)

CABLE_WRAP_TCL(ITKBasicFiltersTcl
  wrap_ITKBasicFilters.cxx
  wrap_itkRandomImageSource.cxx
  wrap_itkRecursiveSeparableImageFilter.cxx
  wrap_itkRecursiveGaussianImageFilter.cxx
)

LINK_LIBRARIES(ITKBasicFilters ITKCommon VXLNumerics ${CABLE_TCL_LIBRARY})

