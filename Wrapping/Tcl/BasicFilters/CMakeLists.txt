INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Wrapping/Tcl/BasicFilters)
INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Code/BasicFilters)

ADD_LIBRARY(ITKBasicFiltersTcl)

ITK_WRAP_TCL(ITKBasicFiltersTcl
  wrap_ITKBasicFilters.cxx
  wrap_itkRandomImageSource.cxx
  wrap_itkRecursiveSeparableImageFilter.cxx
  wrap_itkRecursiveGaussianImageFilter.cxx
  wrap_itkRescaleIntensityImageFilter.cxx
  wrap_itkBinaryThresholdImageFilter.cxx
)

TARGET_LINK_LIBRARIES(ITKBasicFiltersTcl
  ITKBasicFilters
  ITKCommon
  ITKNumerics
  VXLNumerics
  ${CABLE_TCL_LIBRARY}
)
