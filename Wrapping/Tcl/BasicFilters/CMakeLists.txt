INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Code/BasicFilters)

ADD_LIBRARY(ITKBasicFiltersTcl SHARED)
CABLE_CLASS_SET(Scalar float)
CABLE_CLASS_SET(Dimension 2)
CABLE_CLASS_SET(Image "itk::Image<$Scalar,$Dimension>")
CABLE_CLASS_SET(Source "itk::RandomImageSource<$Image >")

CABLE_WRAP_TCL(ITKBasicFiltersTcl
  $Source
  "itkSmartPointer_:itk::SmartPointer<$Source >"
)

LINK_LIBRARIES(
ITKCommon
ITKBasicFilters
VXLNumerics
${CABLE_TCL_LIBRARY}
${TCL_LIBRARY}
)
