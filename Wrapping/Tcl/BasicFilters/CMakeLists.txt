INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Code/BasicFilters)

ADD_LIBRARY(ITKBasicFiltersTcl SHARED)
CABLE_CLASS_SET(Scalar float)
CABLE_CLASS_SET(Dimension 2)
CABLE_CLASS_SET(Image "itk::Image<$Scalar,$Dimension>")
CABLE_CLASS_SET(Source "itk::RandomImageSource<$Image >")
CABLE_CLASS_SET(Writer "itk::VTKImageWriter<$Image >")

CABLE_WRAP_TCL(ITKBasicFiltersTcl
  itk::Writer
  $Source
  $Writer
  "itk::ImageWriter<$Image >"
  "itkSmartPointer_:itk::SmartPointer<$Source >"
  "itkSmartPointer_:itk::SmartPointer<$Writer >"
)

LINK_LIBRARIES(
ITKCommon
ITKBasicFilters
VXLNumerics
${CABLE_TCL_LIBRARY}
${TCL_LIBRARY}
)
