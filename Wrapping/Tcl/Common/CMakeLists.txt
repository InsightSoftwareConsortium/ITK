ADD_LIBRARY(ITKCommonTcl SHARED)
CABLE_CLASS_SET(ItkCommonBase
  itk::Command
  itk::DataObject
  itk::Directory
  itk::ImageIO
  itk::LightObject
  itk::LightProcessObject
  itk::Object
  itk::OutputWindow
  itk::ProcessObject
  itk::Version
)

CABLE_WRAP_TCL(ITKCommonTcl
  $ItkCommonBase
  itk::MeshRegion
  itk::Region
  itk::TimeStamp
  "itkSmartPointer_:itk::SmartPointer<$ItkCommonBase >"
)

LINK_LIBRARIES(
ITKCommon
VXLNumerics
${CABLE_TCL_LIBRARY}
${TCL_LIBRARY}
)

