CABLE_CLASS_SET(Scalar float)
CABLE_CLASS_SET(Dimension 2)
CABLE_CLASS_SET(Image "itk::Image<$Scalar,$Dimension>")
CABLE_CLASS_SET(Container "itk::ValarrayImageContainer<unsigned long, $Scalar>")

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

  itk::ImageBase<$Dimension>
  itk::Size<$Dimension>
  itk::Index<$Dimension>
  itk::ImageRegion<$Dimension>
  $Image
  $Container
  "itk::ImageSource<$Image >"
  "itk::ImageToImageFilter<$Image, $Image >"
  "itkSmartPointer_:itk::SmartPointer<$Image >"
  "itkSmartPointer_:itk::SmartPointer<$Container >"
)

LINK_LIBRARIES(
ITKCommon
VXLNumerics
${CABLE_TCL_LIBRARY}
${TCL_LIBRARY}
)
