set(
  IOTransformDCMTK_SRCS
  itkDCMTKTransformIO.cxx
  itkDCMTKTransformIOFactory.cxx
)

add_library(
  IOTransformDCMTK
  ${ITK_LIBRARY_BUILD_TYPE}
  ${IOTransformDCMTK_SRCS}
)
itk_module_link_dependencies()
itk_module_target(IOTransformDCMTK)
