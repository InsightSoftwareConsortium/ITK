set(
  IOTransformDCMTK_SRCS
  itkDCMTKTransformIO.cxx
  itkDCMTKTransformIOFactory.cxx
)

itk_module_add_library(IOTransformDCMTK ${IOTransformDCMTK_SRCS})
