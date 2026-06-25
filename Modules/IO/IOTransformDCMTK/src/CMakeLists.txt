set(
  IOTransformDCMTK_SRCS
  itkDCMTKTransformIO.cxx
  itkDCMTKTransformIOFactory.cxx
)

itk_module_add_library(IOTransformDCMTK ${IOTransformDCMTK_SRCS})

# itkDCMTKTransformIO.hxx is a private implementation header in this directory.
target_include_directories(IOTransformDCMTK PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
