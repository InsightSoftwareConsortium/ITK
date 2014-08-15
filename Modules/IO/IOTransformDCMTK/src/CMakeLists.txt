set(
  IOTransformDCMTK_SRCS
  itkDCMTKTransformIO.cxx
  itkDCMTKTransformIOFactory.cxx
)

add_library(IOTransformDCMTK ${IOTransformDCMTK_SRCS})
target_link_libraries(
  IOTransformDCMTK
  ${IOTransformBase_LIBRARIES}
  ${ITKDCMTK_LIBRARIES}
)
itk_module_target(IOTransformDCMTK)
