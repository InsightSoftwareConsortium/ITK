set(
  IOTransformDCMTK_SRCS
  itkDCMTKTransformIO.cxx
  itkDCMTKTransformIOFactory.cxx
)

# Remove ITKDCMTK from private depends so itk_module_add_library does not link
# all DCMTK libraries transitively; only the required subset is linked below.
list(REMOVE_ITEM ITK_MODULE_IOTransformDCMTK_PRIVATE_DEPENDS ITKDCMTK)

itk_module_add_library(IOTransformDCMTK ${IOTransformDCMTK_SRCS})

target_link_libraries(IOTransformDCMTK PRIVATE dcmdata)

# itkDCMTKTransformIO.hxx is a private implementation header in this directory.
target_include_directories(IOTransformDCMTK PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
