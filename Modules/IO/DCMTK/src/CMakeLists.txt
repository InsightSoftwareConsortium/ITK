set(
  ITKIODCMTK_SRC
  itkDCMTKFileReader.cxx
  itkDCMTKImageIO.cxx
  itkDCMTKImageIOFactory.cxx
  itkDCMTKSeriesFileNames.cxx
)

itk_module_add_library(
  ITKIODCMTK
  ${ITKIODCMTK_SRC}
)

# itkDCMTKFileReader.h is an internal header in this directory (not installed).
target_include_directories(ITKIODCMTK PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

if(DEFINED DCMTK_HAVE_CONFIG_H_OPTIONAL AND NOT DCMTK_HAVE_CONFIG_H_OPTIONAL)
  # Workaround Debian packaging issue - See FindDCMTK.cmake for more details
  target_compile_definitions(ITKIODCMTK PRIVATE ${DCMTK_DEFINITIONS})
endif()
