set(
  ITKIODCMTK_SRC
  itkDCMTKImageIO.cxx
  itkDCMTKFileReader.cxx
  itkDCMTKImageIOFactory.cxx
  itkDCMTKSeriesFileNames.cxx
)

itk_module_add_library(
  ITKIODCMTK
  ${ITKIODCMTK_SRC}
)
if(DEFINED DCMTK_HAVE_CONFIG_H_OPTIONAL AND NOT DCMTK_HAVE_CONFIG_H_OPTIONAL)
  # Workaround Debian packaging issue - See FindDCMTK.cmake for more details
  target_compile_definitions(ITKIODCMTK PRIVATE ${DCMTK_DEFINITIONS})
endif()
