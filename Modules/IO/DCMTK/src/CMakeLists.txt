set(ITKIODCMTK_SRC
  itkDCMTKImageIO.cxx
  itkDCMTKFileReader.cxx
  itkDCMTKImageIOFactory.cxx
  itkDCMTKSeriesFileNames.cxx
)

add_library(ITKIODCMTK ${ITK_LIBRARY_BUILD_TYPE} ${ITKIODCMTK_SRC})

itk_module_link_dependencies()

if(DEFINED DCMTK_HAVE_CONFIG_H_OPTIONAL AND NOT DCMTK_HAVE_CONFIG_H_OPTIONAL)
  # Workaround Debian packaging issue - See FindDCMTK.cmake for more details
  set_target_properties(ITKIODCMTK PROPERTIES COMPILE_DEFINITIONS ${DCMTK_DEFINITIONS})
endif()

itk_module_target(ITKIODCMTK)
