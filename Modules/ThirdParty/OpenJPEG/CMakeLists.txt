project(ITKOpenJPEG)
set(ITKOpenJPEG_THIRD_PARTY 1)

set(ITKOpenJPEG_INCLUDE_DIRS
  ${ITKOpenJPEG_SOURCE_DIR}/src/
  ${ITKOpenJPEG_BINARY_DIR}/src/openjpeg/src/lib/openjp2
  ${ITKOpenJPEG_SOURCE_DIR}/src/openjpeg/src/lib/openjp2
  )
set(ITKOpenJPEG_LIBRARIES itkopenjpeg)
set(ITK_LIBRARY_PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS FALSE)

itk_module_impl()

install(FILES ${ITKOpenJPEG_SOURCE_DIR}/src/itk_openjpeg.h
  DESTINATION ${ITKOpenJPEG_INSTALL_INCLUDE_DIR}
  COMPONENT Development
  )
