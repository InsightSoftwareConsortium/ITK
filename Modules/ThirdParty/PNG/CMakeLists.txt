project(ITKPNG)
set(ITKPNG_THIRD_PARTY 1)

option(ITK_USE_SYSTEM_PNG "Use system-installed png" ${ITK_USE_SYSTEM_LIBRARIES})
mark_as_advanced(ITK_USE_SYSTEM_PNG)

if(ITK_USE_SYSTEM_PNG)
  find_package(PNG REQUIRED)
  set(ITKPNG_INCLUDE_DIRS
    ${ITKPNG_BINARY_DIR}/src
    )
  set(ITKPNG_SYSTEM_INCLUDE_DIRS
    ${PNG_INCLUDE_DIRS}
    ${PNG_INCLUDE_DIR}
    )
  set(ITKPNG_LIBRARIES "${PNG_LIBRARIES}")
  set(ITKPNG_NO_SRC 1)
else()
  set(ITKPNG_INCLUDE_DIRS
    ${ITKPNG_BINARY_DIR}/src
    ${ITKPNG_SOURCE_DIR}/src
    )
  set(ITKPNG_LIBRARIES itkpng)
  set(ITK_LIBRARY_PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS FALSE)
endif()

configure_file(src/itk_png.h.in src/itk_png.h)

itk_module_impl()

install(FILES ${ITKPNG_BINARY_DIR}/src/itk_png.h
  DESTINATION ${ITKPNG_INSTALL_INCLUDE_DIR}
  COMPONENT Development
  )
