project(ITKZLIB)
set(ITKZLIB_THIRD_PARTY 1)

option(ITK_USE_SYSTEM_ZLIB "Use system-installed zlib" ${ITK_USE_SYSTEM_LIBRARIES})
mark_as_advanced(ITK_USE_SYSTEM_ZLIB)

if(ITK_USE_SYSTEM_ZLIB)
  find_package(ZLIB REQUIRED)
  set(ITKZLIB_INCLUDE_DIRS
    ${ITKZLIB_BINARY_DIR}/src
    )
  set(ITKZLIB_SYSTEM_INCLUDE_DIRS
    ${ZLIB_INCLUDE_DIRS}
    )
  set(ITKZLIB_LIBRARIES "${ZLIB_LIBRARIES}")
  set(ITKZLIB_NO_SRC 1)
else()
  set(ITKZLIB_INCLUDE_DIRS
    ${ITKZLIB_BINARY_DIR}/src
    ${ITKZLIB_SOURCE_DIR}/src
    )
  set(ITKZLIB_LIBRARIES itkzlib)
endif()

configure_file(src/itk_zlib.h.in src/itk_zlib.h)
set(ITK_LIBRARY_PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS FALSE)
itk_module_impl()

install(FILES ${ITKZLIB_BINARY_DIR}/src/itk_zlib.h
  DESTINATION ${ITKZLIB_INSTALL_INCLUDE_DIR}
  COMPONENT Development
  )
