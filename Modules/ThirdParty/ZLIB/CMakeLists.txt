project(ITK-ZLIB)
set(ITK-ZLIB_THIRD_PARTY 1)

option(ITK_USE_SYSTEM_ZLIB "Use system-installed zlib" OFF)
mark_as_advanced(ITK_USE_SYSTEM_ZLIB)

if(ITK_USE_SYSTEM_ZLIB)
  find_package(ZLIB REQUIRED)
  set(ITK-ZLIB_INCLUDE_DIRS
    ${ITK-ZLIB_BINARY_DIR}/src
    )
  set(ITK-ZLIB_SYSTEM_INCLUDE_DIRS
    ${ZLIB_INCLUDE_DIRS}
    )
  set(ITK-ZLIB_LIBRARIES "${ZLIB_LIBRARIES}")
  set(ITK-ZLIB_NO_SRC 1)
else()
  set(ITK-ZLIB_INCLUDE_DIRS
    ${ITK-ZLIB_BINARY_DIR}/src
    ${ITK-ZLIB_SOURCE_DIR}/src
    )
  set(ITK-ZLIB_LIBRARIES itkzlib)
endif()

configure_file(src/itk_zlib.h.in src/itk_zlib.h)

itk_module_impl()

install(FILES ${ITK-ZLIB_BINARY_DIR}/src/itk_zlib.h
  DESTINATION ${ITK-ZLIB_INSTALL_INCLUDE_DIR})
