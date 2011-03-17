project(ITK-ZLIB)

option(ITK_USE_SYSTEM_ZLIB "Use system-installed zlib" OFF)
mark_as_advanced(ITK_USE_SYSTEM_ZLIB)

if(ITK_USE_SYSTEM_ZLIB)
  find_package(ZLIB REQUIRED)
  set(ITK-ZLIB_INCLUDE_DIRS
    ${ITK-ZLIB_BINARY_DIR}/src
    ${ZLIB_INCLUDE_DIRS}
    )
  set(ITK-ZLIB_LIBRARIES "${ZLIB_LIBRARIES}")
  set(itk-module-config-install "
list(APPEND ITK-ZLIB_LIBRARIES \"${ITK-ZLIB_LIBRARIES}\")
list(APPEND ITK-ZLIB_INCLUDE_DIRS \"${ITK-ZLIB_INCLUDE_DIRS}\")
")
  set(itk-module-config-build "
list(APPEND ITK-ZLIB_LIBRARIES \"${ITK-ZLIB_LIBRARIES}\")
list(APPEND ITK-ZLIB_INCLUDE_DIRS \"${ITK-ZLIB_INCLUDE_DIRS}\")
")
  set(itk-module-no-include 1)
  set(itk-module-no-src 1)
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
