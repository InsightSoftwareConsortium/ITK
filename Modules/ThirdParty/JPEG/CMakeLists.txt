project(ITK-JPEG)
set(ITK-JPEG_THIRD_PARTY 1)

option(ITK_USE_SYSTEM_JPEG "Use an outside build of JPEG." OFF)
mark_as_advanced(ITK_USE_SYSTEM_JPEG)

if(ITK_USE_SYSTEM_JPEG)
  find_package(JPEG REQUIRED)
  set(ITK-JPEG_INCLUDE_DIRS
    ${ITK-JPEG_BINARY_DIR}/src
    )
  set(ITK-JPEG_SYSTEM_INCLUDE_DIRS
    ${JPEG_INCLUDE_DIRS}
    )
  set(ITK-JPEG_LIBRARIES "${JPEG_LIBRARIES}")
  set(ITK-JPEG_NO_SRC 1)
else()
  set(ITK-JPEG_INCLUDE_DIRS
    ${ITK-JPEG_BINARY_DIR}/src
    ${ITK-JPEG_SOURCE_DIR}/src
    )
  set(ITK-JPEG_LIBRARIES itkjpeg)
endif()

configure_file(src/itk_jpeg.h.in src/itk_jpeg.h)

itk_module_impl()

install(FILES ${ITK-JPEG_BINARY_DIR}/src/itk_jpeg.h
  DESTINATION ${ITK-JPEG_INSTALL_INCLUDE_DIR})
