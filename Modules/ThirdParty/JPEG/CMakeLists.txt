project(ITKJPEG)
set(ITKJPEG_THIRD_PARTY 1)

option(
  ITK_USE_SYSTEM_JPEG
  "Use an outside build of JPEG."
  ${ITK_USE_SYSTEM_LIBRARIES}
)
mark_as_advanced(ITK_USE_SYSTEM_JPEG)

if(ITK_USE_SYSTEM_JPEG)
  find_package(JPEG REQUIRED)
  set(ITKJPEG_INCLUDE_DIRS ${ITKJPEG_BINARY_DIR}/src)
  set(ITKJPEG_LIBRARIES "JPEG::JPEG")
  set(ITKJPEG_NO_SRC 1)

  set(
    ITKJPEG_EXPORT_CODE_INSTALL
    "
find_package(JPEG REQUIRED)
"
  )
  set(
    ITKJPEG_EXPORT_CODE_BUILD
    "
if(NOT ITK_BINARY_DIR)
  find_package(JPEG REQUIRED)
endif()
"
  )
else()
  set(
    ITKJPEG_INCLUDE_DIRS
    ${ITKJPEG_BINARY_DIR}/src
    ${ITKJPEG_SOURCE_DIR}/src
  )
  set(ITKJPEG_LIBRARIES itkjpeg)
endif()

configure_file(src/itk_jpeg.h.in src/itk_jpeg.h)

itk_module_impl()

install(
  FILES
    ${ITKJPEG_BINARY_DIR}/src/itk_jpeg.h
  DESTINATION ${ITKJPEG_INSTALL_INCLUDE_DIR}
  COMPONENT Development
)
