project(ITKPNG)
set(ITKPNG_THIRD_PARTY 1)

if(ITK_USE_SYSTEM_PNG)
  find_package(PNG REQUIRED)
  set(ITKPNG_INCLUDE_DIRS ${ITKPNG_BINARY_DIR}/src)
  set(ITKPNG_LIBRARIES "PNG::PNG")
  set(ITKPNG_NO_SRC 1)

  set(
    ITKPNG_EXPORT_CODE_INSTALL
    "
find_package(PNG REQUIRED)
"
  )
  set(
    ITKPNG_EXPORT_CODE_BUILD
    "
if(NOT ITK_BINARY_DIR)
  find_package(PNG REQUIRED)
endif()
"
  )
else()
  set(
    ITKPNG_INCLUDE_DIRS
    ${ITKPNG_BINARY_DIR}/src
    ${ITKPNG_BINARY_DIR}/src/itkpng
    ${ITKPNG_SOURCE_DIR}/src
  )
  set(ITKPNG_LIBRARIES itkpng)
  set(
    ITK_LIBRARY_PROPERTIES
    WINDOWS_EXPORT_ALL_SYMBOLS
    FALSE
  )
endif()

configure_file(src/itk_png.h.in src/itk_png.h)

itk_module_impl()

install(
  FILES
    ${ITKPNG_BINARY_DIR}/src/itk_png.h
  DESTINATION ${ITKPNG_INSTALL_INCLUDE_DIR}
  COMPONENT Development
)
