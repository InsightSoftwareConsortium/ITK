project(ITKTIFF)
set(ITKTIFF_THIRD_PARTY 1)

if(ITK_USE_SYSTEM_TIFF)
  find_package(TIFF REQUIRED)
  set(
    ITKTIFF_INCLUDE_DIRS
    ${ITKTIFF_BINARY_DIR}/src
    ${TIFF_INCLUDE_DIRS}
  )
  set(ITKTIFF_LIBRARIES "TIFF::TIFF")
  set(ITKTIFF_NO_SRC 1)

  set(
    ITKTIFF_EXPORT_CODE_INSTALL
    "
  find_package(TIFF REQUIRED)
  "
  )
  set(
    ITKTIFF_EXPORT_CODE_BUILD
    "
  if(NOT ITK_BINARY_DIR)
    find_package(TIFF REQUIRED)
  endif()
  "
  )
else()
  set(
    ITKTIFF_INCLUDE_DIRS
    ${ITKTIFF_BINARY_DIR}/src
    ${ITKTIFF_BINARY_DIR}/src/itktiff/libtiff
    ${ITKTIFF_SOURCE_DIR}/src
  )
  set(ITKTIFF_LIBRARIES itktiff)
  set(
    ITK_LIBRARY_PROPERTIES
    WINDOWS_EXPORT_ALL_SYMBOLS
    FALSE
  )
endif()

configure_file(src/itk_tiff.h.in src/itk_tiff.h)

itk_module_impl()
