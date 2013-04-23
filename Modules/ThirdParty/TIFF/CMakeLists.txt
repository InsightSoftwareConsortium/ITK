project(ITKTIFF)
set(ITKTIFF_THIRD_PARTY 1)

option(ITK_USE_SYSTEM_TIFF "Use system-installed tiff" OFF)
mark_as_advanced(ITK_USE_SYSTEM_TIFF)

if(ITK_USE_SYSTEM_TIFF)
  find_package(TIFF REQUIRED)
  set(ITKTIFF_INCLUDE_DIRS
    ${ITKTIFF_BINARY_DIR}/src
    )
  set(ITKTIFF_SYSTEM_INCLUDE_DIRS
    ${TIFF_INCLUDE_DIR}
    )
  set(ITKTIFF_LIBRARIES "${TIFF_LIBRARIES}")
  set(ITKTIFF_NO_SRC 1)
else()
  set(ITKTIFF_INCLUDE_DIRS
    ${ITKTIFF_BINARY_DIR}/src
    ${ITKTIFF_BINARY_DIR}/src/itktiff
    ${ITKTIFF_SOURCE_DIR}/src
    )
  set(ITKTIFF_LIBRARIES itktiff)
endif()

configure_file(src/itk_tiff.h.in src/itk_tiff.h)

itk_module_impl()
