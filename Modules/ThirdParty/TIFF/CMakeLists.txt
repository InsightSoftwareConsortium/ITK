project(ITKTIFF)
set(ITKTIFF_THIRD_PARTY 1)

option(ITK_USE_SYSTEM_TIFF "Use system-installed tiff" ${ITK_USE_SYSTEM_LIBRARIES})
mark_as_advanced(ITK_USE_SYSTEM_TIFF)

if(ITK_USE_SYSTEM_TIFF)
  find_package(TIFF REQUIRED)
  set(ITKTIFF_INCLUDE_DIRS
    ${ITKTIFF_BINARY_DIR}/src
    ${TIFF_INCLUDE_DIRS}
    )
  set(ITKTIFF_SYSTEM_INCLUDE_DIRS
    )
  set(ITKTIFF_LIBRARIES "${TIFF_LIBRARIES}")
  set(ITKTIFF_NO_SRC 1)

else()
  set(ITKTIFF_INCLUDE_DIRS
    ${ITKTIFF_BINARY_DIR}/src
    ${ITKTIFF_BINARY_DIR}/src/itktiff/libtiff
    ${ITKTIFF_SOURCE_DIR}/src
    )
  set(ITKTIFF_LIBRARIES itktiff)
  set(ITK_LIBRARY_PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS FALSE)
endif()

configure_file(src/itk_tiff.h.in src/itk_tiff.h)

itk_module_impl()
