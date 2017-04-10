project(ITKTIFF)
set(ITKTIFF_THIRD_PARTY 1)

option(ITK_USE_SYSTEM_TIFF "Use system-installed tiff" ${ITK_USE_SYSTEM_LIBRARIES})
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

  # Check availability of different methods to access TIFF's field
  # data structure.
  include(CheckTypeSize)
  set(CMAKE_REQUIRED_INCLUDES ${ITKTIFF_SYSTEM_INCLUDE_DIRS})
  set(CMAKE_EXTRA_INCLUDE_FILES "tiffio.h")
  check_type_size(TIFFFieldReadCount ITK_TIFF_HAS_TIFFFieldReadCount)
  check_type_size(TIFFField*  ITK_TIFF_HAS_TIFFField)
else()
  set(ITKTIFF_INCLUDE_DIRS
    ${ITKTIFF_BINARY_DIR}/src
    ${ITKTIFF_BINARY_DIR}/src/itktiff
    ${ITKTIFF_SOURCE_DIR}/src
    )
  set(ITKTIFF_LIBRARIES itktiff)
  set(ITK_TIFF_HAS_TIFFFieldReadCount 1)
  set(ITK_TIFF_HAS_TIFFField 1)
  set(ITK_LIBRARY_PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS FALSE)
endif()

configure_file(src/itk_tiff.h.in src/itk_tiff.h)

itk_module_impl()
