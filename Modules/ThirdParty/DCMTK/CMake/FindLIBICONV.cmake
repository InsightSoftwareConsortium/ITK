# Find iconv library
#
# Released under BSD license
#
#  LIBICONV_INCLUDE_DIRS - where to find iconv.h, etc
#  LIBICONV_LIBRARIES    - Lists of libraries when using iconv
#  LIBICONV_FOUND        - True if iconv found

include(FindPackageHandleStandardArgs)

# Look for the header file
if(NOT LIBICONV_INCLUDE_DIR)
  find_path(LIBICONV_INCLUDE_DIR NAMES iconv.h)
endif()
mark_as_advanced(LIBICONV_INCLUDE_DIR)

# Look for the library
set(LIBICONV_LIBS iconv)

if(NOT LIBICONV_LIBRARY)
  find_library(LIBICONV_LIBRARY NAMES ${LIBICONV_LIBS})
endif()
mark_as_advanced(LIBICONV_LIBRARY)

find_package_handle_standard_args(
  LIBICONV
  REQUIRED_VARS
    LIBICONV_LIBRARY
    LIBICONV_INCLUDE_DIR
)

# Copy the result to output variables
if(LIBICONV_FOUND)
  set(LIBICONV_LIBRARIES ${LIBICONV_LIBRARY})
  set(LIBICONV_INCLUDE_DIRS ${LIBICONV_INCLUDE_DIR})
  include_directories(${LIBICONV_INCLUDE_DIR})
  link_directories(${LIBICONV_LIBDIR})
else(LIBICONV_FOUND)
  set(LIBICONV_LIBS)
  set(LIBICONV_LIBRARY)
  set(LIBICONV_LIBRARIES)
  set(LIBICONV_INCLUDE_DIR)
  set(LIBICONV_INCLUDE_DIRS)
endif(LIBICONV_FOUND)
