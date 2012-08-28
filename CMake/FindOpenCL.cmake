# - Try to find OpenCL
# This module tries to find an OpenCL implementation on your system. It supports
# AMD / ATI, Apple and NVIDIA implementations, but should work, too.
#
# Once done this will define
#  OPENCL_FOUND        - system has OpenCL
#  OPENCL_INCLUDE_DIRS  - the OpenCL include directory
#  OPENCL_LIBRARIES    - link these to use OpenCL
#
# WIN32 should work, but is untested

find_package( PackageHandleStandardArgs )

set (OPENCL_VERSION_STRING "0.1.0")
set (OPENCL_VERSION_MAJOR 0)
set (OPENCL_VERSION_MINOR 1)
set (OPENCL_VERSION_PATCH 0)

set(OPENCL_ROOT_DIR
  "${OPENCL_ROOT_DIR}"
  CACHE
  PATH
  "Path to search for opencl")

if (APPLE)

  find_library(OPENCL_LIBRARIES OpenCL DOC "OpenCL lib for OSX")
  find_path(OPENCL_INCLUDE_DIRS OpenCL/cl.h DOC "Include for OpenCL on OSX")
  find_path(_OPENCL_CPP_INCLUDE_DIRS OpenCL/cl.hpp DOC "Include for OpenCL CPP bindings on OSX")

else ()

  if (WIN32)

    find_path(OPENCL_INCLUDE_DIRS CL/cl.h )
    find_path(_OPENCL_CPP_INCLUDE_DIRS CL/cl.hpp )

    if( ${OPENCL_INCLUDE_DIRS} STREQUAL "OPENCL_INCLUDE_DIRS-NOTFOUND" )
     set( SEARCH_PATH ${OPENCL_ROOT_DIR}/inc ${OPENCL_ROOT_DIR}/common/inc ${PATH} "C:/ProgramData/NVIDIA Corporation/NVIDIA GPU Computing SDK/OpenCL/common/inc" "$ENV{ATISTREAMSDKROOT}/include" "C:/Program Files (x86)/AMD APP/include")
     find_path(OPENCL_INCLUDE_DIRS CL/cl.h PATHS ${SEARCH_PATH} )
     find_path(_OPENCL_CPP_INCLUDE_DIRS CL/cl.hpp PATHS ${SEARCH_PATH} )
    endif()

    set(_OPENCL_BASE ${OPENCL_INCLUDE_DIRS}/..)

    if(CMAKE_SIZEOF_VOID_P EQUAL 8)
     set(OPENCL_LIB_DIR ${_OPENCL_BASE}/lib/x64 ${_OPENCL_BASE}/lib/x86_64)
    else()
     set(OPENCL_LIB_DIR ${_OPENCL_BASE}/lib/Win32 ${_OPENCL_BASE}/lib/x86)
    endif()

    find_library(OPENCL_LIBRARIES OpenCL.lib PATHS ${OPENCL_LIB_DIR})

    get_filename_component(_OPENCL_INC_CAND_PRE ${OPENCL_LIBRARIES} PATH)
    set(_OPENCL_INC_CAND ${_OPENCL_INC_CAND_PRE}/../../include)

    # On Win32 search relative to the library
    find_path(OPENCL_INCLUDE_DIRS CL/cl.h PATHS "${_OPENCL_INC_CAND}")
    find_path(_OPENCL_CPP_INCLUDE_DIRS CL/cl.hpp PATHS "${_OPENCL_INC_CAND}")

  else ()

    # Unix style platforms
    find_library(OPENCL_LIBRARIES OpenCL
      PATHS ${OPENCL_ROOT_DIR}/lib ${OPENCL_ROOT_DIR}/common/lib
      ENV LD_LIBRARY_PATH
      )

    get_filename_component(OPENCL_LIB_DIR ${OPENCL_LIBRARIES} PATH)
    get_filename_component(_OPENCL_INC_CAND ${OPENCL_LIB_DIR}/../../include ABSOLUTE)

    set(_OPENCL_INC_CAND
        ${_OPENCL_INC_CAND}
        ${OPENCL_ROOT_DIR}/include
        ${OPENCL_ROOT_DIR}/inc
        ${OPENCL_ROOT_DIR}/common/inc
       )

    # The AMD SDK currently does not place its headers
    # in /usr/include, therefore also search relative
    # to the library
    find_path(OPENCL_INCLUDE_DIRS CL/cl.h PATHS ${_OPENCL_INC_CAND} /usr/local/cuda/include/)
    find_path(_OPENCL_CPP_INCLUDE_DIRS CL/cl.hpp PATHS ${_OPENCL_INC_CAND})

  endif ()

endif ()

FIND_PACKAGE_HANDLE_STANDARD_ARGS( OpenCL DEFAULT_MSG OPENCL_LIBRARIES OPENCL_INCLUDE_DIRS )

if( _OPENCL_CPP_INCLUDE_DIRS )
  set( OPENCL_HAS_CPP_BINDINGS TRUE )
  list( APPEND OPENCL_INCLUDE_DIRS ${_OPENCL_CPP_INCLUDE_DIRS} )
  # This is often the same, so clean up
  list( REMOVE_DUPLICATES OPENCL_INCLUDE_DIRS )
endif()

mark_as_advanced(
  OPENCL_INCLUDE_DIRS
  )
