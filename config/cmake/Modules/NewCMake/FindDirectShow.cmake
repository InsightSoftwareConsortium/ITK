# - Test for DirectShow on Windows.
# Once loaded this will define
#   DIRECTSHOW_FOUND        - system has DirectShow
#   DIRECTSHOW_INCLUDE_DIRS - include directory for DirectShow
#   DIRECTSHOW_LIBRARIES    - libraries you need to link to

set(DIRECTSHOW_FOUND "NO")

# DirectShow is only available on Windows platforms
if(MSVC)
  # Find DirectX Include Directory (dshow depends on it)
  find_path(DIRECTX_INCLUDE_DIR ddraw.h
    # WindowsSDK: includes ddraw and dshow
    "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Microsoft SDKs\\Windows;CurrentInstallFolder]/Include"
    # VS 7.1 PlatformSDK: includes ddraw and dshow
    "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\VisualStudio\\7.1\\Setup\\VC;ProductDir]/PlatformSDK/Include"
    # Newer DirectX: dshow not included; requires Platform SDK
    "$ENV{DXSDK_DIR}/Include"
    # Older DirectX: dshow included
    "C:/DXSDK/Include"
    DOC "What is the path where the file ddraw.h can be found"
    NO_DEFAULT_PATH
  )

  # if DirectX found, then find DirectShow include directory
  if(DIRECTX_INCLUDE_DIR)
    find_path(DIRECTSHOW_INCLUDE_DIR dshow.h
      "${DIRECTX_INCLUDE_DIR}"
      "C:/Program Files/Microsoft Platform SDK for Windows Server 2003 R2/Include"
      "C:/Program Files/Microsoft Platform SDK/Include"
      DOC "What is the path where the file dshow.h can be found"
      NO_DEFAULT_PATH
    )

    # if DirectShow include dir found, then find DirectShow libraries
    if(DIRECTSHOW_INCLUDE_DIR)
      if(CMAKE_CL_64)
        find_library(DIRECTSHOW_STRMIIDS_LIBRARY strmiids
          "${DIRECTSHOW_INCLUDE_DIR}/../Lib/x64"
          DOC "Where can the DirectShow strmiids library be found"
          NO_DEFAULT_PATH
          )
        find_library(DIRECTSHOW_QUARTZ_LIBRARY quartz
          "${DIRECTSHOW_INCLUDE_DIR}/../Lib/x64"
          DOC "Where can the DirectShow quartz library be found"
          NO_DEFAULT_PATH
          )
      else()
        find_library(DIRECTSHOW_STRMIIDS_LIBRARY strmiids
          "${DIRECTSHOW_INCLUDE_DIR}/../Lib"
          "${DIRECTSHOW_INCLUDE_DIR}/../Lib/x86"
          DOC "Where can the DirectShow strmiids library be found"
          NO_DEFAULT_PATH
          )
        find_library(DIRECTSHOW_QUARTZ_LIBRARY quartz
          "${DIRECTSHOW_INCLUDE_DIR}/../Lib"
          "${DIRECTSHOW_INCLUDE_DIR}/../Lib/x86"
          DOC "Where can the DirectShow quartz library be found"
          NO_DEFAULT_PATH
          )
      endif()
    endif()
  endif()
endif()

if(DIRECTSHOW_INCLUDE_DIR)
#---------------------------------------------------------------------
set(DIRECTSHOW_INCLUDE_DIRS
  "${DIRECTX_INCLUDE_DIR}"
  "${DIRECTSHOW_INCLUDE_DIR}"
  )

set(DIRECTSHOW_LIBRARIES
  "${DIRECTSHOW_STRMIIDS_LIBRARY}"
  "${DIRECTSHOW_QUARTZ_LIBRARY}"
  )

#---------------------------------------------------------------------
include(CheckCXXSourceCompiles)

set(CMAKE_REQUIRED_INCLUDES  ${DIRECTSHOW_INCLUDE_DIRS})
set(CMAKE_REQUIRED_LIBRARIES ${DIRECTSHOW_LIBRARIES})
CHECK_CXX_SOURCE_COMPILES("
  #include <atlbase.h>
  #include <dshow.h>
  #include <qedit.h>

  int main()
  {
    CComPtr<IFilterGraph2> filter_graph;
    filter_graph.CoCreateInstance(CLSID_FilterGraph);
    return 0;
  }
" DIRECTSHOW_SOURCE_COMPILES)
set(CMAKE_REQUIRED_INCLUDES)
set(CMAKE_REQUIRED_LIBRARIES)

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(
  DIRECTSHOW
  DEFAULT_MSG
  DIRECTX_INCLUDE_DIR
  DIRECTSHOW_INCLUDE_DIR
  DIRECTSHOW_STRMIIDS_LIBRARY
  DIRECTSHOW_QUARTZ_LIBRARY
  DIRECTSHOW_SOURCE_COMPILES
  )

set(_NAME DIRECTSHOW)
set(_NAME_UPPER DIRECTSHOW)
set(MISSING_VARS "")
# check if all passed variables are valid
set(${_NAME_UPPER}_FOUND TRUE)
foreach(_CURRENT_VAR
    DIRECTX_INCLUDE_DIR
    DIRECTSHOW_INCLUDE_DIR
    DIRECTSHOW_STRMIIDS_LIBRARY
    DIRECTSHOW_QUARTZ_LIBRARY
    DIRECTSHOW_SOURCE_COMPILES
    )
  if(NOT "${_CURRENT_VAR}")
    set(${_NAME_UPPER}_FOUND FALSE)
    set(MISSING_VARS "${MISSING_VARS} ${_CURRENT_VAR}")
  endif()
endforeach()

if(${_NAME_UPPER}_FOUND)
  if(NOT ${_NAME}_FIND_QUIETLY)
    message(STATUS "Found ${_NAME}")
  endif()
else()
  if(${_NAME}_FIND_REQUIRED)
    message(FATAL_ERROR "Could NOT find ${_NAME} (missing: ${MISSING_VARS})")
  else()
    if(NOT ${_NAME}_FIND_QUIETLY)
      message(STATUS "Could NOT find ${_NAME} (missing: ${MISSING_VARS})")
    endif()
  endif()
endif()

endif()
