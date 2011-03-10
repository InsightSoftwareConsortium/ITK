# - Test for DirectShow on Windows.
# Once loaded this will define
#   DIRECTSHOW_FOUND        - system has DirectShow
#   DIRECTSHOW_INCLUDE_DIRS - include directory for DirectShow
#   DIRECTSHOW_LIBRARIES    - libraries you need to link to

SET(DIRECTSHOW_FOUND "NO")

# DirectShow is only available on Windows platforms
IF(MSVC)
  # Find DirectX Include Directory (dshow depends on it)
  FIND_PATH(DIRECTX_INCLUDE_DIR ddraw.h
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
  IF(DIRECTX_INCLUDE_DIR)
    FIND_PATH(DIRECTSHOW_INCLUDE_DIR dshow.h
      "${DIRECTX_INCLUDE_DIR}"
      "C:/Program Files/Microsoft Platform SDK for Windows Server 2003 R2/Include"
      "C:/Program Files/Microsoft Platform SDK/Include"
      DOC "What is the path where the file dshow.h can be found"
      NO_DEFAULT_PATH
    )

    # if DirectShow include dir found, then find DirectShow libraries
    IF(DIRECTSHOW_INCLUDE_DIR)
      IF(CMAKE_CL_64)
        FIND_LIBRARY(DIRECTSHOW_STRMIIDS_LIBRARY strmiids
          "${DIRECTSHOW_INCLUDE_DIR}/../Lib/x64"
          DOC "Where can the DirectShow strmiids library be found"
          NO_DEFAULT_PATH
          )
        FIND_LIBRARY(DIRECTSHOW_QUARTZ_LIBRARY quartz
          "${DIRECTSHOW_INCLUDE_DIR}/../Lib/x64"
          DOC "Where can the DirectShow quartz library be found"
          NO_DEFAULT_PATH
          )
      ELSE(CMAKE_CL_64)
        FIND_LIBRARY(DIRECTSHOW_STRMIIDS_LIBRARY strmiids
          "${DIRECTSHOW_INCLUDE_DIR}/../Lib"
          "${DIRECTSHOW_INCLUDE_DIR}/../Lib/x86"
          DOC "Where can the DirectShow strmiids library be found"
          NO_DEFAULT_PATH
          )
        FIND_LIBRARY(DIRECTSHOW_QUARTZ_LIBRARY quartz
          "${DIRECTSHOW_INCLUDE_DIR}/../Lib"
          "${DIRECTSHOW_INCLUDE_DIR}/../Lib/x86"
          DOC "Where can the DirectShow quartz library be found"
          NO_DEFAULT_PATH
          )
      ENDIF(CMAKE_CL_64)
    ENDIF(DIRECTSHOW_INCLUDE_DIR)
  ENDIF(DIRECTX_INCLUDE_DIR)
ENDIF(MSVC)

#---------------------------------------------------------------------
SET(DIRECTSHOW_INCLUDE_DIRS
  "${DIRECTX_INCLUDE_DIR}"
  "${DIRECTSHOW_INCLUDE_DIR}"
  )

SET(DIRECTSHOW_LIBRARIES
  "${DIRECTSHOW_STRMIIDS_LIBRARY}"
  "${DIRECTSHOW_QUARTZ_LIBRARY}"
  )

#---------------------------------------------------------------------
INCLUDE (CheckCXXSourceCompiles)

SET(CMAKE_REQUIRED_INCLUDES  ${DIRECTSHOW_INCLUDE_DIRS})
SET(CMAKE_REQUIRED_LIBRARIES ${DIRECTSHOW_LIBRARIES})
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
SET(CMAKE_REQUIRED_INCLUDES)
SET(CMAKE_REQUIRED_LIBRARIES)

#---------------------------------------------------------------------
# FIXME: When cmake_minimum_version reaches 2.6.0 the
#        FindPackageHandleStandardArgs module can be used.
IF(CMAKE_MINIMUM_REQUIRED_VERSION GREATER 2.5)
  MESSAGE(FATAL_ERROR
    "Uncomment code below: FindPackageHandleStandardArgs is now available.")
ENDIF(CMAKE_MINIMUM_REQUIRED_VERSION GREATER 2.5)

#INCLUDE(FindPackageHandleStandardArgs)
#FIND_PACKAGE_HANDLE_STANDARD_ARGS(
#  DIRECTSHOW
#  DEFAULT_MSG
#  DIRECTX_INCLUDE_DIR
#  DIRECTSHOW_INCLUDE_DIR
#  DIRECTSHOW_STRMIIDS_LIBRARY
#  DIRECTSHOW_QUARTZ_LIBRARY
#  DIRECTSHOW_SOURCE_COMPILES
#  )

SET(_NAME DIRECTSHOW)
SET(_NAME_UPPER DIRECTSHOW)
SET(MISSING_VARS "")
# check if all passed variables are valid
SET(${_NAME_UPPER}_FOUND TRUE)
FOREACH(_CURRENT_VAR
    DIRECTX_INCLUDE_DIR
    DIRECTSHOW_INCLUDE_DIR
    DIRECTSHOW_STRMIIDS_LIBRARY
    DIRECTSHOW_QUARTZ_LIBRARY
    DIRECTSHOW_SOURCE_COMPILES
    )
  IF(NOT "${_CURRENT_VAR}")
    SET(${_NAME_UPPER}_FOUND FALSE)
    SET(MISSING_VARS "${MISSING_VARS} ${_CURRENT_VAR}")
  ENDIF(NOT "${_CURRENT_VAR}")
ENDFOREACH(_CURRENT_VAR)

IF (${_NAME_UPPER}_FOUND)
  IF(NOT ${_NAME}_FIND_QUIETLY)
    MESSAGE(STATUS "Found ${_NAME}")
  ENDIF(NOT ${_NAME}_FIND_QUIETLY)
ELSE (${_NAME_UPPER}_FOUND)
  IF (${_NAME}_FIND_REQUIRED)
    MESSAGE(FATAL_ERROR "Could NOT find ${_NAME} (missing: ${MISSING_VARS})")
  ELSE (${_NAME}_FIND_REQUIRED)
    IF (NOT ${_NAME}_FIND_QUIETLY)
      MESSAGE(STATUS "Could NOT find ${_NAME} (missing: ${MISSING_VARS})")
    ENDIF (NOT ${_NAME}_FIND_QUIETLY)
  ENDIF (${_NAME}_FIND_REQUIRED)
ENDIF (${_NAME_UPPER}_FOUND)
