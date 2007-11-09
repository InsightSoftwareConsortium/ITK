# - Find a wxWidgets (a.k.a., wxWindows) installation.
# This module finds if wxWidgets is installed and selects a default
# configuration to use.
#
# The following variables are searched for and set to defaults in case
# of multiple choices. Change them if the defaults are not desired:
#
#  WXWIDGETS_ROOT_DIR      - Base wxWidgets directory
#                            (e.g., C:/wxWidgets-2.6.3).
#  WXWIDGETS_LIB_DIR       - Path to wxWidgets libraries
#                            (e.g., C:/wxWidgets-2.6.3/lib/vc_lib).
#  WXWIDGETS_CONFIGURATION - Configuration to use
#                            (e.g., msw, mswd, mswu, mswunivud, etc.)
#  WXWIDGETS_USE_LIBS      - Libraries to use besides the common
#                            required ones; set to base and core by
#                            default.
#
# The following are set after configuration is done:
#
#  WXWIDGETS_FOUND            - Set to TRUE if wxWidgets was found.
#  WXWIDGETS_INCLUDE_DIR      - Include directories for WIN32 (i.e.,
#                               where to find "wx/wx.h" and
#                               "wx/setup.h"); empty for unices.
#  WXWIDGETS_LIBRARIES        - Path to the wxWidgets libraries.
#  WXWIDGETS_LINK_DIRECTORIES - Link dirs, useful for rpath on UNIX.
#                               Empty string in WIN32 environment.
#  WXWIDGETS_CXX_FLAGS        - Include dirs and ompiler flags for
#                               unices, empty on WIN32. Esentially
#                               "`wx-config --cxxflags`".
#
# Sample usage:
#
#   SET(WXWIDGETS_USE_LIBS base core gl net)
#   FIND_PACKAGE(wxWidgets)
#   IF(WXWIDGETS_FOUND)
#     MESSAGE(STATUS "Found wxWidgets!")
#     IF(WIN32)
#       INCLUDE_DIRECTORIES(${WXWIDGETS_INCLUDE_DIR})
#     ELSE(WIN32)
#       SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${WXWIDGETS_CXX_FLAGS}")
#       LINK_DIRECTORIES(${WXWIDGETS_LINK_DIRECTORIES})
#     ENDIF(WIN32)
#     TARGET_LINK_LIBRARIES(target ${WXWIDGETS_LIBRARIES})
#   ENDIF(WXWIDGETS_FOUND)
#
# NOTES
#
#   This module has been tested on the WIN32 platform with wxWidgets
#   2.6.2, 2.6.3, and 2.5.3. However, it has been designed to be easily
#   extended to support all possible builds (e.g., static/shared,
#   debug/release, unicode, universal, multilib/monolithic, etc.).
#
#   If you want to use the module and your build type is not supported
#   out-of-the-box, please contact me to exchange information on how
#   your system is setup and I'll try to add support for it.
#
# AUTHOR
#   Miguel A. Figueroa-Villanueva (miguelf at ieee dot org).
#
#   Based on previous works of: Jan Woetzel (FindwxWindows.cmake),
#   and Jorgen Bodde (FindwxWin.cmake).

#
# Helper macro to control the debugging output globally.
# - NOTE: This and all the DBG_MSG calls should be removed after the
#         module stabilizes.
#
MACRO(DBG_MSG _MSG)
#  MESSAGE(STATUS ${_MSG})
ENDMACRO(DBG_MSG)

#
# Clear return values in case the module is loaded more than once.
#
SET(WXWIDGETS_FOUND FALSE)
#
SET(WXWIDGETS_INCLUDE_DIR      "")
SET(WXWIDGETS_LIBRARIES        "")
SET(WXWIDGETS_LINK_DIRECTORIES "")
SET(WXWIDGETS_CXX_FLAGS        "")

#=====================================================================
#=====================================================================
IF(WIN32)

#---------------------------------------------------------------------
# WIN32: Helper MACROS
#---------------------------------------------------------------------
#
# Get filename components for a configuration. For example,
#   if _CONFIGURATION = mswunivud, then _UNV=univ, _UCD=u _DBG=d
#   if _CONFIGURATION = mswu,      then _UNV="",   _UCD=u _DBG=""
#
MACRO(WX_GET_NAME_COMPONENTS _CONFIGURATION _UNV _UCD _DBG)
 STRING(REGEX MATCH "univ" ${_UNV} "${_CONFIGURATION}")
 STRING(REGEX REPLACE "msw.*(u)[d]*$" "u" ${_UCD} "${_CONFIGURATION}")
 IF(${_UCD} STREQUAL ${_CONFIGURATION})
   SET(${_UCD} "")
 ENDIF(${_UCD} STREQUAL ${_CONFIGURATION})
 STRING(REGEX MATCH "d$" ${_DBG} "${_CONFIGURATION}")
ENDMACRO(WX_GET_NAME_COMPONENTS)

#
# Find libraries associated to a configuration.
#
MACRO(WX_FIND_LIBS _UNV _UCD _DBG)
 DBG_MSG("m_unv = ${_UNV}")
 DBG_MSG("m_ucd = ${_UCD}")
 DBG_MSG("m_dbg = ${_DBG}")

 # Find wxWidgets common libraries
 FOREACH(LIB png tiff jpeg zlib regex expat)
   FIND_LIBRARY(WX_${LIB}${_DBG}
     NAMES
       wx${LIB}${_UCD}${_DBG} # for regex
       wx${LIB}${_DBG}
     PATHS ${WX_LIB_DIR}
     NO_DEFAULT_PATH
   )
   MARK_AS_ADVANCED(WX_${LIB}${_DBG})
 ENDFOREACH(LIB)

 # Find wxWidgets multilib base libraries
 FIND_LIBRARY(WX_base${_DBG}
   NAMES
     wxbase26${_UCD}${_DBG}
     wxbase25${_UCD}${_DBG}
   PATHS ${WX_LIB_DIR}
   NO_DEFAULT_PATH
 )
 MARK_AS_ADVANCED(WX_base${_DBG})
 FOREACH(LIB net odbc xml)
   FIND_LIBRARY(WX_${LIB}${_DBG}
     NAMES
       wxbase26${_UCD}${_DBG}_${LIB}
       wxbase25${_UCD}${_DBG}_${LIB}
     PATHS ${WX_LIB_DIR}
     NO_DEFAULT_PATH
   )
   MARK_AS_ADVANCED(WX_${LIB}${_DBG})
 ENDFOREACH(LIB)

 # Find wxWidgets monolithic library
 FIND_LIBRARY(WX_mono${_DBG}
   NAMES
     wxmsw${_UNV}26${_UCD}${_DBG}
     wxmsw${_UNV}25${_UCD}${_DBG}
   PATHS ${WX_LIB_DIR}
   NO_DEFAULT_PATH
 )
 MARK_AS_ADVANCED(WX_mono${_DBG})

 # Find wxWidgets multilib libraries
 FOREACH(LIB core adv html media xrc dbgrid gl qa)
   FIND_LIBRARY(WX_${LIB}${_DBG}
     NAMES
       wxmsw${_UNV}26${_UCD}${_DBG}_${LIB}
       wxmsw${_UNV}25${_UCD}${_DBG}_${LIB}
     PATHS ${WX_LIB_DIR}
     NO_DEFAULT_PATH
   )
   MARK_AS_ADVANCED(WX_${LIB}${_DBG})
 ENDFOREACH(LIB)
ENDMACRO(WX_FIND_LIBS)

#
# Clear all library paths, so that FIND_LIBRARY refinds them.
#
# Clear a lib, reset its found flag, and mark as advanced.
MACRO(WX_CLEAR_LIB _LIB)
   SET(${_LIB} "${_LIB}-NOTFOUND" CACHE FILEPATH "Cleared." FORCE)
   SET(${_LIB}_FOUND FALSE)
   MARK_AS_ADVANCED(${_LIB})
ENDMACRO(WX_CLEAR_LIB)
# Clear all debug or release library paths (arguments are "d" or "").
MACRO(WX_CLEAR_ALL_LIBS _DBG)
 # Clear wxWidgets common libraries
 FOREACH(LIB png tiff jpeg zlib regex expat)
   WX_CLEAR_LIB(WX_${LIB}${_DBG})
 ENDFOREACH(LIB)

 # Clear wxWidgets multilib base libraries
 WX_CLEAR_LIB(WX_base${_DBG})
 FOREACH(LIB net odbc xml)
   WX_CLEAR_LIB(WX_${LIB}${_DBG})
 ENDFOREACH(LIB)

 # Clear wxWidgets monolithic library
 WX_CLEAR_LIB(WX_mono${_DBG})

 # Clear wxWidgets multilib libraries
 FOREACH(LIB core adv html media xrc dbgrid gl qa)
   WX_CLEAR_LIB(WX_${LIB}${_DBG})
 ENDFOREACH(LIB)
ENDMACRO(WX_CLEAR_ALL_LIBS)
# Clear all wxWidgets debug libraries.
MACRO(WX_CLEAR_ALL_DBG_LIBS)
  WX_CLEAR_ALL_LIBS("d")
ENDMACRO(WX_CLEAR_ALL_DBG_LIBS)
# Clear all wxWidgets release libraries.
MACRO(WX_CLEAR_ALL_REL_LIBS)
  WX_CLEAR_ALL_LIBS("")
ENDMACRO(WX_CLEAR_ALL_REL_LIBS)

#
# Set the WXWIDGETS_LIBRARIES variable.
# Also, Sets output variable WXWIDGETS_FOUND to FALSE if it fails.
#
MACRO(WX_SET_LIBRARIES _LIBS _DBG)
 IF(WX_USE_REL_AND_DBG)
   FOREACH(LIB ${${_LIBS}})
     DBG_MSG("Finding ${LIB} and ${LIB}d")
     DBG_MSG("WX_${LIB}  : ${WX_${LIB}}")
     DBG_MSG("WX_${LIB}d : ${WX_${LIB}d}")
     IF(WX_${LIB} AND WX_${LIB}d)
       DBG_MSG("Found ${LIB} and ${LIB}d")
       SET(WXWIDGETS_LIBRARIES ${WXWIDGETS_LIBRARIES}
           debug     ${WX_${LIB}d}
           optimized ${WX_${LIB}}
       )
     ELSE(WX_${LIB} AND WX_${LIB}d)
       SET(WXWIDGETS_FOUND FALSE)
     ENDIF(WX_${LIB} AND WX_${LIB}d)
   ENDFOREACH(LIB)
 ELSE(WX_USE_REL_AND_DBG)
   FOREACH(LIB ${${_LIBS}})
     DBG_MSG("Finding ${LIB}${_DBG}")
     DBG_MSG("WX_${LIB}${_DBG} : ${WX_${LIB}${_DBG}}")
     IF(WX_${LIB}${_DBG})
       DBG_MSG("Found ${LIB}${_DBG}")
       SET(WXWIDGETS_LIBRARIES ${WXWIDGETS_LIBRARIES}
           ${WX_${LIB}${_DBG}}
       )
     ELSE(WX_${LIB}${_DBG})
       SET(WXWIDGETS_FOUND FALSE)
     ENDIF(WX_${LIB}${_DBG})
   ENDFOREACH(LIB)
 ENDIF(WX_USE_REL_AND_DBG)

 FOREACH(LIB ${${_LIBS}})
   DBG_MSG("required: ${LIB}")
   IF(LIB STREQUAL "gl")
     DBG_MSG("gl required: ${LIB}")
     SET(WXWIDGETS_LIBRARIES ${WXWIDGETS_LIBRARIES}
       opengl32
       glu32
     )
   ENDIF(LIB STREQUAL "gl")
 ENDFOREACH(LIB ${${_LIBS}})

 SET(WXWIDGETS_LIBRARIES ${WXWIDGETS_LIBRARIES}
   winmm
   comctl32
   rpcrt4
   wsock32
 )
ENDMACRO(WX_SET_LIBRARIES)

#---------------------------------------------------------------------
# WIN32: Start actual work.
#---------------------------------------------------------------------
#
# Look for an installation tree.
#
FIND_PATH(WXWIDGETS_ROOT_DIR include/wx/wx.h
   $ENV{WXWIN}
   "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\wxWidgets_is1;Inno Setup: App Path]"  ## WX 2.6.x
#   "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\wxWindows_is1;Inno Setup: App Path]"  ## WX 2.4.x
   C:\\wxWidgets-2.6.3
   D:\\wxWidgets-2.6.3
   C:\\wxWidgets-2.6.2
   D:\\wxWidgets-2.6.2
   C:\\wxWidgets-2.6.1
   D:\\wxWidgets-2.6.1
   C:\\wxWidgets-2.6.0
   D:\\wxWidgets-2.6.0
   C:\\wxWidgets-2.5.5
   D:\\wxWidgets-2.5.5
   C:\\wxWidgets-2.5.4
   D:\\wxWidgets-2.5.4
   C:\\wxWidgets-2.5.3
   D:\\wxWidgets-2.5.3
   C:\\wxWidgets-2.5.2
   D:\\wxWidgets-2.5.2
   C:\\wxWidgets-2.5.1
   D:\\wxWidgets-2.5.1
#   C:\\wxWindows-2.4.2
#   D:\\wxWindows-2.4.2
  DOC "wxWidgets base/installation directory?"
)
DBG_MSG("WXWIDGETS_ROOT_DIR: ${WXWIDGETS_ROOT_DIR}")

# If WXWIDGETS_ROOT_DIR changed, clear all libraries and lib dir.
IF(NOT WX_ROOT_DIR STREQUAL WXWIDGETS_ROOT_DIR)
  SET(WX_ROOT_DIR ${WXWIDGETS_ROOT_DIR} CACHE INTERNAL "WXWIDGETS_ROOT_DIR")
#  WX_CLEAR_ALL_DBG_LIBS()
#  WX_CLEAR_ALL_REL_LIBS()
  SET(WXWIDGETS_LIB_DIR "WXWIDGETS_LIB_DIR-NOTFOUND" CACHE PATH "Cleared." FORCE)
ENDIF(NOT WX_ROOT_DIR STREQUAL WXWIDGETS_ROOT_DIR)
DBG_MSG("WX_ROOT_DIR: ${WX_ROOT_DIR}")

IF(WX_ROOT_DIR)

# *** Temporary hack for cmake 2.2-patch3 and older.
#     FIND_PATH(<VAR> NAMES ... PATHS ...) not available
# ***
#FIND_PATH(WXWIDGETS_LIB_DIR
#  NAMES wxpng.lib wxpngd.lib
#  PATHS
#  ${WX_ROOT_DIR}/lib/vc_lib
#  ${WX_ROOT_DIR}/lib/vc_dll
#  NO_DEFAULT_PATH
#)
IF(NOT WXWIDGETS_LIB_DIR AND EXISTS ${WX_ROOT_DIR}/lib/vc_lib)
  SET(WXWIDGETS_LIB_DIR ${WX_ROOT_DIR}/lib/vc_lib CACHE PATH "Path to wxWidgets libraries." FORCE)
ENDIF(NOT WXWIDGETS_LIB_DIR AND EXISTS ${WX_ROOT_DIR}/lib/vc_lib)
IF(NOT WXWIDGETS_LIB_DIR AND EXISTS ${WX_ROOT_DIR}/lib/vc_dll)
  SET(WXWIDGETS_LIB_DIR ${WX_ROOT_DIR}/lib/vc_dll CACHE PATH "Path to wxWidgets libraries." FORCE)
ENDIF(NOT WXWIDGETS_LIB_DIR AND EXISTS ${WX_ROOT_DIR}/lib/vc_dll)
DBG_MSG("WXWIDGETS_LIB_DIR: ${WXWIDGETS_LIB_DIR}")

# If WXWIDGETS_LIB_DIR changed, clear all libraries.
IF(NOT WX_LIB_DIR STREQUAL WXWIDGETS_LIB_DIR)
  SET(WX_LIB_DIR ${WXWIDGETS_LIB_DIR} CACHE INTERNAL "WXWIDGETS_LIB_DIR")
  WX_CLEAR_ALL_DBG_LIBS()
  WX_CLEAR_ALL_REL_LIBS()
ENDIF(NOT WX_LIB_DIR STREQUAL WXWIDGETS_LIB_DIR)
DBG_MSG("WX_LIB_DIR: ${WX_LIB_DIR}")

IF(WX_LIB_DIR)
  SET(WXWIDGETS_FOUND TRUE)

#---------------------------------------------------------------------
# WIN32: ???
#---------------------------------------------------------------------
# Search for possible configuration type availabilities
# ***** SET(WX_LAST_CFG "")
FOREACH(CFG mswunivud mswunivd mswud mswd mswunivu mswuniv mswu msw)
  SET(WX_${CFG}_FOUND FALSE)
  IF(EXISTS ${WX_LIB_DIR}/${CFG})
    SET(WX_CONFIGURATION_LIST ${WX_CONFIGURATION_LIST} ${CFG})
    SET(WX_${CFG}_FOUND TRUE)
    SET(WX_CONFIGURATION ${CFG})
  ENDIF(EXISTS ${WX_LIB_DIR}/${CFG})
ENDFOREACH(CFG)

# ***** SET(WX_USE_REL_AND_DBG FALSE)
IF(WX_CONFIGURATION)
  # if selected configuration wasn't found, force the default one
  # else, use it but still force a refresh for the list in doc string
  IF(NOT WX_${WXWIDGETS_CONFIGURATION}_FOUND)
    SET(WXWIDGETS_CONFIGURATION ${WX_CONFIGURATION} CACHE STRING
        "Set wxWidgets configuration (${WX_CONFIGURATION_LIST})" FORCE)
  ELSE(NOT WX_${WXWIDGETS_CONFIGURATION}_FOUND)
    SET(WXWIDGETS_CONFIGURATION ${WXWIDGETS_CONFIGURATION} CACHE STRING
        "Set wxWidgets configuration (${WX_CONFIGURATION_LIST})" FORCE)
  ENDIF(NOT WX_${WXWIDGETS_CONFIGURATION}_FOUND)

  # if release config was selected, and both release/debug exist
  IF(WX_${WXWIDGETS_CONFIGURATION}d_FOUND)
    OPTION(WXWIDGETS_USE_REL_AND_DBG
           "Use release and debug configurations?" TRUE)
    SET(WX_USE_REL_AND_DBG ${WXWIDGETS_USE_REL_AND_DBG})
  ELSE(WX_${WXWIDGETS_CONFIGURATION}d_FOUND)
    # if the option exists, force it to false
    IF(WXWIDGETS_USE_REL_AND_DBG)
      SET(WXWIDGETS_USE_REL_AND_DBG FALSE CACHE BOOL
          "No ${WXWIDGETS_CONFIGURATION}d found." FORCE)
    ENDIF(WXWIDGETS_USE_REL_AND_DBG)
    SET(WX_USE_REL_AND_DBG FALSE)
  ENDIF(WX_${WXWIDGETS_CONFIGURATION}d_FOUND)

  # Get configuration parameters from the name.
  WX_GET_NAME_COMPONENTS(${WXWIDGETS_CONFIGURATION} UNV UCD DBG)

  # Set wxWidgets main include directory.
  IF(EXISTS ${WX_ROOT_DIR}/include/wx/wx.h)
    SET(WXWIDGETS_INCLUDE_DIR ${WX_ROOT_DIR}/include)
  ELSE(EXISTS ${WX_ROOT_DIR}/include/wx/wx.h)
    SET(WXWIDGETS_FOUND FALSE)
  ENDIF(EXISTS ${WX_ROOT_DIR}/include/wx/wx.h)

  # Set wxWidgets lib setup include directory.
  IF(EXISTS ${WX_LIB_DIR}/${WXWIDGETS_CONFIGURATION}/wx/setup.h)
    SET(WXWIDGETS_INCLUDE_DIR ${WXWIDGETS_INCLUDE_DIR}
        ${WX_LIB_DIR}/${WXWIDGETS_CONFIGURATION})
  ELSE(EXISTS ${WX_LIB_DIR}/${WXWIDGETS_CONFIGURATION}/wx/setup.h)
    SET(WXWIDGETS_FOUND FALSE)
  ENDIF(EXISTS ${WX_LIB_DIR}/${WXWIDGETS_CONFIGURATION}/wx/setup.h)
  #FIND_PATH(WX_SETUP_INCLUDE_DIR wx/setup.h
  #          ${WX_LIB_DIR}/${WXWIDGETS_CONFIGURATION})
  #MARK_AS_ADVANCED(WX_SETUP_INCLUDE_DIR)

  # Find wxWidgets libraries.
  WX_FIND_LIBS("${UNV}" "${UCD}" "${DBG}")
  IF(WX_USE_REL_AND_DBG)
    WX_FIND_LIBS("${UNV}" "${UCD}" "d")
  ENDIF(WX_USE_REL_AND_DBG)

  # Libraries we are interested in.
  IF(WXWIDGETS_USE_LIBS)
    # Add the common required libs.
    SET(WXWIDGETS_USE_LIBS ${WXWIDGETS_USE_LIBS}
        png tiff jpeg zlib regex expat
    )
  ELSE(WXWIDGETS_USE_LIBS)
    # Default minimal use setting (i.e., link to only core and base).
    SET(WXWIDGETS_USE_LIBS base core
        png tiff jpeg zlib regex expat
    )
  ENDIF(WXWIDGETS_USE_LIBS)

  # Settings for requested libs (i.e., include dir, libraries, etc.).
  WX_SET_LIBRARIES(WXWIDGETS_USE_LIBS "${DBG}")

ENDIF(WX_CONFIGURATION)
ENDIF(WX_LIB_DIR)
ENDIF(WX_ROOT_DIR)


#=====================================================================
#=====================================================================
ELSE(WIN32)
  FIND_PROGRAM(WXWIDGETS_CONFIG_EXE wx-config)
  IF(WXWIDGETS_CONFIG_EXE)
    SET(WXWIDGETS_FOUND TRUE)

    # run the wx-config program to get cxxflags
    EXEC_PROGRAM(${WXWIDGETS_CONFIG_EXE}
                 ARGS "--cxxflags"
                 OUTPUT_VARIABLE WXWIDGETS_CXX_FLAGS
                 RETURN_VALUE RET)
    IF(NOT RET EQUAL 0)
      SET(WXWIDGETS_FOUND FALSE)
    ENDIF(NOT RET EQUAL 0)
    
    # run the wx-config program to get the libs
    # - NOTE: wx-config doesn't verify that the libs requested exist
    #         it just produces the names. Maybe a TRY_COMPILE would
    #         be useful here...
    #STRING(REPLACE ";" "," WXWIDGETS_USE_LIBS "${WXWIDGETS_USE_LIBS}")
    STRING(REGEX REPLACE ";" "," WXWIDGETS_USE_LIBS "${WXWIDGETS_USE_LIBS}")
    DBG_MSG(${WXWIDGETS_USE_LIBS})
    EXEC_PROGRAM(${WXWIDGETS_CONFIG_EXE}
                 ARGS "--libs ${WXWIDGETS_USE_LIBS}"
                 OUTPUT_VARIABLE WXWIDGETS_LIBRARIES
                 RETURN_VALUE RET)
    IF(RET EQUAL 0)
      SEPARATE_ARGUMENTS(WXWIDGETS_LIBRARIES)
      STRING(REGEX REPLACE "-framework;" "-framework "
             WXWIDGETS_LIBRARIES
             "${WXWIDGETS_LIBRARIES}")

      # extract linkdirs (-L) for rpath (i.e., LINK_DIRECTORIES)
      STRING(REGEX MATCHALL "[-][L][^ ;]+"
             WXWIDGETS_LINK_DIRECTORIES
             "${WXWIDGETS_LIBRARIES}")
      STRING(REGEX REPLACE "[-][L]" ""
             WXWIDGETS_LINK_DIRECTORIES
             "${WXWIDGETS_LINK_DIRECTORIES}")
    ELSE(RET EQUAL 0)
      SET(WXWIDGETS_FOUND FALSE)
    ENDIF(RET EQUAL 0)
  ENDIF(WXWIDGETS_CONFIG_EXE)
ENDIF(WIN32)

DBG_MSG("WXWIDGETS_FOUND      : ${WXWIDGETS_FOUND}"      )
DBG_MSG("WXWIDGETS_INCLUDE_DIR: ${WXWIDGETS_INCLUDE_DIR}")
DBG_MSG("WXWIDGETS_LIBRARIES  : ${WXWIDGETS_LIBRARIES}"  )
DBG_MSG("WXWIDGETS_LINK_DIRECTORIES: ${WXWIDGETS_LINK_DIRECTORIES}")
DBG_MSG("WXWIDGETS_CXX_FLAGS       : ${WXWIDGETS_CXX_FLAGS}"       )

#=====================================================================
#=====================================================================
IF(NOT WXWIDGETS_FOUND)
  # make FIND_PACKAGE friendly
  IF(NOT wxWidgets_FIND_QUIETLY)
    IF(wxWidgets_FIND_REQUIRED)
      MESSAGE(FATAL_ERROR
              "wxWidgets required, please specify it's location.")
    ELSE(wxWidgets_FIND_REQUIRED)
      MESSAGE(STATUS "Warning: wxWidgets was not found.")
    ENDIF(wxWidgets_FIND_REQUIRED)
  ENDIF(NOT wxWidgets_FIND_QUIETLY)
ENDIF(NOT WXWIDGETS_FOUND)
