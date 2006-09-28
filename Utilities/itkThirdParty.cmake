#-----------------------------------------------------------------------------
MACRO(ITK_THIRD_PARTY_OPTION upper lower)
  OPTION(ITK_USE_SYSTEM_${upper} "Use the system's ${lower} library." OFF)
  MARK_AS_ADVANCED(ITK_USE_SYSTEM_${upper})
  IF(ITK_USE_SYSTEM_${upper})
    IF(EXISTS ${CMAKE_ROOT}/Modules/Find${upper}.cmake)
      INCLUDE(${CMAKE_ROOT}/Modules/Find${upper}.cmake)
    ELSE(EXISTS ${CMAKE_ROOT}/Modules/Find${upper}.cmake)
      INCLUDE(${ITK_SOURCE_DIR}/Utilities/Find${upper}.cmake)
    ENDIF(EXISTS ${CMAKE_ROOT}/Modules/Find${upper}.cmake)
    MARK_AS_ADVANCED(${upper}_INCLUDE_DIR ${upper}_LIBRARY)
    IF(${upper}_FOUND)
      SET(ITK_${upper}_LIBRARIES ${${upper}_LIBRARIES})
      IF("${upper}" MATCHES "^PNG$")
        SET(PNG_INCLUDE_DIR ${PNG_PNG_INCLUDE_DIR})
        MARK_AS_ADVANCED(PNG_PNG_INCLUDE_DIR)
      ENDIF("${upper}" MATCHES "^PNG$")
    ELSE(${upper}_FOUND)
      MESSAGE(SEND_ERROR "ITK_USE_SYSTEM_${upper} is ON, but ${upper}_LIBRARY is NOTFOUND.")
    ENDIF(${upper}_FOUND)
  ELSE(ITK_USE_SYSTEM_${upper})
    SET(ITK_${upper}_LIBRARIES itk${lower})
  ENDIF(ITK_USE_SYSTEM_${upper})
ENDMACRO(ITK_THIRD_PARTY_OPTION)

#-----------------------------------------------------------------------------
# The in-tree third-party libraries are not exported.  We only need
# the include directory inside the tree.  If using a third-party
# library from the system, though, make sure the system include
# directory is consistent inside and outside the tree.
MACRO(ITK_THIRD_PARTY_INCLUDE upper lower)
  IF(ITK_USE_SYSTEM_${upper})
    IF(${upper}_INCLUDE_DIR)
      SET(ITK_INCLUDE_DIRS_SYSTEM ${ITK_INCLUDE_DIRS_SYSTEM} ${${upper}_INCLUDE_DIR})
    ENDIF(${upper}_INCLUDE_DIR)
  ELSE(ITK_USE_SYSTEM_${upper})
    SET(ITK_INCLUDE_DIRS_BUILD_TREE_CXX ${ITK_INCLUDE_DIRS_BUILD_TREE_CXX}
      ${ITK_SOURCE_DIR}/Utilities/${lower}
      ${ITK_BINARY_DIR}/Utilities/${lower}
      )
  ENDIF(ITK_USE_SYSTEM_${upper})
ENDMACRO(ITK_THIRD_PARTY_INCLUDE)

MACRO(ITK_THIRD_PARTY_INCLUDE2 upper)
  IF(ITK_USE_SYSTEM_${upper})
    IF(${upper}_INCLUDE_DIR)
      SET(ITK_INCLUDE_DIRS_SYSTEM ${ITK_INCLUDE_DIRS_SYSTEM} ${${upper}_INCLUDE_DIR})
    ENDIF(${upper}_INCLUDE_DIR)
  ENDIF(ITK_USE_SYSTEM_${upper})
ENDMACRO(ITK_THIRD_PARTY_INCLUDE2)

#-----------------------------------------------------------------------------
MACRO(ITK_THIRD_PARTY_SUBDIR upper lower)
  IF(NOT ITK_USE_SYSTEM_${upper})
    SUBDIRS(${lower})
  ENDIF(NOT ITK_USE_SYSTEM_${upper})
ENDMACRO(ITK_THIRD_PARTY_SUBDIR)
