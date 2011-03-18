#
# This module is provided as GDCM_USE_FILE by GDCMConfig.cmake.
# It can be INCLUDEd in a project to load the needed compiler and linker
# settings to use GDCM:
#   FIND_PACKAGE(GDCM REQUIRED)
#   INCLUDE(${GDCM_USE_FILE})

IF(NOT GDCM_USE_FILE_INCLUDED)
  SET(GDCM_USE_FILE_INCLUDED 1)

  # Add include directories needed to use GDCM.
  INCLUDE_DIRECTORIES(${GDCM_INCLUDE_DIRS})

  # Add link directories needed to use GDCM.
  LINK_DIRECTORIES(${GDCM_LIBRARY_DIRS})

  # Add cmake module path.
  SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${GDCM_CMAKE_DIR}")

  # Use VTK.
  IF(GDCM_USE_VTK)
    SET(VTK_DIR ${GDCM_VTK_DIR})
    FIND_PACKAGE(VTK)
    IF(VTK_FOUND)
      INCLUDE(${VTK_USE_FILE})
    ELSE(VTK_FOUND)
      MESSAGE("VTK not found in GDCM_VTK_DIR=\"${GDCM_VTK_DIR}\".")
    ENDIF(VTK_FOUND)
  ENDIF(GDCM_USE_VTK)

ENDIF(NOT GDCM_USE_FILE_INCLUDED)
