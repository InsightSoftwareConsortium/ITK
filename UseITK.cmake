#
# This module add the ITK include paths to a project
# It should be included after the FindITK module
#

# We must support both the FindITK.cmake module in CMake 1.4.x, and
# the new configuration form based on ITKConfig.cmake.

IF (ITK_BINARY_PATH)

  # ITK_BINARY_PATH is set, so we are being included from the old
  # CMake 1.4 module.  Load settings from the build tree's config file.
  INCLUDE (${ITK_BINARY_PATH}/ITKConfig.cmake)
  INCLUDE_DIRECTORIES(${ITK_INCLUDE_DIRS})
  LINK_DIRECTORIES(${ITK_LIBRARY_DIRS})
  ADD_DEFINITIONS(${ITK_DEFINITIONS})

ELSE (ITK_BINARY_PATH)

  # We are being included from the ITK_USE_FILE set in ITKConfig.cmake.
  # The settings have already been loaded.  This is the preferred style
  # because it supports using ITK from an installation.
  INCLUDE_DIRECTORIES(${ITK_INCLUDE_DIRS})
  LINK_DIRECTORIES(${ITK_LIBRARY_DIRS})
  ADD_DEFINITIONS(${ITK_DEFINITIONS})

ENDIF (ITK_BINARY_PATH)
