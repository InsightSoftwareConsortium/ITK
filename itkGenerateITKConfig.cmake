# Generate the ITKConfig.cmake file in the build tree.  Also configure
# one for installation.  The file tells external projects how to use
# ITK.

#-----------------------------------------------------------------------------
# Settings specific to the build tree.

# The "use" file.
SET(ITK_USE_FILE ${ITK_BINARY_DIR}/UseITK.cmake)

# The library dependencies file.
SET(ITK_LIBRARY_DEPENDS_FILE ${ITK_BINARY_DIR}/ITKLibraryDepends.cmake)

# The build settings file.
SET(ITK_BUILD_SETTINGS_FILE ${ITK_BINARY_DIR}/ITKBuildSettings.cmake)

# Library directory.
SET(ITK_LIBRARY_DIRS_CONFIG ${ITK_LIBRARY_PATH})

# Determine the include directories needed.
SET(ITK_INCLUDE_DIRS_CONFIG
  ${ITK_INCLUDE_DIRS_BUILD_TREE}
  ${ITK_INCLUDE_DIRS_SOURCE_TREE}
  ${ITK_INCLUDE_DIRS_SYSTEM}
)

#-----------------------------------------------------------------------------
# Configure ITKConfig.cmake for the build tree.
CONFIGURE_FILE(${ITK_SOURCE_DIR}/ITKConfig.cmake.in
               ${ITK_BINARY_DIR}/ITKConfig.cmake @ONLY IMMEDIATE)

#-----------------------------------------------------------------------------
# Settings specific to the install tree.

# The "use" file.
SET(ITK_USE_FILE ${CMAKE_INSTALL_PREFIX}/lib/InsightToolkit/UseITK.cmake)

# The library dependencies file.
SET(ITK_LIBRARY_DEPENDS_FILE
    ${CMAKE_INSTALL_PREFIX}/lib/InsightToolkit/ITKLibraryDepends.cmake)

# The build settings file.
SET(ITK_BUILD_SETTINGS_FILE
    ${CMAKE_INSTALL_PREFIX}/lib/InsightToolkit/ITKBuildSettings.cmake)

# Include directories.
SET(ITK_INCLUDE_DIRS_CONFIG
  ${ITK_INCLUDE_DIRS_INSTALL_TREE}
  ${ITK_INCLUDE_DIRS_SYSTEM}
)

# Link directories.
SET(ITK_LIBRARY_DIRS_CONFIG ${CMAKE_INSTALL_PREFIX}/lib/InsightToolkit)

#-----------------------------------------------------------------------------
# Configure ITKConfig.cmake for the install tree.
CONFIGURE_FILE(${ITK_SOURCE_DIR}/ITKConfig.cmake.in
               ${ITK_BINARY_DIR}/Utilities/ITKConfig.cmake @ONLY IMMEDIATE)
