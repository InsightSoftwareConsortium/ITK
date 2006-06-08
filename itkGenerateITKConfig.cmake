# Generate the ITKConfig.cmake file in the build tree.  Also configure
# one for installation.  The file tells external projects how to use
# ITK.

IF(ITK_USE_SYSTEM_VXL)
  SET(ITK_VXL_DIR ${VXL_DIR})
ELSE(ITK_USE_SYSTEM_VXL)
  SET(ITK_VXL_DIR)
ENDIF(ITK_USE_SYSTEM_VXL)

# System GDCM
IF(ITK_USE_SYSTEM_GDCM)
  SET(ITK_GDCM_DIR ${GDCM_DIR})
ELSE(ITK_USE_SYSTEM_GDCM)
  SET(ITK_GDCM_DIR)
ENDIF(ITK_USE_SYSTEM_GDCM)

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

# The CableSwig configuration directory.
IF(ITK_NEED_CableSwig)
  # Use the same CableSwig we used.
  SET(ITK_CableSwig_DIR_CONFIG ${CableSwig_DIR})
ELSE(ITK_NEED_CableSwig)
  # No wrapping was done.  Do not provide a CableSwig setting.
  SET(ITK_CableSwig_DIR_CONFIG "")
ENDIF(ITK_NEED_CableSwig)

#-----------------------------------------------------------------------------
# Configure ITKConfig.cmake for the build tree.
CONFIGURE_FILE(${ITK_SOURCE_DIR}/ITKConfig.cmake.in
               ${ITK_BINARY_DIR}/ITKConfig.cmake @ONLY IMMEDIATE)

#-----------------------------------------------------------------------------
# Settings specific to the install tree.

# The "use" file.
SET(ITK_USE_FILE ${CMAKE_INSTALL_PREFIX}${ITK_INSTALL_LIB_DIR}/UseITK.cmake)

# The library dependencies file.
SET(ITK_LIBRARY_DEPENDS_FILE
    ${CMAKE_INSTALL_PREFIX}${ITK_INSTALL_LIB_DIR}/ITKLibraryDepends.cmake)

# The build settings file.
SET(ITK_BUILD_SETTINGS_FILE
    ${CMAKE_INSTALL_PREFIX}${ITK_INSTALL_LIB_DIR}/ITKBuildSettings.cmake)

# Include directories.
SET(ITK_INCLUDE_DIRS_CONFIG
  ${ITK_INCLUDE_DIRS_INSTALL_TREE}
  ${ITK_INCLUDE_DIRS_SYSTEM}
)

# Link directories.
SET(ITK_LIBRARY_DIRS_CONFIG ${CMAKE_INSTALL_PREFIX}${ITK_INSTALL_LIB_DIR})

# The CableSwig configuration directory.
IF(ITK_NEED_CableSwig)
  IF(ITK_BUILD_CABLESWIG)
    # We built an internal CableSwig.
    SET(ITK_CableSwig_DIR_CONFIG
      ${CMAKE_INSTALL_PREFIX}${CableSwig_INSTALL_ROOT}/lib/CableSwig)
  ELSE(ITK_BUILD_CABLESWIG)
    # We built with an external CableSwig.
    SET(ITK_CableSwig_DIR_CONFIG ${CableSwig_DIR})
  ENDIF(ITK_BUILD_CABLESWIG)
ELSE(ITK_NEED_CableSwig)
  # No wrapping was done.  Do not provide a CableSwig setting.
  SET(ITK_CableSwig_DIR_CONFIG "")
ENDIF(ITK_NEED_CableSwig)

#-----------------------------------------------------------------------------
# Configure ITKConfig.cmake for the install tree.
CONFIGURE_FILE(${ITK_SOURCE_DIR}/ITKConfig.cmake.in
               ${ITK_BINARY_DIR}/Utilities/ITKConfig.cmake @ONLY IMMEDIATE)
