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
# Settings common to the build and installation tree.

# The "use" file.
SET(ITK_USE_FILE                  UseITK.cmake)

# The build settings file.
SET(ITK_BUILD_SETTINGS_FILE       ITKBuildSettings.cmake)


#-----------------------------------------------------------------------------
# Settings specific to the build tree.

# The library dependencies file.
SET(ITK_LIBRARY_DEPENDS_FILE       ITKLibraryDepends.cmake)

# Library directory.
SET(ITK_LIBRARY_DIRS_CONFIG ${ITK_BUILD_LIB_DIR})

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

# The library dependencies file.
SET(ITK_LIBRARY_DEPENDS_FILE      ITKLibraryDepends.cmake)

# Include directories.
SET(ITK_INCLUDE_DIRS_CONFIG
  ${ITK_INCLUDE_DIRS_INSTALL_TREE}
  ${ITK_INCLUDE_DIRS_SYSTEM}
)

# Link directories.
# The install tree will use the directory where ITKConfig.cmake is found, which
# happens to be "INSTALLATION/lib/InsightToolkit". That is, it is already the
# same directory where the libraries are installed. Therefore this variable
# must be empty here. See ITKConfig.cmake.in for details on how this variable
# is used.
SET(ITK_LIBRARY_DIRS_CONFIG "")  

# The CableSwig configuration directory.
IF(ITK_NEED_CableSwig)
  IF(ITK_BUILD_CABLESWIG)
    # We built an internal CableSwig.
    SET(ITK_CableSwig_DIR_CONFIG
      ${CableSwig_INSTALL_ROOT}/lib/CableSwig)
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
