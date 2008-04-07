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

# Generate CMake lines that will define the ITK_SOURCE_DIR in the ITKConfig.cmake.
# We want this to happen only in the ITKConfig.cmake of the build dir, not in the
# installed or relocatable one.
SET(ITK_CONFIG_CODE "
# The ITK source tree.
# For backward compatibility issues we still need to define this variable, although
# it is highly probable that it will cause more harm than being useful. 
# Use ITK_INCLUDE_DIRS instead, since ITK_SOURCE_DIR may point to non-existent directory
IF(NOT ITK_LEGACY_REMOVE)
  SET(ITK_SOURCE_DIR \"${ITK_SOURCE_DIR}\")
ENDIF(NOT ITK_LEGACY_REMOVE)"
)

# The library dependencies file.
SET(ITK_LIBRARY_DEPENDS_FILE  ${ITK_BINARY_DIR}/ITKLibraryDepends.cmake)

# The "use" file.
SET(ITK_USE_FILE ${ITK_BINARY_DIR}/UseITK.cmake)

# The build settings file.
SET(ITK_BUILD_SETTINGS_FILE ${ITK_BINARY_DIR}/ITKBuildSettings.cmake)

# Library directory.
SET(ITK_LIBRARY_DIRS_CONFIG ${ITK_LIBRARY_PATH})

# Determine the include directories needed.
SET(ITK_INCLUDE_DIRS_CONFIG
  ${ITK_INCLUDE_DIRS_BUILD_TREE}
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
SET(ITK_LIBRARY_DEPENDS_FILE "\${ITK_INSTALL_PREFIX}${ITK_INSTALL_PACKAGE_DIR}/ITKLibraryDepends.cmake")

# The "use" file.
SET(ITK_USE_FILE \${ITK_INSTALL_PREFIX}${ITK_INSTALL_PACKAGE_DIR}/UseITK.cmake)

# The build settings file.
SET(ITK_BUILD_SETTINGS_FILE \${ITK_INSTALL_PREFIX}${ITK_INSTALL_PACKAGE_DIR}/ITKBuildSettings.cmake)

# Include directories.
SET(ITK_INCLUDE_DIRS_CONFIG \${ITK_INSTALL_PREFIX}${ITK_INSTALL_INCLUDE_DIR})
FOREACH(DIR ${ITK_INCLUDE_RELATIVE_DIRS})
  LIST(APPEND ITK_INCLUDE_DIRS_CONFIG \${ITK_INSTALL_PREFIX}${ITK_INSTALL_INCLUDE_DIR}/${DIR})
ENDFOREACH(DIR)
IF(ITK_INCLUDE_DIRS_SYSTEM)
  LIST(APPEND ITK_INCLUDE_DIRS_CONFIG ${ITK_INCLUDE_DIRS_SYSTEM})
ENDIF(ITK_INCLUDE_DIRS_SYSTEM)

# Link directories.
SET(ITK_LIBRARY_DIRS_CONFIG "\${ITK_INSTALL_PREFIX}${ITK_INSTALL_LIB_DIR}")

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

# Construct the proper number of GET_FILENAME_COMPONENT(... PATH)
# calls to compute the installation prefix.
STRING(REGEX REPLACE "/" ";" ITK_INSTALL_PACKAGE_DIR_COUNT
  "${ITK_INSTALL_PACKAGE_DIR}")
SET(ITK_CONFIG_CODE "
# Compute the installation prefix from this ITKConfig.cmake file location.
GET_FILENAME_COMPONENT(ITK_INSTALL_PREFIX \"\${CMAKE_CURRENT_LIST_FILE}\" PATH)")
FOREACH(p ${ITK_INSTALL_PACKAGE_DIR_COUNT})
  SET(ITK_CONFIG_CODE
    "${ITK_CONFIG_CODE}\nGET_FILENAME_COMPONENT(ITK_INSTALL_PREFIX \"\${ITK_INSTALL_PREFIX}\" PATH)"
    )
ENDFOREACH(p)


CONFIGURE_FILE(${ITK_SOURCE_DIR}/ITKConfig.cmake.in
               ${ITK_BINARY_DIR}/Utilities/ITKConfig.cmake @ONLY IMMEDIATE)

