# Generate the ITKConfig.cmake file in the build tree.  Also configure
# one for installation.  The file tells external projects how to use
# ITK.

if(ITK_USE_SYSTEM_VXL)
  set(ITK_VXL_DIR ${VXL_DIR})
else(ITK_USE_SYSTEM_VXL)
  set(ITK_VXL_DIR)
endif(ITK_USE_SYSTEM_VXL)

# System GDCM
if(ITK_USE_SYSTEM_GDCM)
  set(ITK_GDCM_DIR ${GDCM_DIR})
else(ITK_USE_SYSTEM_GDCM)
  set(ITK_GDCM_DIR)
endif(ITK_USE_SYSTEM_GDCM)



#-----------------------------------------------------------------------------
# Settings specific to the build tree.

# Generate CMake lines that will define the ITK_SOURCE_DIR in the ITKConfig.cmake.
# We want this to happen only in the ITKConfig.cmake of the build dir, not in the
# installed or relocatable one.
set(ITK_CONFIG_CODE "")

# The library dependencies file.
set(ITK_LIBRARY_DEPENDS_FILE  ${ITK_BINARY_DIR}/ITKLibraryDepends.cmake)

# The "use" file.
set(ITK_USE_FILE ${ITK_BINARY_DIR}/UseITK.cmake)

# The build settings file.
set(ITK_BUILD_SETTINGS_FILE ${ITK_BINARY_DIR}/ITKBuildSettings.cmake)

# Library directory.
set(ITK_LIBRARY_DIRS_CONFIG ${ITK_LIBRARY_PATH})

# Determine the include directories needed.
set(ITK_INCLUDE_DIRS_CONFIG
  ${ITK_INCLUDE_DIRS_BUILD_TREE}
  ${ITK_INCLUDE_DIRS_SYSTEM}
)

# The CableSwig configuration directory.
if(ITK_NEED_CableSwig)
  # Use the same CableSwig we used.
  set(ITK_CableSwig_DIR_CONFIG ${CableSwig_DIR})
else(ITK_NEED_CableSwig)
  # No wrapping was done.  Do not provide a CableSwig setting.
  set(ITK_CableSwig_DIR_CONFIG "")
endif(ITK_NEED_CableSwig)

#-----------------------------------------------------------------------------
# Configure ITKConfig.cmake for the build tree.
configure_file(${ITK_SOURCE_DIR}/ITKConfig.cmake.in
               ${ITK_BINARY_DIR}/ITKConfig.cmake @ONLY IMMEDIATE)

#-----------------------------------------------------------------------------
# Settings specific to the install tree.

# The library dependencies file.
set(ITK_LIBRARY_DEPENDS_FILE "\${ITK_INSTALL_PREFIX}${ITK_INSTALL_PACKAGE_DIR}/ITKLibraryDepends.cmake")

# The "use" file.
set(ITK_USE_FILE \${ITK_INSTALL_PREFIX}${ITK_INSTALL_PACKAGE_DIR}/UseITK.cmake)

# The build settings file.
set(ITK_BUILD_SETTINGS_FILE \${ITK_INSTALL_PREFIX}${ITK_INSTALL_PACKAGE_DIR}/ITKBuildSettings.cmake)

# Include directories.
set(ITK_INCLUDE_DIRS_CONFIG \${ITK_INSTALL_PREFIX}${ITK_INSTALL_INCLUDE_DIR})
foreach(DIR ${ITK_INCLUDE_RELATIVE_DIRS})
  list(APPEND ITK_INCLUDE_DIRS_CONFIG \${ITK_INSTALL_PREFIX}${ITK_INSTALL_INCLUDE_DIR}/${DIR})
endforeach(DIR)
if(ITK_INCLUDE_DIRS_SYSTEM)
  list(APPEND ITK_INCLUDE_DIRS_CONFIG ${ITK_INCLUDE_DIRS_SYSTEM})
endif(ITK_INCLUDE_DIRS_SYSTEM)

# Link directories.
set(ITK_LIBRARY_DIRS_CONFIG "\${ITK_INSTALL_PREFIX}${ITK_INSTALL_LIB_DIR}")

# The CableSwig configuration directory.
if(ITK_NEED_CableSwig)
  if(ITK_BUILD_CABLESWIG)
    # We built an internal CableSwig.
    set(ITK_CableSwig_DIR_CONFIG
      ${CableSwig_INSTALL_ROOT}/lib/CableSwig)
  else(ITK_BUILD_CABLESWIG)
    # We built with an external CableSwig.
    set(ITK_CableSwig_DIR_CONFIG ${CableSwig_DIR})
  endif(ITK_BUILD_CABLESWIG)
else(ITK_NEED_CableSwig)
  # No wrapping was done.  Do not provide a CableSwig setting.
  set(ITK_CableSwig_DIR_CONFIG "")
endif(ITK_NEED_CableSwig)

#-----------------------------------------------------------------------------
# Configure ITKConfig.cmake for the install tree.

# Construct the proper number of get_filename_component(... PATH)
# calls to compute the installation prefix.
string(REGEX REPLACE "/" ";" ITK_INSTALL_PACKAGE_DIR_COUNT
  "${ITK_INSTALL_PACKAGE_DIR}")
set(ITK_CONFIG_CODE "
# Compute the installation prefix from this ITKConfig.cmake file location.
get_filename_component(ITK_INSTALL_PREFIX \"\${CMAKE_CURRENT_LIST_FILE}\" PATH)")
foreach(p ${ITK_INSTALL_PACKAGE_DIR_COUNT})
  set(ITK_CONFIG_CODE
    "${ITK_CONFIG_CODE}\nGET_FILENAME_COMPONENT(ITK_INSTALL_PREFIX \"\${ITK_INSTALL_PREFIX}\" PATH)"
    )
endforeach(p)


configure_file(${ITK_SOURCE_DIR}/ITKConfig.cmake.in
               ${ITK_BINARY_DIR}/Utilities/ITKConfig.cmake @ONLY IMMEDIATE)

