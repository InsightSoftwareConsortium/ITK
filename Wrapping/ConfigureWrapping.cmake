###############################################################################
# ConfigureWrapping.cmake
#
# This file sets up all needed macros, paths, and so forth for wrapping itk
# projects.
#
# The following variables should be set before including this file:
# ITK_WRAP_TCL
# ITK_WRAP_PYTHON
# ITK_WRAP_JAVA
# ITK_WRAP_unsigned_char
# ITK_WRAP_unsigned_short
# ITK_WRAP_unsigned_long
# ITK_WRAP_signed_char
# ITK_WRAP_signed_short
# ITK_WRAP_signed_long
# ITK_WRAP_float
# ITK_WRAP_double
# ITK_WRAP_vector_float
# ITK_WRAP_vector_double
# ITK_WRAP_covariant_vector_float
# ITK_WRAP_covariant_vector_double
# ITK_WRAP_IMAGE_DIMS
# ITK_WRAP_JAVA_DIR -- directory for java classes to be placed
# WRAP_ITK_CONFIG_DIR -- directory where XXX.in files for CONFIGURE_FILE
#                        commands are to be found.
# WRAP_ITK_CMAKE_DIR -- directory where XXX.cmake files are to be found
#
# This file sets a default value for WRAPPER_MASTER_INDEX_OUTPUT_DIR and
# WRAPPER_SWIG_LIBRARY_OUTPUT_DIR. Change it after including this file if needed,
# but this shouldn't really be necessary except for complex external projects.
#
# A note on convention: Global variables (those shared between macros) are
# defined in ALL_CAPS (or partially all-caps, for the WRAP_pixel_type) values
# listed above. Variables local to a macro are in lower-case.
# Moreover, only variables defined in this file (or listed) above are shared
# across macros defined in different files. All other global variables are
# only used by the macros defined in a given cmake file.
###############################################################################


###############################################################################
# Find Required Packages
###############################################################################

#-----------------------------------------------------------------------------
# Find ITK
#-----------------------------------------------------------------------------
find_package(ITK REQUIRED)
include(${ITK_USE_FILE})

###############################################################################
# Set various variables in order
###############################################################################
# set(CMAKE_SKIP_RPATH ON CACHE BOOL "ITK wrappers must not have runtime path information." FORCE)

#------------------------------------------------------------------------------
# System dependant wrapping stuff

# Make a variable that expands to nothing if there are no configuration types,
# otherwise it expands to the active type plus a /, so that in either case,
# the variable can be used in the middle of a path.
if(CMAKE_CONFIGURATION_TYPES)
  set(WRAP_ITK_BUILD_INTDIR "${CMAKE_CFG_INTDIR}/")
  set(WRAP_ITK_INSTALL_INTDIR "\${BUILD_TYPE}/")

  # horrible hack to avoid having ${BUILD_TYPE} expanded to an empty string
  # while passing through the macros.
  # Instead of expanding to an empty string, it expands to ${BUILD_TYPE}
  # and so can be reexpanded again and again (and again)
  set(BUILD_TYPE "\${BUILD_TYPE}")

else()
  set(WRAP_ITK_BUILD_INTDIR "")
  set(WRAP_ITK_INSTALL_INTDIR "")
endif()


set(ITK_WRAP_NEEDS_DEPEND 1)
if(${CMAKE_MAKE_PROGRAM} MATCHES make)
  set(ITK_WRAP_NEEDS_DEPEND 0)
endif()

set(CSWIG_EXTRA_LINKFLAGS )
if(CMAKE_BUILD_TOOL MATCHES "(msdev|devenv|nmake)")
  set(CSWIG_EXTRA_LINKFLAGS "/IGNORE:4049 /IGNORE:4109")
endif()

if(CMAKE_SYSTEM MATCHES "IRIX.*")
  if(CMAKE_CXX_COMPILER MATCHES "CC")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -woff 1552")
  endif()
endif()

if(CMAKE_COMPILER_IS_GNUCXX)
  string(REGEX REPLACE "-Wcast-qual" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
endif()

if(UNIX)
  set(WRAP_ITK_LIBNAME_PREFIX "lib")
else()
  set(WRAP_ITK_LIBNAME_PREFIX "")
endif()

# generators dir
set(GENERATORS_SRC_DIR "${WRAP_ITK_CMAKE_DIR}/Generators" CACHE INTERNAL "generators source directory")

###############################################################################
# Define install files macro. If we are building WrapITK, the generated files
# and libraries will be installed into CMAKE_INSTALL_PREFIX, as usual. However,
# if we are building an external project, we need to ensure that the wrapper
# files will be installed into wherever WrapITK was installed.
###############################################################################
include("${WRAP_ITK_CMAKE_DIR}/CMakeUtilityFunctions.cmake")

macro(WRAP_ITK_INSTALL path)
  # Install documentation along with ITKCommon wrapping
  set(_component_module "")
  if(WRAP_ITK_INSTALL_COMPONENT_PER_MODULE)
    set(_component_module "ITKCommon")
  endif()
  install(FILES ${ARGN}
    DESTINATION "${WRAP_ITK_INSTALL_PREFIX}${path}"
    COMPONENT ${_component_module}${WRAP_ITK_INSTALL_COMPONENT_IDENTIFIER}RuntimeLibraries
    )
endmacro()


###############################################################################
# Macro to install the language bindings
###############################################################################
macro(WRAP_ITK_BINDINGS_INSTALL path)
  if(WRAP_ITK_INSTALL_COMPONENT_PER_MODULE)
    message(WARNING "Option WRAP_ITK_INSTALL_COMPONENT_PER_MODULE is only supported for Python wrapping language")
  endif()
  install(FILES ${ARGN}
    DESTINATION "${ITK_INSTALL_LIBRARY_DIR}/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}${path}"
    COMPONENT ${WRAP_ITK_INSTALL_COMPONENT_IDENTIFIER}RuntimeLibraries
    )
endmacro()

###############################################################################
# Include needed macros -- WRAP_ITK_CMAKE_DIR must be set correctly
###############################################################################
include("${WRAP_ITK_CMAKE_DIR}/TypedefMacros.cmake")

###############################################################################
# Create wrapper names for simple types to ensure consistent naming
###############################################################################
include("${WRAP_ITK_CMAKE_DIR}/WrapBasicTypes.cmake")
include("${WRAP_ITK_CMAKE_DIR}/WrapITKTypes.cmake")

###############################################################################
# Lets the target generators do their job
###############################################################################
add_subdirectory("${WRAP_ITK_CMAKE_DIR}/Generators" "${CMAKE_CURRENT_BINARY_DIR}/Generators")
# get the porperties from the generators dirs - there should be others than this one
get_directory_property(inc DIRECTORY "${WRAP_ITK_CMAKE_DIR}/Generators" INCLUDE_DIRECTORIES)
include_directories(${inc})
