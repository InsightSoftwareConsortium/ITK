###############################################################################
# ConfigureWrapping.cmake
#
# This file sets up all needed macros, paths, and so forth for wrapping itk
# projects.
#
# The following variables should be set before including this file:
# WRAP_ITK_TCL 
# WRAP_ITK_PYTHON 
# WRAP_ITK_JAVA 
# WRAP_unsigned_char
# WRAP_unsigned_short 
# WRAP_unsigned_long 
# WRAP_signed_char 
# WRAP_signed_short 
# WRAP_signed_long 
# WRAP_float 
# WRAP_double 
# WRAP_vector_float 
# WRAP_vector_double
# WRAP_covariant_vector_float 
# WRAP_covariant_vector_double 
# WRAP_ITK_DIMS
# WRAP_ITK_JAVA_DIR -- directory for java classes to be placed
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
# Define fundamental wrapping macro which sets up the global variables used
# across all of the wrapping macros included at the end of this file. 
# All variables set here are optional and have sensible default values.
# Also define some other global defaults like WRAPPER_MASTER_INDEX_OUTPUT_DIR.
###############################################################################
MACRO(BEGIN_WRAPPER_LIBRARY library_name)
  SET(WRAPPER_LIBRARY_NAME "${library_name}")

  # Mark the current source dir for inclusion because it may contain header files.
  INCLUDE_DIRECTORIES("${CMAKE_CURRENT_SOURCE_DIR}")
  
  # WRAPPER_LIBRARY_SOURCE_DIR. Directory to be scanned for wrap_*.cmake files. 
  SET(WRAPPER_LIBRARY_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")
  
  # WRAPPER_LIBRARY_OUTPUT_DIR. Directory in which generated cxx, xml, and idx
  # files will be placed. 
  SET(WRAPPER_LIBRARY_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}")

  # WRAPPER_LIBRARY_DEPENDS. List of names of other wrapper libraries that
  # define symbols used by this wrapper library.
  SET(WRAPPER_LIBRARY_DEPENDS )

  # WRAPPER_LIBRARY_LINK_LIBRARIES. List of other libraries that should
  # be linked to the wrapper library.
  SET(WRAPPER_LIBRARY_LINK_LIBRARIES )

  # WRAPPER_LIBRARY_GROUPS. List of wrap_*.cmake groups in the source dir
  # that should be included/wrapped before the rest. Just the group name is needed,
  # not the full path or file name. 
  SET(WRAPPER_LIBRARY_GROUPS )

  # WRAPPER_LIBRARY_CABLESWIG_INPUTS. List of C++ source files to be used
  # as input for CableSwig. This list is then appended to by
  # WRAPPER_LIBRARY_AUTO_INCLUDE_WRAP_FILES. A full path to each input is required.
  SET(WRAPPER_LIBRARY_CABLESWIG_INPUTS )

  # WRAPPER_SWIG_LIBRARY_FILES. List of swig .swg files to pass to cswig to control
  # type handling and so forth. A full path to each include is required.
  # The itk.swg file and the library file for the current library are implicitly added.
  SET(WRAPPER_SWIG_LIBRARY_FILES )

  # WRAPPER_LIBRARY_SWIG_INPUTS. SWIG input files to be fed to swig (not
  # CableSwig). A full path to each input is required.
  SET(WRAPPER_LIBRARY_SWIG_INPUTS ) 

  # WRAPPER_LIBRARY_CXX_SOURCES. C++ sources to be compiled and linked in
  # to the wrapper library (with no prior processing by swig, etc.)
  # A full path to each input is required.
  SET(WRAPPER_LIBRARY_CXX_SOURCES ) 

  IF("${ARGC}" EQUAL 2)
    SET(WRAPPER_LIBRARY_PYTHON OFF)
    SET(WRAPPER_LIBRARY_TCL OFF)
    SET(WRAPPER_LIBRARY_JAVA OFF)
    FOREACH(lang ${ARGV1})
      IF("${lang}" STREQUAL "Python") 
        SET(WRAPPER_LIBRARY_PYTHON ON)
      ENDIF("${lang}" STREQUAL "Python")

      IF("${lang}" STREQUAL "Tcl") 
        SET(WRAPPER_LIBRARY_TCL ON)
      ENDIF("${lang}" STREQUAL "Tcl")

      IF("${lang}" STREQUAL "Java") 
        SET(WRAPPER_LIBRARY_JAVA ON)
      ENDIF("${lang}" STREQUAL "Java")
    ENDFOREACH(lang)
  ELSE("${ARGC}" EQUAL 2)
    SET(WRAPPER_LIBRARY_PYTHON ON)
    SET(WRAPPER_LIBRARY_TCL ON)
    SET(WRAPPER_LIBRARY_JAVA ON)
  ENDIF("${ARGC}" EQUAL 2)

# MESSAGE("${library_name} ${WRAPPER_LIBRARY_PYTHON} ${WRAPPER_LIBRARY_TCL} ${WRAPPER_LIBRARY_JAVA}")

  # Call the language support initialization function from CreateLanguageSupport.cmake
  LANGUAGE_SUPPORT_INITIALIZE()
ENDMACRO(BEGIN_WRAPPER_LIBRARY)

SET(WRAPPER_MASTER_INDEX_OUTPUT_DIR "${PROJECT_BINARY_DIR}/ClassIndex")
SET(WRAPPER_SWIG_LIBRARY_OUTPUT_DIR "${PROJECT_BINARY_DIR}/SWIG")

###############################################################################
# Find Required Packages
###############################################################################

#-----------------------------------------------------------------------------
# Find ITK
#-----------------------------------------------------------------------------
FIND_PACKAGE(ITK REQUIRED)
INCLUDE(${ITK_USE_FILE})
# we must be sure we have the right ITK version; WrapITK can't build with
# an old version of ITK because some classes will not be there.
# newer version should only cause some warnings
SET(ITK_REQUIRED_VERSION "2.9.0")
SET(ITK_VERSION "${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}.${ITK_VERSION_PATCH}")
IF("${ITK_VERSION}" STRLESS "${ITK_REQUIRED_VERSION}")
  MESSAGE(FATAL_ERROR "ITK ${ITK_REQUIRED_VERSION} is required to build this version of WrapITK, and you are trying to use version ${ITK_VERSION}. Set ITK_DIR to point to the directory of ITK ${ITK_REQUIRED_VERSION}.")
ENDIF("${ITK_VERSION}" STRLESS "${ITK_REQUIRED_VERSION}")

#-----------------------------------------------------------------------------
# Load the CableSwig settings used by ITK, or find CableSwig otherwise.
#-----------------------------------------------------------------------------
#
#SET(CableSwig_DIR ${ITK_CableSwig_DIR})
FIND_PACKAGE(CableSwig REQUIRED)


# We have found CableSwig.  Use the settings.
SET(CABLE_INDEX ${CableSwig_cableidx_EXE})
SET(CSWIG ${CableSwig_cswig_EXE})
SET(GCCXML ${CableSwig_gccxml_EXE})

SET(CSWIG_MISSING_VALUES)
IF(NOT CSWIG)
   SET(CSWIG_MISSING_VALUES "${CSWIG_MISSING_VALUES} CSWIG ")
ENDIF(NOT CSWIG)
IF(NOT CABLE_INDEX)
   SET(CSWIG_MISSING_VALUES "${CSWIG_MISSING_VALUES} CABLE_INDEX ")
ENDIF(NOT CABLE_INDEX)
IF(NOT GCCXML)
   SET(CSWIG_MISSING_VALUES "${CSWIG_MISSING_VALUES} GCCXML ")
ENDIF(NOT GCCXML)
IF(CSWIG_MISSING_VALUES)
  MESSAGE(SEND_ERROR "To use cswig wrapping, CSWIG, CABLE_INDEX, and GCCXML executables must be specified.  If they are all in the same directory, only specifiy one of them, and then run cmake configure again and the others should be found.\nCurrently, you are missing the following:\n ${CSWIG_MISSING_VALUES}")
ENDIF(CSWIG_MISSING_VALUES)

#-----------------------------------------------------------------------------
# Find wrapping language API libraries.
#-----------------------------------------------------------------------------
IF(WRAP_ITK_TCL)
  INCLUDE("${WRAP_ITK_CMAKE_DIR}/Tcl/ConfigureWrapping.cmake")
ENDIF(WRAP_ITK_TCL)

IF(WRAP_ITK_PYTHON)
  INCLUDE("${WRAP_ITK_CMAKE_DIR}/Python/ConfigureWrapping.cmake")
ENDIF(WRAP_ITK_PYTHON)

IF(WRAP_ITK_JAVA)
  INCLUDE("${WRAP_ITK_CMAKE_DIR}/Java/ConfigureWrapping.cmake")
ENDIF(WRAP_ITK_JAVA)

IF(WRAP_ITK_PERL)
  INCLUDE("${WRAP_ITK_CMAKE_DIR}/Perl/ConfigureWrapping.cmake")
ENDIF(WRAP_ITK_PERL)


###############################################################################
# Set various variables in order
###############################################################################
# SET(CMAKE_SKIP_RPATH ON CACHE BOOL "ITK wrappers must not have runtime path information." FORCE)

#------------------------------------------------------------------------------
# System dependant wraping stuff

# Make a variable that expands to nothing if there are no configuration types,
# otherwise it expands to the active type plus a /, so that in either case,
# the variable can be used in the middle of a path.
IF(CMAKE_CONFIGURATION_TYPES)
  SET(WRAP_ITK_BUILD_INTDIR "${CMAKE_CFG_INTDIR}/")
  SET(WRAP_ITK_INSTALL_INTDIR "\${BUILD_TYPE}/")

  # horrible hack to avoid having ${BUILD_TYPE} expanded to an empty sting
  # while passing through the macros.
  # Insitead of expanding to an empty string, it expand to ${BUILD_TYPE}
  # and so can be reexpanded again and again (and again)
  SET(BUILD_TYPE "\${BUILD_TYPE}")

ELSE(CMAKE_CONFIGURATION_TYPES)
  SET(WRAP_ITK_BUILD_INTDIR "")
  SET(WRAP_ITK_INSTALL_INTDIR "")
ENDIF(CMAKE_CONFIGURATION_TYPES)


SET(ITK_WRAP_NEEDS_DEPEND 1)
IF(${CMAKE_MAKE_PROGRAM} MATCHES make)
  SET(ITK_WRAP_NEEDS_DEPEND 0)
ENDIF(${CMAKE_MAKE_PROGRAM} MATCHES make)

SET(CSWIG_DEFAULT_LIB ${CableSwig_DIR}/SWIG/Lib )

SET(CSWIG_EXTRA_LINKFLAGS )
IF(CMAKE_BUILD_TOOL MATCHES "(msdev|devenv|nmake)")
  SET(CSWIG_EXTRA_LINKFLAGS "/IGNORE:4049 /IGNORE:4109")
ENDIF(CMAKE_BUILD_TOOL MATCHES "(msdev|devenv|nmake)")

IF(CMAKE_SYSTEM MATCHES "IRIX.*")
  IF(CMAKE_CXX_COMPILER MATCHES "CC")
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -woff 1552")
  ENDIF(CMAKE_CXX_COMPILER MATCHES "CC")
ENDIF(CMAKE_SYSTEM MATCHES "IRIX.*")

IF(CMAKE_COMPILER_IS_GNUCXX)
  STRING(REGEX REPLACE "-Wcast-qual" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
ENDIF(CMAKE_COMPILER_IS_GNUCXX)

IF(UNIX)
  SET(WRAP_ITK_LIBNAME_PREFIX "lib")
ELSE(UNIX)
  SET(WRAP_ITK_LIBNAME_PREFIX "")
ENDIF(UNIX)

# 467 is for warnings caused by typemap on overloaded methods
SET(CSWIG_IGNORE_WARNINGS -w362 -w389 -w467 -w503 -w508 -w509 -w516)
ADD_DEFINITIONS(-DSWIG_GLOBAL)

###############################################################################
# Define install files macro. If we are building WrapITK, the generated files
# and libraries will be installed into CMAKE_INSTALL_PREFIX, as usual. However,
# if we are building an external project, we need to ensure that the wrapper 
# files will be installed into wherever WrapITK was installed. 
###############################################################################
INCLUDE("${WRAP_ITK_CMAKE_DIR}/CMakeUtilityFunctions.cmake")

IF(EXTERNAL_WRAP_ITK_PROJECT)
  CREATE_INSTALL_AT_ABSOLUTE_PATH_TARGET(install_external_wrapitk_project DEFAULT
    "Installing external project ${PROJECT} into the WrapITK installation directory.")
  MACRO(WRAP_ITK_INSTALL path)
#    INSTALL_AT_ABSOLUTE_PATH(install_external_wrapitk_project "${WRAP_ITK_INSTALL_LOCATION}${path}" ${ARGN})
    INSTALL_FILES("${WRAP_ITK_INSTALL_PREFIX}${path}" FILES ${ARGN})
  ENDMACRO(WRAP_ITK_INSTALL)
ELSE(EXTERNAL_WRAP_ITK_PROJECT)
  MACRO(WRAP_ITK_INSTALL path)
    INSTALL_FILES("${WRAP_ITK_INSTALL_PREFIX}${path}" FILES ${ARGN})
  ENDMACRO(WRAP_ITK_INSTALL)
ENDIF(EXTERNAL_WRAP_ITK_PROJECT)

###############################################################################
# Include needed macros -- WRAP_ITK_CMAKE_DIR must be set correctly
###############################################################################
INCLUDE("${WRAP_ITK_CMAKE_DIR}/CreateCableSwigInputs.cmake")
INCLUDE("${WRAP_ITK_CMAKE_DIR}/CreateWrapperLibrary.cmake")
INCLUDE("${WRAP_ITK_CMAKE_DIR}/CreateLanguageSupport.cmake")

###############################################################################
# Create wrapper names for simple types to ensure consistent naming
###############################################################################
INCLUDE("${WRAP_ITK_CMAKE_DIR}/WrapBasicTypes.cmake")
INCLUDE("${WRAP_ITK_CMAKE_DIR}/WrapITKTypes.cmake")

