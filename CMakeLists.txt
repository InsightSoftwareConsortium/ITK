# This is the root ITK CMakeLists file.
CMAKE_MINIMUM_REQUIRED(VERSION 1.6)
PROJECT(ITK)
INCLUDE_REGULAR_EXPRESSION("^(itk|vtk|vnl|vcl|vxl|f2c|netlib|ce|itpack|DICOM|meta|png|dbh).*$")
SOURCE_GROUP("XML Files" "[.]xml$")

# Don't build anything unless the version of CMake is high enough.
# The matching ELSE/ENDIF should be the last lines in the file.
IF("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" GREATER 1.5)

#-----------------------------------------------------------------------------
# ITK version number.  An even minor number corresponds to releases.
SET(ITK_VERSION_MAJOR "1")
SET(ITK_VERSION_MINOR "3")
SET(ITK_VERSION_PATCH "0")

# Version string should not include patch level.  The major.minor is
# enough to distinguish available features of the toolkit.
SET(ITK_VERSION_STRING "${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}")

#-----------------------------------------------------------------------------
# Load some macros.
INCLUDE(${ITK_SOURCE_DIR}/Utilities/itkThirdParty.cmake)

#-----------------------------------------------------------------------------
# Test for some required system information.
INCLUDE (${CMAKE_ROOT}/Modules/CMakeBackwardCompatibilityC.cmake)
INCLUDE (${CMAKE_ROOT}/Modules/CMakeBackwardCompatibilityCXX.cmake)

#-----------------------------------------------------------------------------
# Output directories.
SET (LIBRARY_OUTPUT_PATH ${ITK_BINARY_DIR}/bin CACHE PATH "Single output directory for building all libraries.")
SET (EXECUTABLE_OUTPUT_PATH ${ITK_BINARY_DIR}/bin CACHE PATH "Single output directory for building all executables.")
MARK_AS_ADVANCED(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)
SET(ITK_LIBRARY_PATH "${LIBRARY_OUTPUT_PATH}")
SET(ITK_EXECUTABLE_PATH "${EXECUTABLE_OUTPUT_PATH}")
SET(CXX_TEST_PATH ${EXECUTABLE_OUTPUT_PATH})

# Put "." in the rpath of all ITK shared libraries.
LINK_DIRECTORIES(.)

#-----------------------------------------------------------------------------
# ITK build configuration options.
IF(NOT WIN32)
  OPTION(BUILD_SHARED_LIBS "Build ITK with shared libraries." OFF)
ENDIF(NOT WIN32)
OPTION(ITK_WRAP_TCL "Build Tcl wrapper support (requires CABLE/GCC-XML)." OFF)
OPTION(BUILD_EXAMPLES "Build the Examples directory." ON)
OPTION(ITK_USE_SYSTEM_STDLIB
       "Use the system's version of the standard C++ library (STL and streams).  If 'OFF', ITK will build against a supplied version of the standard library from SGI.  On SGI systems, this should be set to 'OFF'." ON)
SET(ITK_USE_SYSTEM_STDLIB ON)
SET(VXL_NUMERICS_LIBRARIES itkvnl itkvnl_algo itknetlib)

# Provide options to use system versions of third-party libraries.
ITK_THIRD_PARTY_OPTION(ZLIB zlib)
ITK_THIRD_PARTY_OPTION(PNG  png)

IF(CMAKE_SYSTEM MATCHES "IRIX.*")
ELSE(CMAKE_SYSTEM MATCHES "IRIX.*")
  MARK_AS_ADVANCED(ITK_USE_SYSTEM_STDLIB)
ENDIF(CMAKE_SYSTEM MATCHES "IRIX.*")

#-----------------------------------------------------------------------------
# Configure Dart testing support.
INCLUDE(${CMAKE_ROOT}/Modules/Dart.cmake)
MARK_AS_ADVANCED(TCL_TCLSH)
IF(BUILD_TESTING)
  ENABLE_TESTING()
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/InsightLogo.gif
                 ${ITK_BINARY_DIR}/Testing/HTML/TestingResults/Icons/Logo.gif
                 COPYONLY IMMEDIATE)
ENDIF(BUILD_TESTING)

#-----------------------------------------------------------------------------
# Dispatch the build into the proper subdirectories.
SUBDIRS(Utilities Code)

IF(ITK_WRAP_TCL)
  SUBDIRS(Wrapping)
ENDIF(ITK_WRAP_TCL)

IF (BUILD_TESTING)
  SUBDIRS(Testing)
ENDIF (BUILD_TESTING)

IF(BUILD_EXAMPLES)
  SUBDIRS(Examples)
ENDIF(BUILD_EXAMPLES)

#-----------------------------------------------------------------------------
# ITK requires special compiler flags on some platforms.
IF(CMAKE_COMPILER_IS_GNUCXX)
  SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -ftemplate-depth-50")
  IF(APPLE)
    SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -Wno-long-double")
  ENDIF(APPLE)
ENDIF(CMAKE_COMPILER_IS_GNUCXX)

# Force the use of ansi cxx flags (i.e. -LANG:std on sgi )
IF("x${CMAKE_ANSI_CXXFLAGS}" MATCHES "^x.*[^ ]")
  SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} ${CMAKE_ANSI_CXXFLAGS}")
ENDIF("x${CMAKE_ANSI_CXXFLAGS}" MATCHES "^x.*[^ ]")
IF("x${CMAKE_ANSI_CFLAGS}" MATCHES "^x.*[^ ]")
  SET(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} ${CMAKE_ANSI_CFLAGS}")
ENDIF("x${CMAKE_ANSI_CFLAGS}" MATCHES "^x.*[^ ]")

IF("x${CMAKE_TEMPLATE_FLAGS}" MATCHES "^x.*[^ ]")
  SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} ${CMAKE_TEMPLATE_FLAGS}")
ENDIF("x${CMAKE_TEMPLATE_FLAGS}" MATCHES "^x.*[^ ]")

IF(CMAKE_USE_PTHREADS) 
  IF(CMAKE_SYSTEM MATCHES "SunOS.*")
    SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -D_PTHREADS")
  ENDIF(CMAKE_SYSTEM MATCHES "SunOS.*")
ENDIF(CMAKE_USE_PTHREADS)
IF(WIN32)
  SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -DNOMINMAX")
ENDIF(WIN32)

IF(NOT ITK_USE_SYSTEM_STDLIB)
  IF(CMAKE_COMPILER_IS_GNUCXX)
    SET(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -D__STL_USE_NEW_IOSTREAMS")
  ENDIF(CMAKE_COMPILER_IS_GNUCXX)
ENDIF(NOT ITK_USE_SYSTEM_STDLIB)

#-----------------------------------------------------------------------------
# Configure the default ITK_DATA_ROOT for the location of ITK Data.
FIND_PATH(ITK_DATA_ROOT ITKData.readme ${ITK_SOURCE_DIR}/Testing/Data $ENV{ITK_DATA_ROOT})

#-----------------------------------------------------------------------------
# Configure Tcl support.
IF(ITK_WRAP_TCL)
  INCLUDE(${CMAKE_ROOT}/Modules/FindTCL.cmake)

  # Hide useless settings provided by FindTCL.
  FOREACH(entry TCL_LIBRARY_DEBUG
                TK_LIBRARY_DEBUG
                TCL_STUB_LIBRARY
                TCL_STUB_LIBRARY_DEBUG
                TK_STUB_LIBRARY
                TK_STUB_LIBRARY_DEBUG
                TK_WISH)
    SET(${entry} "${${entry}}" CACHE INTERNAL "This value is not used by ITK.")
  ENDFOREACH(entry)
ENDIF(ITK_WRAP_TCL)

#-----------------------------------------------------------------------------
# Configure files with settings for use by the build.
CONFIGURE_FILE(${ITK_SOURCE_DIR}/itkConfigure.h.in 
               ${ITK_BINARY_DIR}/itkConfigure.h)
CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Doxygen/doxygen.config.in
               ${ITK_BINARY_DIR}/doxygen.config)
CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Doxygen/itkdoxygen.pl.in
               ${ITK_BINARY_DIR}/itkdoxygen.pl)

#-----------------------------------------------------------------------------
# The entire ITK tree should use the same include path, except for the
# Wrapping directory.

# Create the list of include directories needed for ITK header files.
INCLUDE(${ITK_SOURCE_DIR}/itkIncludeDirectories.cmake)

# This should be the only INCLUDE_DIRECTORIES command in the entire
# tree, except for the Utilities and Wrapping directories.  We need to
# do this in one place to make sure the order is correct.
INCLUDE_DIRECTORIES(
  ${ITK_INCLUDE_DIRS_BUILD_TREE}
  ${ITK_INCLUDE_DIRS_BUILD_TREE_CXX}
  ${ITK_INCLUDE_DIRS_SYSTEM}
)

#-----------------------------------------------------------------------------
# Help other projects use ITK.

# Copy the UseITK.cmake file to the binary tree for backward compatability.
CONFIGURE_FILE(${ITK_SOURCE_DIR}/UseITK.cmake.in
               ${ITK_BINARY_DIR}/UseITK.cmake COPYONLY IMMEDIATE)

# Save the compiler settings so another project can import them.
INCLUDE(${CMAKE_ROOT}/Modules/CMakeExportBuildSettings.cmake)
CMAKE_EXPORT_BUILD_SETTINGS(${ITK_BINARY_DIR}/ITKBuildSettings.cmake)
INSTALL_FILES(/lib/InsightToolkit .cmake ITKBuildSettings)

# Save library dependencies.
EXPORT_LIBRARY_DEPENDENCIES(${ITK_BINARY_DIR}/ITKLibraryDepends.cmake)

# Create the ITKConfig.cmake file containing the ITK configuration.
INCLUDE (${ITK_SOURCE_DIR}/itkGenerateITKConfig.cmake)

# Install some files.
INSTALL_FILES(/include/InsightToolkit .h itkConfigure)
INSTALL_FILES(/lib/InsightToolkit .cmake UseITK ITKLibraryDepends)

#-----------------------------------------------------------------------------
# Add compiler flags ITK needs to work on this platform.  This must be
# done AFTER the call to CMAKE_EXPORT_BUILD_SETTINGS.

SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${ITK_REQUIRED_C_FLAGS}")
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${ITK_REQUIRED_CXX_FLAGS}")

# If the version of CMake was too old, complain and build nothing.
# These should be the last lines in this file.
ELSE("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" GREATER 1.5)
  MESSAGE(SEND_ERROR
          "This version of CMake is too old to build ITK.  "
          "Please upgrade to CMake 1.6.")
ENDIF("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" GREATER 1.5)
