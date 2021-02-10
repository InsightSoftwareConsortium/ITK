# This file ensures the appropriate variables are set up for a project extending
# ITK before including ITKModuleMacros. This is the preferred way to build an
# ITK module outside of the ITK source tree.
if(NOT ITK_FOUND)
  message(FATAL_ERROR "ITK must be found before module macros can be used.")
endif()
if(NOT ITK_VERSION VERSION_GREATER "5.1")
  message(FATAL_ERROR "Requires ITK 5.1 or later to work.")
endif()
if(NOT EXISTS ${ITK_CMAKE_DIR}/ITKModuleMacros.cmake)
  message(FATAL_ERROR "Modules can only be built against an ITK build tree; they cannot be built against an ITK install tree.")
endif()

set(PYTHON_DEVELOPMENT_REQUIRED ${ITK_WRAP_PYTHON})
include(ITKSetPython3Vars)

# To hide dependent variables
include(CMakeDependentOption)

# Install rules when creating a Python package with scikit-build
if(SKBUILD)
  set(PY_SITE_PACKAGES_PATH ${CMAKE_INSTALL_PREFIX} CACHE PATH "The install prefix for python package contents")
  install(CODE "
    unset(CMAKE_INSTALL_COMPONENT)
    set(COMPONENT \"PythonWheelRuntimeLibraries\")
    set(CMAKE_INSTALL_DO_STRIP 1)
    include\(\"${PROJECT_BINARY_DIR}/cmake_install.cmake\")
    unset(CMAKE_INSTALL_COMPONENT)
    return()
")
endif()

# Configure find_package behavior
if(NOT DEFINED CMAKE_FIND_PACKAGE_NO_PACKAGE_REGISTRY)
  set(CMAKE_FIND_PACKAGE_NO_PACKAGE_REGISTRY 1)
endif()


# Setup build locations for shared libraries ----START
#     ITK/CMakeLists.txt -- use ITK_BINARY_DIR as root
#     ITK/CMake/ITKModuleExternal.cmake -- use ITK_DIR as root
if(NOT ITK_BINARY_DIR)
  set(ITK_BINARY_DIR ${ITK_DIR})
endif()

# The default path when not wrapping.  Restore standard build location
# if python wrapping is turned on, and then turned off.
if(NOT CMAKE_LIBRARY_OUTPUT_DIRECTORY)
  set(NO_WRAP_CMAKE_LIBRARY_OUTPUT_DIRECTORY ${ITK_BINARY_DIR}/lib CACHE PATH "Shared library directory")
else()
  set(NO_WRAP_CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} CACHE PATH "Shared library directory")
endif()
if(NOT CMAKE_RUNTIME_OUTPUT_DIRECTORY)
  set(NO_WRAP_CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ITK_BINARY_DIR}/bin CACHE PATH "Shared library directory")
else()
  set(NO_WRAP_CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY} CACHE PATH "Shared library directory")
endif()

if(ITK_WRAPPING)
  # WRAPPER_LIBRARY_OUTPUT_DIR. Directory in which generated cxx, xml, and idx files will be placed.
  set(WRAPPER_LIBRARY_OUTPUT_DIR "${ITK_DIR}/Wrapping" CACHE INTERNAL "Need to specify the output library directory globally")
  if(ITK_WRAP_PYTHON)
    set(ITK_WRAP_PYTHON_ROOT_BINARY_DIR "${WRAPPER_LIBRARY_OUTPUT_DIR}/Generators/Python" CACHE INTERNAL "python binary dir")
    # create the directory to avoid loosing case on windows
    file(MAKE_DIRECTORY ${ITK_WRAP_PYTHON_ROOT_BINARY_DIR})

    set(ITK_PYTHON_PACKAGE_DIR "${ITK_WRAP_PYTHON_ROOT_BINARY_DIR}/itk")
    # create the directory to avoid loosing case on windows
    file(MAKE_DIRECTORY ${ITK_PYTHON_PACKAGE_DIR})

    set(ITK_WRAP_PYTHON_SWIG_CONFIGURATION_DIR "${ITK_PYTHON_PACKAGE_DIR}/Configuration" CACHE INTERNAL "python binary dir")
    # create the directory to avoid loosing case on windows
    file(MAKE_DIRECTORY ${ITK_WRAP_PYTHON_SWIG_CONFIGURATION_DIR})

    # IF WRAP_PYTHON then we must unconditionally set the CMAKE_LIBRARY_OUTPUT_DIRECTORY
    # If wrapping for python, then put all the shared libraries (both core shared libs,
    # and python shared libs) in the itk python package directory.
    #
    # https://cmake.org/cmake/help/v3.10/prop_tgt/LIBRARY_OUTPUT_DIRECTORY.html#prop_tgt:LIBRARY_OUTPUT_DIRECTORY
    # Multi-configuration generators (VS, Xcode) append a per-configuration subdirectory
    # to the specified directory unless a generator expression is used.
    # Using an always true generator expression to disable multi-config standard behavior
    #
    # When wrapping, the multi-config generators can only be used in degraded state
    # of allowing only a single element int the CMAKE_CONFIGURATION_TYPES and enforcing
    # that CMAKE_BUILD_TYPE match that type (see Wrapping/CMakeLists.txt enforcement)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "$<1:${ITK_PYTHON_PACKAGE_DIR}>" CACHE PATH "Shared library directory with generator override")
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "$<1:${ITK_PYTHON_PACKAGE_DIR}>" CACHE PATH "Shared library directory with generator override")
  endif()
else()
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${NO_WRAP_CMAKE_LIBRARY_OUTPUT_DIRECTORY}   CACHE PATH "Shared library directory")
endif()
# Setup build locations for shared libraries ----STOP

if(NOT CMAKE_ARCHIVE_OUTPUT_DIRECTORY)
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${ITK_DIR}/lib CACHE PATH "Static library install directory")
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${NO_WRAP_CMAKE_RUNTIME_OUTPUT_DIRECTORY}   CACHE PATH "Runtime library directory")
endif()

# ITK installation structure
if(NOT ITK_INSTALL_RUNTIME_DIR)
  set(ITK_INSTALL_RUNTIME_DIR bin)
endif()
if(NOT ITK_INSTALL_LIBRARY_DIR)
  set(ITK_INSTALL_LIBRARY_DIR lib)
endif()
if(NOT ITK_INSTALL_ARCHIVE_DIR)
  set(ITK_INSTALL_ARCHIVE_DIR lib)
endif()
if(NOT ITK_INSTALL_INCLUDE_DIR)
  set(ITK_INSTALL_INCLUDE_DIR include/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR})
endif()
if(NOT ITK_INSTALL_DATA_DIR)
  set(ITK_INSTALL_DATA_DIR share/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR})
endif()
if(NOT ITK_INSTALL_DOC_DIR)
  set(ITK_INSTALL_DOC_DIR share/doc/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR})
endif()
if(NOT ITK_INSTALL_PACKAGE_DIR)
  set(ITK_INSTALL_PACKAGE_DIR "lib/cmake/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}")
endif()

include(${ITK_CMAKE_DIR}/ITKInitializeBuildType.cmake)

# Use ITK's flags.
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${ITK_REQUIRED_C_FLAGS}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${ITK_REQUIRED_CXX_FLAGS}")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
set(BUILD_EXAMPLES ${ITK_BUILD_EXAMPLES})
set(BUILD_DOCUMENTATION ${ITK_BUILD_DOCUMENTATION})
option(BUILD_SHARED_LIBS "Build ITK with shared libraries." ${ITK_BUILD_SHARED})
if(NOT CMAKE_POSITION_INDEPENDENT_CODE)
  set(CMAKE_POSITION_INDEPENDENT_CODE ON)
endif()

# Add the ITK_MODULES_DIR to the CMAKE_MODULE_PATH and then use the binary
# directory for the project to write out new ones to.
if(ITK_MODULES_DIR)
  set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${ITK_MODULES_DIR})
endif()
set(ITK_MODULES_DIR "${ITK_DIR}/${ITK_INSTALL_PACKAGE_DIR}/Modules")

include(CTest)
include(ITKExternalData)
include(ITKModuleTest)
include(ITKDownloadSetup)

include(ITKModuleMacros)
include(itk-module.cmake)
set(${itk-module}-targets ${itk-module}Targets)
set(${itk-module}-targets-install "\${ITK_INSTALL_PREFIX}/${ITK_INSTALL_PACKAGE_DIR}/Modules/Targets/${itk-module}Targets.cmake")
set(${itk-module}_TARGETS_FILE_INSTALL "${${itk-module}-targets-install}")
set(${itk-module}-targets-build-directory "${ITK_DIR}/${ITK_INSTALL_PACKAGE_DIR}/Modules/Targets")
file(MAKE_DIRECTORY ${${itk-module}-targets-build-directory})
set(${itk-module}-targets-build "${${itk-module}-targets-build-directory}/${itk-module}Targets.cmake")
set(${itk-module}_TARGETS_FILE_BUILD "${${itk-module}-targets-build}")
file(WRITE "${${itk-module}_TARGETS_FILE_BUILD}" "") # Clear targets
set(${itk-module}_ENABLE_SHARED "${ITK_MODULE_${itk-module}_ENABLE_SHARED}")
itk_module_impl()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/src/CMakeLists.txt AND NOT ${itk-module}_NO_SRC AND "${${itk-module}-targets}")
  install(EXPORT ${${itk-module}-targets} DESTINATION "${ITK_INSTALL_PACKAGE_DIR}/Modules"
          COMPONENT Development)
endif()

set(ITK_TEST_OUTPUT_DIR "${CMAKE_BINARY_DIR}/Testing/Temporary")
if(${BUILD_TESTING} AND EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/test/CMakeLists.txt")
  add_subdirectory(test)
endif()

if(ITK_WRAPPING)
  CMAKE_DEPENDENT_OPTION(${itk-module}_WRAP_PYTHON "Build Python support." ${ITK_WRAP_PYTHON}
                       "ITK_WRAP_PYTHON" OFF)
  CMAKE_DEPENDENT_OPTION(${itk-module}_WRAP_JAVA "Build Java support." ${ITK_WRAP_JAVA}
                       "ITK_WRAP_JAVA" OFF)
  CMAKE_DEPENDENT_OPTION(${itk-module}_WRAP_RUBY "Build Ruby support." ${ITK_WRAP_RUBY}
                       "ITK_WRAP_RUBY" OFF)
  CMAKE_DEPENDENT_OPTION(${itk-module}_WRAP_PERL "Build Perl support." ${ITK_WRAP_PERL}
                       "ITK_WRAP_PERL" OFF)
  CMAKE_DEPENDENT_OPTION(${itk-module}_WRAP_TCL "Build Tcl support." ${ITK_WRAP_TCL}
                       "ITK_WRAP_TCL" OFF)
  CMAKE_DEPENDENT_OPTION(${itk-module}_WRAP_EXPLICIT "Build Explicit support." OFF
                       "ITK_WRAP_EXPLICIT" OFF)
  CMAKE_DEPENDENT_OPTION(${itk-module}_WRAP_DOC "Build Doxygen support." OFF
                       "ITK_WRAP_DOC" OFF)
  set(${itk-module}_WRAP_CASTXML ${ITK_WRAPPING})
  set(${itk-module}_WRAP_SWIGINTERFACE ${ITK_WRAPPING})
  if( (${itk-module}_WRAP_PYTHON OR
       ${itk-module}_WRAP_JAVA OR
       ${itk-module}_WRAP_RUBY OR
       ${itk-module}_WRAP_PERL OR
       ${itk-module}_WRAP_TCL OR
       ${itk-module}_WRAP_EXPLICIT OR
       ${itk-module}_WRAP_DOC
      )
    AND EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/wrapping/CMakeLists.txt"
    )
    set(EXTERNAL_WRAP_ITK_PROJECT ON)
    set(WRAP_ITK_CMAKE_DIR "${ITK_CMAKE_DIR}/../Wrapping")
    include("${WRAP_ITK_CMAKE_DIR}/TypedefMacros.cmake")
    # Build tree
    if(EXISTS "${ITK_CMAKE_DIR}/../Wrapping/CMakeLists.txt")
      add_subdirectory("${ITK_CMAKE_DIR}/../Wrapping"
        ${CMAKE_CURRENT_BINARY_DIR}/Wrapping)
    # Install tree
    elseif(EXISTS"${ITK_CMAKE_DIR}/Wrapping/CMakeLists.txt")
      add_subdirectory("${ITK_CMAKE_DIR}/Wrapping"
        ${CMAKE_CURRENT_BINARY_DIR}/Wrapping)
    else()
      message(FATAL_ERROR "Could not find wrapping infrastructure.")
    endif()
  endif()
endif()
# Create target to download data from the ITKData group.  This must come after
# all tests have been added that reference the group, so we put it last.
if(NOT TARGET ITKData)
  include(ExternalData)
  ExternalData_Add_Target(ITKData)
endif()
