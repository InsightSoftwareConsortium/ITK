# This file ensures the appropriate variables are set up for a project extending
# ITK before including ITKModuleMacros. This is the preferred way to build an
# ITK module outside of the ITK source tree.

if(NOT ITK_FOUND)
  message(FATAL_ERROR "ITK must be found before module macros can be used.")
endif()
if(NOT ITK_VERSION VERSION_GREATER "4.8")
  message(FATAL_ERROR "Requires ITK 4.9 or later to work.")
endif()
if(NOT EXISTS ${ITK_CMAKE_DIR}/ITKModuleMacros.cmake)
  message(FATAL_ERROR "Modules can only be built against an ITK build tree; they cannot be built against an ITK install tree.")
endif()

# Setup build locations.
if(NOT CMAKE_RUNTIME_OUTPUT_DIRECTORY)
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ITK_DIR}/bin)
endif()
if(NOT CMAKE_LIBRARY_OUTPUT_DIRECTORY)
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${ITK_DIR}/lib)
endif()
if(NOT CMAKE_ARCHIVE_OUTPUT_DIRECTORY)
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${ITK_DIR}/lib)
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

# Use ITK's flags.
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${ITK_REQUIRED_C_FLAGS}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${ITK_REQUIRED_CXX_FLAGS}")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
option(BUILD_SHARED_LIBS "Build ITK with shared libraries." ${ITK_BUILD_SHARED})

# Add the ITK_MODULES_DIR to the CMAKE_MODULE_PATH and then use the binary
# directory for the project to write out new ones to.
if(ITK_MODULES_DIR)
  set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${ITK_MODULES_DIR})
endif()
set(ITK_MODULES_DIR "${ITK_DIR}/${ITK_INSTALL_PACKAGE_DIR}/Modules")

include(CTest)
include(ITKExternalData)
include(ITKModuleTest)

include(ITKModuleMacros)
include(itk-module.cmake)
set(${itk-module}-targets ${itk-module}Targets)
set(${itk-module}-targets-install "\${ITK_INSTALL_PREFIX}/${ITK_INSTALL_PACKAGE_DIR}/${itk-module}Targets.cmake")
set(${itk-module}_TARGETS_FILE_INSTALL "${${itk-module}-targets-install}")
set(${itk-module}-targets-build "${ITK_DIR}/${ITK_INSTALL_PACKAGE_DIR}/Modules/${itk-module}Targets.cmake")
set(${itk-module}_TARGETS_FILE_BUILD "${${itk-module}-targets-build}")
itk_module_impl()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/src/CMakeLists.txt AND NOT ${itk-module}_NO_SRC AND "${${itk-module}-targets}")
  install(EXPORT ${${itk-module}-targets} DESTINATION "${ITK_INSTALL_PACKAGE_DIR}/Modules"
          COMPONENT Development)
endif()

set(ITK_TEST_OUTPUT_DIR "${CMAKE_BINARY_DIR}/Testing/Temporary")
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/test/CMakeLists.txt")
  add_subdirectory(test)
endif()

option(ITK_WRAP_PYTHON "Build Python support" OFF)
option(ITK_WRAP_JAVA "Build Java support (Currently not supported)" OFF)
option(ITK_WRAP_RUBY "Build Ruby support (Currently not supported)" OFF)
option(ITK_WRAP_PERL "Build Perl support (Currently not supported)" OFF)
option(ITK_WRAP_TCL "Build Tcl support (Currently not supported)" OFF)
option(ITK_WRAP_EXPLICIT "Build explicit instantiation support (Currently not supported)" OFF)
option(ITK_WRAP_DOC "Build swig Doc wrapper support (Currently not supported in external builds)" OFF)
option(ITK_WRAP_DOC_MAN "Generate unix manual pages (Currently not supported in external builds)" OFF)
mark_as_advanced(
  ITK_WRAP_CASTXML
  ITK_WRAP_SWIG_INTERFACE
  ITK_WRAP_JAVA
  ITK_WRAP_RUBY
  ITK_WRAP_PERL
  ITK_WRAP_TCL
  ITK_WRAP_EXPLICIT
  )
if(ITK_WRAPPING AND EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/wrapping/CMakeLists.txt")
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

# Create target to download data from the ITKData group.  This must come after
# all tests have been added that reference the group, so we put it last.
if(NOT TARGET ITKData)
  ExternalData_Add_Target(ITKData)
endif()
