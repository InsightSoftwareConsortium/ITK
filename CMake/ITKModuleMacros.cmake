get_filename_component(_ITKModuleMacros_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)

set(_ITKModuleMacros_DEFAULT_LABEL "ITKModular")

include(${_ITKModuleMacros_DIR}/ITKModuleAPI.cmake)
include(${_ITKModuleMacros_DIR}/ITKModuleDoxygen.cmake)
include(${_ITKModuleMacros_DIR}/ITKModuleHeaderTest.cmake)
include(${_ITKModuleMacros_DIR}/ITKModuleKWStyleTest.cmake)
include(${_ITKModuleMacros_DIR}/CppcheckTargets.cmake)
include(${_ITKModuleMacros_DIR}/ITKModuleCPPCheckTest.cmake)

# With Apple's (GGC <=4.2 and LLVM-GCC <=4.2) or (Clang < 3.2)
# visibility of template  don't work. Set the option to off and hide it.
if(APPLE AND ((CMAKE_COMPILER_IS_GNUCXX AND CMAKE_CXX_COMPILER_VERSION  VERSION_LESS "4.3")
   OR ((CMAKE_CXX_COMPILER_ID MATCHES "Clang") AND CMAKE_CXX_COMPILER_VERSION  VERSION_LESS "3.2")))
  set( USE_COMPILER_HIDDEN_VISIBILITY OFF CACHE INTERNAL "" )
endif()
include(GenerateExportHeader)

# itk_module(<name>)
#
# Main function for declaring an ITK module, usually in an itk-module.cmake file
# in the module search path. The module name is the only required argument, all
# others are optional named arguments that will be outlined below.
# The following named options take one (or more) arguments, such as the names of
# dependent modules:
#  DEPENDS = Modules that will be publicly linked to this module
#  PRIVATE_DEPENDS = Modules that will be privately linked to this module
#  COMPILE_DEPENDS = Modules that are needed at compile time by this module
#  TEST_DEPENDS = Modules that are needed by this modules testing executables
#  DESCRIPTION = Free text description of the module
#  FACTORY_NAMES = List of <factories>::<formats> to register
#
# The following options take no arguments:
#  EXCLUDE_FROM_DEFAULT = Exclude this module from the build default modules flag
#  EXCLUDE_FROM_ALL = (depreciated) Exclude this module from the build all modules flag
#  ENABLE_SHARED = Build this module as a shared library if the build shared libraries flag is set
#
# This macro will ensure the module name is compliant, and set the appropriate
# module variables as declared in the itk-module.cmake file.
#
# Note on dependency types:
#  Public vs. Private Dependencies: Public dependencies are added to the modules
#  INTERFACE_LINK_LIBRARIES which is a list of transitive link dependencies.
#  When this module is linked to by another target the libraries listed (and
#  recursively their link interface libraries) will be provided to the target
#  also.  Private dependencies are linked to by this module, but not
#  added to INTERFACE_LINK_LIBRARIES.
#
#  Compile dependencies: Compile Dependencies are added to CMake's list of
#  dependencies for the current module ensuring that they are built before the
#  current module, but will not be linked either publicly or privately, they are
#  only used to support the building of the current module.

macro(itk_module _name)
  itk_module_check_name(${_name})
  set(itk-module ${_name})
  set(itk-module-test ${_name}-Test)
  set(_doing "")
  set(ITK_MODULE_${itk-module}_DECLARED 1)
  set(ITK_MODULE_${itk-module-test}_DECLARED 1)
  set(ITK_MODULE_${itk-module}_DEPENDS "")
  set(ITK_MODULE_${itk-module}_COMPILE_DEPENDS "")
  set(ITK_MODULE_${itk-module}_PRIVATE_DEPENDS "")
  set(ITK_MODULE_${itk-module}_FACTORY_NAMES "")
  set(ITK_MODULE_${itk-module-test}_DEPENDS "${itk-module}")
  set(ITK_MODULE_${itk-module}_DESCRIPTION "description")
  set(ITK_MODULE_${itk-module}_EXCLUDE_FROM_DEFAULT 0)
  set(ITK_MODULE_${itk-module}_ENABLE_SHARED 0)
  foreach(arg ${ARGN})
    ### Parse itk_module named options
    if("${arg}" MATCHES "^((|COMPILE_|PRIVATE_|TEST_|)DEPENDS|DESCRIPTION|DEFAULT|FACTORY_NAMES)$")
      set(_doing "${arg}")
    elseif("${arg}" MATCHES "^EXCLUDE_FROM_DEFAULT$")
      set(_doing "")
      set(ITK_MODULE_${itk-module}_EXCLUDE_FROM_DEFAULT 1)
    elseif("${arg}" MATCHES "^EXCLUDE_FROM_ALL$") # To maintain backward compatibility
      set(_doing "")
      message(AUTHOR_WARNING "EXCLUDE_FROM_ALL is deprecated, please use EXCLUDE_FROM_DEFAULT.")
      set(ITK_MODULE_${itk-module}_EXCLUDE_FROM_DEFAULT 1)
    elseif("${arg}" MATCHES "^ENABLE_SHARED$")
      set(_doing "")
      set(ITK_MODULE_${itk-module}_ENABLE_SHARED 1)
    ### Parse named option parameters
    elseif("${_doing}" MATCHES "^DEPENDS$")
      list(APPEND ITK_MODULE_${itk-module}_DEPENDS "${arg}")
    elseif("${_doing}" MATCHES "^TEST_DEPENDS$")
      list(APPEND ITK_MODULE_${itk-module-test}_DEPENDS "${arg}")
    elseif("${_doing}" MATCHES "^COMPILE_DEPENDS$")
      list(APPEND ITK_MODULE_${itk-module}_COMPILE_DEPENDS "${arg}")
    elseif("${_doing}" MATCHES "^PRIVATE_DEPENDS$")
      list(APPEND ITK_MODULE_${itk-module}_PRIVATE_DEPENDS "${arg}")
    elseif("${_doing}" MATCHES "^FACTORY_NAMES$")
      list(APPEND ITK_MODULE_${itk-module}_FACTORY_NAMES "${arg}")
    elseif("${_doing}" MATCHES "^DESCRIPTION$")
      set(_doing "")
      set(ITK_MODULE_${itk-module}_DESCRIPTION "${arg}")
    elseif("${_doing}" MATCHES "^DEFAULT")
      message(FATAL_ERROR "Invalid argument [DEFAULT]")
    else()
      set(_doing "")
      message(AUTHOR_WARNING "Unknown argument [${arg}]")
    endif()
  endforeach()
  list(SORT ITK_MODULE_${itk-module}_DEPENDS) # Deterministic order.
  set(ITK_MODULE_${itk-module}_PUBLIC_DEPENDS ${ITK_MODULE_${itk-module}_DEPENDS} )
  list(APPEND ITK_MODULE_${itk-module}_DEPENDS
    ${ITK_MODULE_${itk-module}_COMPILE_DEPENDS}
    ${ITK_MODULE_${itk-module}_PRIVATE_DEPENDS}
  )
  set(ITK_MODULE_${itk-module}_TRANSITIVE_DEPENDS
    ${ITK_MODULE_${itk-module}_PUBLIC_DEPENDS}
    ${ITK_MODULE_${itk-module}_COMPILE_DEPENDS}
  )
  unset(ITK_MODULE_${itk-module}_COMPILE_DEPENDS)
  list(SORT ITK_MODULE_${itk-module}_DEPENDS) # Deterministic order.
  if(ITK_MODULE_${itk-module}_TRANSITIVE_DEPENDS) # Don't sort an empty list
    list(SORT ITK_MODULE_${itk-module}_TRANSITIVE_DEPENDS) # Deterministic order.
  endif()
  list(SORT ITK_MODULE_${itk-module}_PRIVATE_DEPENDS) # Deterministic order.
  list(SORT ITK_MODULE_${itk-module-test}_DEPENDS) # Deterministic order.
  if(ITK_MODULE_${itk-module}_FACTORY_NAMES) # Don't sort an empty list
    list(SORT ITK_MODULE_${itk-module}_FACTORY_NAMES) # Deterministic order.
  endif()
endmacro()

macro(itk_module_check_name _name)
  if( NOT "${_name}" MATCHES "^[a-zA-Z][a-zA-Z0-9]*$")
    message(FATAL_ERROR "Invalid module name: ${_name}")
  endif()
endmacro()

macro(itk_module_impl)
  include(itk-module.cmake) # Load module meta-data
  set(${itk-module}_INSTALL_RUNTIME_DIR ${ITK_INSTALL_RUNTIME_DIR})
  set(${itk-module}_INSTALL_LIBRARY_DIR ${ITK_INSTALL_LIBRARY_DIR})
  set(${itk-module}_INSTALL_ARCHIVE_DIR ${ITK_INSTALL_ARCHIVE_DIR})
  set(${itk-module}_INSTALL_INCLUDE_DIR ${ITK_INSTALL_INCLUDE_DIR})

  # Collect all sources and headers for IDE projects.
  set(_srcs "")
  if("${CMAKE_GENERATOR}" MATCHES "Xcode|Visual Studio|KDevelop"
      OR CMAKE_EXTRA_GENERATOR)
    # Add sources to the module target for easy editing in the IDE.
    set(_include ${${itk-module}_SOURCE_DIR}/include)
    if(EXISTS ${_include})
      set(_src ${${itk-module}_SOURCE_DIR}/src)
      file(GLOB_RECURSE _srcs ${_src}/*.cxx)
      file(GLOB_RECURSE _hdrs ${_include}/*.h ${_include}/*.hxx)
      list(APPEND _srcs ${_hdrs})
    endif()
  endif()

  # Create a ${itk-module}-all target to build the whole module.
  add_custom_target(${itk-module}-all ALL SOURCES ${_srcs})

  itk_module_use(${ITK_MODULE_${itk-module}_DEPENDS})

  if(NOT DEFINED ${itk-module}_LIBRARIES)
    set(${itk-module}_LIBRARIES "")
    foreach(dep IN LISTS ITK_MODULE_${itk-module}_DEPENDS)
      list(APPEND ${itk-module}_LIBRARIES "${${dep}_LIBRARIES}")
    endforeach()
    if(${itk-module}_LIBRARIES)
      list(REMOVE_DUPLICATES ${itk-module}_LIBRARIES)
    endif()
  endif()

  if(EXISTS ${${itk-module}_SOURCE_DIR}/include)
    list(APPEND ${itk-module}_INCLUDE_DIRS ${${itk-module}_SOURCE_DIR}/include)
    install(DIRECTORY include/ DESTINATION ${${itk-module}_INSTALL_INCLUDE_DIR} COMPONENT Development)
  endif()
  if(NOT ITK_SOURCE_DIR AND ${itk-module}_ENABLE_SHARED)
    # When building a module outside the ITK source tree, if ENABLE_SHARED is enabled,
    # find the export header.
    list(APPEND ${itk-module}_INCLUDE_DIRS ${${itk-module}_BINARY_DIR}/include)
  endif()

  if(${itk-module}_INCLUDE_DIRS)
    include_directories(${${itk-module}_INCLUDE_DIRS})
  endif()
  if(${itk-module}_SYSTEM_INCLUDE_DIRS)
    include_directories(${${itk-module}_SYSTEM_INCLUDE_DIRS})
  endif()

  if(${itk-module}_SYSTEM_LIBRARY_DIRS)
    link_directories(${${itk-module}_SYSTEM_LIBRARY_DIRS})
  endif()

  if(${itk-module}_THIRD_PARTY)
    itk_module_warnings_disable(C CXX)
  else()
    if(ITK_USE_KWSTYLE)
      itk_module_kwstyle_test( ${itk-module} )
    endif()
    if(ITK_CPPCHECK_TEST)
      itk_module_cppcheck_test( ${itk-module} )
    endif()
    if(EXISTS "${${itk-module}_SOURCE_DIR}/include" AND BUILD_TESTING)
      if("${itk-module}" MATCHES ITKGPU)
        if(${ITK_USE_GPU})
          itk_module_headertest(${itk-module})
        endif()
      else()
        itk_module_headertest(${itk-module})
      endif()
    endif()
  endif()

  if(EXISTS ${${itk-module}_SOURCE_DIR}/src/CMakeLists.txt AND NOT ${itk-module}_NO_SRC)
    set_property(GLOBAL APPEND PROPERTY ITKTargets_MODULES ${itk-module})
    add_subdirectory(src)
  endif()

    # Target ${itk-module} may not exist if the module only contains header files
  if(TARGET ${itk-module})
    if( ITK_MODULE_${itk-module}_ENABLE_SHARED )
      if(ITK_SOURCE_DIR)
        set(_export_header_file "${ITKCommon_BINARY_DIR}/${itk-module}Export.h")
      else()
        set(_export_header_file "${${itk-module}_BINARY_DIR}/include/${itk-module}Export.h")
      endif()

      # Generate the export macro header for symbol visibility/Windows DLL declspec
      generate_export_header(${itk-module}
        EXPORT_FILE_NAME ${_export_header_file}
        EXPORT_MACRO_NAME ${itk-module}_EXPORT
        NO_EXPORT_MACRO_NAME ${itk-module}_HIDDEN
        STATIC_DEFINE ITK_STATIC )
      install(FILES
        ${_export_header_file}
        DESTINATION ${${itk-module}_INSTALL_INCLUDE_DIR}
        COMPONENT Development
        )
    endif()
    if( (ITK_MODULE_${itk-module}_ENABLE_SHARED AND BUILD_SHARED_LIBS) OR (APPLE AND NOT BUILD_SHARED_LIBS) )
      if (USE_COMPILER_HIDDEN_VISIBILITY)
        # Prefer to use target properties supported by newer cmake
        set_target_properties(${itk-module} PROPERTIES CXX_VISIBILITY_PRESET hidden)
        set_target_properties(${itk-module} PROPERTIES C_VISIBILITY_PRESET hidden)
        set_target_properties(${itk-module} PROPERTIES VISIBILITY_INLINES_HIDDEN 1)
      endif()
    endif()
  endif()

  set(itk-module-EXPORT_CODE-build "${${itk-module}_EXPORT_CODE_BUILD}")
  set(itk-module-EXPORT_CODE-install "${${itk-module}_EXPORT_CODE_INSTALL}")
  if(ITK_SOURCE_DIR)
    # Uses ITKTargets.cmake
    set(itk-module-TARGETS_FILE-build "")
    set(itk-module-TARGETS_FILE-install "")
  else()
    set(itk-module-TARGETS_FILE-build "${${itk-module}_TARGETS_FILE_BUILD}")
    set(itk-module-TARGETS_FILE-install "${${itk-module}_TARGETS_FILE_INSTALL}")
  endif()

  set(itk-module-ENABLE_SHARED "${ITK_MODULE_${itk-module}_ENABLE_SHARED}")
  set(itk-module-DEPENDS "${ITK_MODULE_${itk-module}_DEPENDS}")
  set(itk-module-PUBLIC_DEPENDS "${ITK_MODULE_${itk-module}_PUBLIC_DEPENDS}")
  set(itk-module-TRANSITIVE_DEPENDS "${ITK_MODULE_${itk-module}_TRANSITIVE_DEPENDS}")
  set(itk-module-PRIVATE_DEPENDS "${ITK_MODULE_${itk-module}_PRIVATE_DEPENDS}")
  set(itk-module-FACTORY_NAMES "${ITK_MODULE_${itk-module}_FACTORY_NAMES}")
  set(itk-module-LIBRARIES "${${itk-module}_LIBRARIES}")
  set(itk-module-INCLUDE_DIRS-build "${${itk-module}_INCLUDE_DIRS}")
  set(itk-module-INCLUDE_DIRS-install "\${ITK_INSTALL_PREFIX}/${${itk-module}_INSTALL_INCLUDE_DIR}")
  if(${itk-module}_SYSTEM_INCLUDE_DIRS)
    list(APPEND itk-module-INCLUDE_DIRS-build "${${itk-module}_SYSTEM_INCLUDE_DIRS}")
    list(APPEND itk-module-INCLUDE_DIRS-install "${${itk-module}_SYSTEM_INCLUDE_DIRS}")
  endif()
  if(WIN32)
    set(itk-module-RUNTIME_LIBRARY_DIRS-build "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}")
    set(itk-module-RUNTIME_LIBRARY_DIRS-install "\${ITK_INSTALL_PREFIX}/${ITK_INSTALL_RUNTIME_DIR}")
  else()
    set(itk-module-RUNTIME_LIBRARY_DIRS-build "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}")
    set(itk-module-RUNTIME_LIBRARY_DIRS-install "\${ITK_INSTALL_PREFIX}/${ITK_INSTALL_LIBRARY_DIR}")
  endif()
  set(itk-module-LIBRARY_DIRS "${${itk-module}_SYSTEM_LIBRARY_DIRS}")
  set(itk-module-RUNTIME_LIBRARY_DIRS "${itk-module-RUNTIME_LIBRARY_DIRS-build}")
  set(itk-module-INCLUDE_DIRS "${itk-module-INCLUDE_DIRS-build}")
  set(itk-module-EXPORT_CODE "${itk-module-EXPORT_CODE-build}")
  set(itk-module-TARGETS_FILE "${itk-module-TARGETS_FILE-build}")
  configure_file(${_ITKModuleMacros_DIR}/ITKModuleInfo.cmake.in ${ITK_MODULES_DIR}/${itk-module}.cmake @ONLY)
  set(itk-module-INCLUDE_DIRS "${itk-module-INCLUDE_DIRS-install}")
  set(itk-module-EXPORT_CODE "${itk-module-EXPORT_CODE-install}")
  set(itk-module-TARGETS_FILE "${itk-module-TARGETS_FILE-install}")
  set(itk-module-RUNTIME_LIBRARY_DIRS "${itk-module-RUNTIME_LIBRARY_DIRS-install}")
  configure_file(${_ITKModuleMacros_DIR}/ITKModuleInfo.cmake.in CMakeFiles/${itk-module}.cmake @ONLY)
  install(FILES
    ${${itk-module}_BINARY_DIR}/CMakeFiles/${itk-module}.cmake
    DESTINATION ${ITK_INSTALL_PACKAGE_DIR}/Modules
    COMPONENT Development
    )
  itk_module_doxygen(${itk-module})   # module name
endmacro()

# itk_module_link_dependencies()
#
# Macro for linking to modules dependencies. Links this module to every
# dependency given to itk_module either publicly or privately.
macro(itk_module_link_dependencies)
  # link to public dependencies
  foreach(dep IN LISTS ITK_MODULE_${itk-module}_PUBLIC_DEPENDS)
    if(DEFINED ${dep}_LIBRARIES)
      target_link_libraries(${itk-module} LINK_PUBLIC ${${dep}_LIBRARIES})
    elseif(DEFINED ${dep})
      target_link_libraries(${itk-module} LINK_PUBLIC ${${dep}})
    else()
      message(FATAL_ERROR "Dependency \"${dep}\" not found: could not find [${dep}] or [${dep}_LIBRARIES]")
    endif()
  endforeach()

  # link to private dependencies
  foreach(dep IN LISTS ITK_MODULE_${itk-module}_PRIVATE_DEPENDS)
    if(DEFINED ${dep}_LIBRARIES)
      target_link_libraries(${itk-module} LINK_PRIVATE ${${dep}_LIBRARIES})
    elseif(DEFINED ${dep})
      target_link_libraries(${itk-module} LINK_PRIVATE ${${dep}})
    else()
      message(FATAL_ERROR "Dependency \"${dep}\" not found: could not find [${dep}] or [${dep}_LIBRARIES]")
    endif()
  endforeach()
endmacro()

macro(itk_module_test)
  include(../itk-module.cmake) # Load module meta-data
  set(${itk-module-test}_LIBRARIES "")
  itk_module_use(${ITK_MODULE_${itk-module-test}_DEPENDS})
  foreach(dep IN LISTS ITK_MODULE_${itk-module-test}_DEPENDS)
    list(APPEND ${itk-module-test}_LIBRARIES "${${dep}_LIBRARIES}")
  endforeach()
endmacro()

macro(itk_module_examples)
  cmake_dependent_option(Module_${itk-module}_BUILD_EXAMPLES "Build the examples" OFF "BUILD_EXAMPLES" OFF)
  if(Module_${itk-module}_BUILD_EXAMPLES)
    if(ITK_SOURCE_DIR)
      # If configuration is done from within ITK,
      # point to internal ITKConfig.cmake
      set(ITK_DIR ${ITK_BINARY_DIR}/CMakeTmp)
    endif()
    # Adds example subdirectory
    add_subdirectory( examples )
    if(ITK_SOURCE_DIR)
      # Cleanup ITK_DIR variable that is currently pointing to the directory
      # containing the internal version of "ITKConfig.cmake". The clean-up could
      # be done directly inside ITKInternalConfig.cmake but it would not be
      # obvious that it is done and therefore the CMake code would not be easy to read.
      unset(ITK_DIR)
    endif()
  endif()
endmacro()

macro(itk_module_warnings_disable)
  foreach(lang ${ARGN})
    if(MSVC)
      string(REGEX REPLACE "(^| )[/-]W[0-4]( |$)" " "
        CMAKE_${lang}_FLAGS "${CMAKE_${lang}_FLAGS}")
      set(CMAKE_${lang}_FLAGS "${CMAKE_${lang}_FLAGS} /W0")
    elseif(BORLAND)
      set(CMAKE_${lang}_FLAGS "${CMAKE_${lang}_FLAGS} -w-")
    else()
      set(CMAKE_${lang}_FLAGS "${CMAKE_${lang}_FLAGS} -w")
    endif()
  endforeach()
endmacro()

macro(itk_module_target_label _target_name)
  if(itk-module)
    set(_label ${itk-module})
    if(TARGET ${itk-module}-all)
      add_dependencies(${itk-module}-all ${_target_name})
    endif()
  else()
    set(_label ${_ITKModuleMacros_DEFAULT_LABEL})
  endif()
  set_property(TARGET ${_target_name} PROPERTY LABELS ${_label})
endmacro()

macro(itk_module_target_name _name)
  set_property(TARGET ${_name} PROPERTY VERSION 1)
  set_property(TARGET ${_name} PROPERTY SOVERSION 1)
  if("${_name}" MATCHES "^[Ii][Tt][Kk]")
    set(_itk "")
  else()
    set(_itk "itk")
  endif()
  # Support custom library suffix names, for other projects wanting to inject
  # their own version numbers etc.
  if(DEFINED ITK_CUSTOM_LIBRARY_SUFFIX)
    set(_lib_suffix "${ITK_CUSTOM_LIBRARY_SUFFIX}")
  else()
    set(_lib_suffix "-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}")
  endif()
  set_property(TARGET ${_name} PROPERTY OUTPUT_NAME ${_itk}${_name}${_lib_suffix})
endmacro()

macro(itk_module_target_export _name)
  export(TARGETS ${_name} APPEND FILE ${${itk-module}-targets-build})
endmacro()

macro(itk_module_target_install _name)
  #Use specific runtime components for executables and libraries separately when installing a module,
  #considering that the target of a module could be either an executable or a library.
  get_property(_ttype TARGET ${_name} PROPERTY TYPE)
  if("${_ttype}" STREQUAL EXECUTABLE)
    set(runtime_component Runtime)
  else()
    set(runtime_component RuntimeLibraries)
  endif()
  install(TARGETS ${_name}
    EXPORT  ${${itk-module}-targets}
    RUNTIME DESTINATION ${${itk-module}_INSTALL_RUNTIME_DIR} COMPONENT ${runtime_component}
    LIBRARY DESTINATION ${${itk-module}_INSTALL_LIBRARY_DIR} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${${itk-module}_INSTALL_ARCHIVE_DIR} COMPONENT Development
    )
endmacro()

macro(itk_module_target _name)
  set(_install 1)
  foreach(arg ${ARGN})
    if("${arg}" MATCHES "^(NO_INSTALL)$")
      set(_install 0)
    else()
      message(FATAL_ERROR "Unknown argument [${arg}]")
    endif()
  endforeach()
  itk_module_target_name(${_name})
  itk_module_target_label(${_name})
  itk_module_target_export(${_name})
  if(_install)
    itk_module_target_install(${_name})
  endif()
endmacro()

# itk_module_add_library(_name LibrarySource1 LibrarySource2 ... LibrarySourceN)
#
# This macro is used to add a library in ITK modules. A typical module
# src/CMakeLists.txt will have contents like:
#
# set(MyModule_SRCS
#   itkClass1.cxx
#   itkClass2.cxx
#   )
#
# itk_module_add_library(MyModule ${ModuleModule_SRCS})
#
macro(itk_module_add_library _name)
  set(_LIBRARY_BUILD_TYPE "${ITK_LIBRARY_BUILD_TYPE}")
  # If ENABLE_SHARED is not specified in the itk_module macro, then
  # there is no export specification generated for the library. In
  # such a case we fall back to the CMake default, instead of the ITK
  # type.
  if(NOT ITK_MODULE_${itk-module}_ENABLE_SHARED)
    set(_LIBRARY_BUILD_TYPE)
  endif()
  add_library(${_name} ${_LIBRARY_BUILD_TYPE} ${ARGN})
  itk_module_link_dependencies()
  itk_module_target(${_name})
endmacro()
