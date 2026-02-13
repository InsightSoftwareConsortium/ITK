get_filename_component(_ITKModuleMacros_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)

set(_ITKModuleMacros_DEFAULT_LABEL "ITKModular")

include(${_ITKModuleMacros_DIR}/ITKModuleAPI.cmake)
include(${_ITKModuleMacros_DIR}/ITKModuleDoxygen.cmake)
include(${_ITKModuleMacros_DIR}/ITKModuleHeaderTest.cmake)
include(${_ITKModuleMacros_DIR}/ITKModuleKWStyleTest.cmake)
include(${_ITKModuleMacros_DIR}/CppcheckTargets.cmake)
include(${_ITKModuleMacros_DIR}/ITKModuleCPPCheckTest.cmake)
include(${_ITKModuleMacros_DIR}/ITKFactoryRegistration.cmake)

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

  # Note: All modules have the same namespace as initially configured in ITK
  set(ITK_MODULE_${itk-module}_TARGETS_NAMESPACE "")
  if(ITK_LIBRARY_NAMESPACE)
    set(ITK_MODULE_${itk-module}_TARGETS_NAMESPACE "${ITK_LIBRARY_NAMESPACE}::")
  endif()
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
    if(
      "${arg}"
        MATCHES
        "^((|COMPILE_|PRIVATE_|TEST_|)DEPENDS|DESCRIPTION|DEFAULT|FACTORY_NAMES)$"
    )
      set(_doing "${arg}")
    elseif("${arg}" MATCHES "^EXCLUDE_FROM_DEFAULT$")
      set(_doing "")
      set(ITK_MODULE_${itk-module}_EXCLUDE_FROM_DEFAULT 1)
    elseif("${arg}" MATCHES "^EXCLUDE_FROM_ALL$") # To maintain backward compatibility
      set(_doing "")
      message(
        AUTHOR_WARNING
        "EXCLUDE_FROM_ALL is deprecated, please use EXCLUDE_FROM_DEFAULT."
      )
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
  set(
    ITK_MODULE_${itk-module}_PUBLIC_DEPENDS
    ${ITK_MODULE_${itk-module}_DEPENDS}
  )
  list(
    APPEND
    ITK_MODULE_${itk-module}_DEPENDS
    ${ITK_MODULE_${itk-module}_COMPILE_DEPENDS}
    ${ITK_MODULE_${itk-module}_PRIVATE_DEPENDS}
  )
  set(
    ITK_MODULE_${itk-module}_TRANSITIVE_DEPENDS
    ${ITK_MODULE_${itk-module}_PUBLIC_DEPENDS}
    ${ITK_MODULE_${itk-module}_COMPILE_DEPENDS}
  )
  unset(ITK_MODULE_${itk-module}_COMPILE_DEPENDS)
  list(SORT ITK_MODULE_${itk-module}_DEPENDS) # Deterministic order.
  list(SORT ITK_MODULE_${itk-module}_TRANSITIVE_DEPENDS) # Deterministic order.
  list(SORT ITK_MODULE_${itk-module}_PRIVATE_DEPENDS) # Deterministic order.
  list(SORT ITK_MODULE_${itk-module-test}_DEPENDS) # Deterministic order.
  list(SORT ITK_MODULE_${itk-module}_FACTORY_NAMES) # Deterministic order.
endmacro()

macro(itk_module_check_name _name)
  if(NOT "${_name}" MATCHES "^[a-zA-Z][a-zA-Z0-9]*$")
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
  if(
    "${CMAKE_GENERATOR}"
      MATCHES
      "Xcode|Visual Studio|KDevelop"
    OR
      CMAKE_EXTRA_GENERATOR
  )
    # Add sources to the module target for easy editing in the IDE.
    set(_include ${${itk-module}_SOURCE_DIR}/include)
    if(EXISTS ${_include})
      set(_src ${${itk-module}_SOURCE_DIR}/src)
      file(GLOB_RECURSE _srcs ${_src}/*.cxx)
      file(
        GLOB_RECURSE _hdrs
        ${_include}/*.h
        ${_include}/*.hxx
      )
      list(APPEND _srcs ${_hdrs})
    endif()
  endif()

  # Create a ${itk-module}-all target to build the whole module.
  add_custom_target(${itk-module}-all ALL SOURCES ${_srcs})

  itk_module_use(${ITK_MODULE_${itk-module}_DEPENDS})

  # The ${itk-module}_LIBRARIES variable defined the libraries provided by this module.
  # Transitive dependencies of this module are provided through the
  # ${itk-module}Module interface library created below.
  if(DEFINED ${itk-module}_LIBRARIES)
    set(_libraries "")
    foreach(dep IN LISTS ${itk-module}_LIBRARIES)
      if(EXISTS "${dep}")
        # If we are linking to a file, use it directly
        message(DEBUG "Linking ${itk-module} to file dependency: ${dep}")
        list(APPEND _libraries "${dep}")
      elseif("${dep}" MATCHES "^(.*)::(.*)$")
        # If dep is already namespaced, use it directly
        message(DEBUG "Linking ${itk-module} to namespaced dependency: ${dep}")
        list(APPEND _libraries "${dep}")
      else()
        list(
          APPEND
          _libraries
          "${ITK_MODULE_${itk-module}_TARGETS_NAMESPACE}${dep}"
        )
        if(NOT ITK_MODULE_${itk-module}_TARGETS_NAMESPACE STREQUAL "")
          set(
            ${itk-module}_EXPORT_CODE_INSTALL
            "${${itk-module}_EXPORT_CODE_INSTALL}
add_library(${dep} INTERFACE IMPORTED)
set_target_properties(${dep}
  PROPERTIES
    INTERFACE_LINK_LIBRARIES ${ITK_MODULE_${itk-module}_TARGETS_NAMESPACE}${dep}
    DEPRECATION \"Use ${ITK_MODULE_${itk-module}_TARGETS_NAMESPACE}${dep} instead\"
)
"
          )
          set(
            ${itk-module}_EXPORT_CODE_BUILD
            "${${itk-module}_EXPORT_CODE_BUILD}
if(NOT TARGET ${dep})
  add_library(${dep} INTERFACE IMPORTED)
  set_target_properties(${dep}
    PROPERTIES
      INTERFACE_LINK_LIBRARIES ${ITK_MODULE_${itk-module}_TARGETS_NAMESPACE}${dep}
      DEPRECATION \"Use ${ITK_MODULE_${itk-module}_TARGETS_NAMESPACE}${dep} instead\"
  )
endif()
"
          )
        endif()
      endif()
    endforeach()
    set(${itk-module}_LIBRARIES "${_libraries}")
    list(REMOVE_DUPLICATES ${itk-module}_LIBRARIES)
  endif()

  if(EXISTS ${${itk-module}_SOURCE_DIR}/include)
    list(APPEND ${itk-module}_INCLUDE_DIRS ${${itk-module}_SOURCE_DIR}/include)
    install(
      DIRECTORY
        include/
      DESTINATION ${${itk-module}_INSTALL_INCLUDE_DIR}
      COMPONENT Development
    )
  endif()
  if(NOT ITK_SOURCE_DIR AND ${itk-module}_ENABLE_SHARED)
    # When building a module outside the ITK source tree, if ENABLE_SHARED is enabled,
    # find the export header.
    list(APPEND ${itk-module}_INCLUDE_DIRS ${${itk-module}_BINARY_DIR}/include)
  endif()

  # Prepare include directories with generator expressions for use in targets
  foreach(_dir ${${itk-module}_INCLUDE_DIRS})
    list(APPEND ${itk-module}_GENEX_INCLUDE_DIRS "$<BUILD_INTERFACE:${_dir}>")
  endforeach()
  list(
    APPEND
    ${itk-module}_GENEX_INCLUDE_DIRS
    "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/${${itk-module}_INSTALL_INCLUDE_DIR}>"
  )

  # System include directories are assumed to be external dependencies that are not installed,
  # and thus do not have separate install interface paths.
  if(${itk-module}_SYSTEM_INCLUDE_DIRS)
    foreach(_dir ${${itk-module}_SYSTEM_INCLUDE_DIRS})
      list(APPEND ${itk-module}_SYSTEM_GENEX_INCLUDE_DIRS ${_dir})
    endforeach()
  endif()

  if(${itk-module}_THIRD_PARTY)
    itk_module_warnings_disable(C CXX)
    if(${itk-module}_INCLUDE_DIRS)
      include_directories(${${itk-module}_INCLUDE_DIRS})
    endif()
    if(${itk-module}_SYSTEM_INCLUDE_DIRS)
      # _SYSTEM_INCLUDE_DIRS should searched after internal _INCLUDE_DIRS
      include_directories(AFTER ${${itk-module}_SYSTEM_INCLUDE_DIRS})
    endif()
  else()
    if(ITK_USE_KWSTYLE)
      itk_module_kwstyle_test(${itk-module})
    endif()
    if(ITK_CPPCHECK_TEST)
      itk_module_cppcheck_test(${itk-module})
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

  if(
    EXISTS
      ${${itk-module}_SOURCE_DIR}/src/CMakeLists.txt
    AND
      NOT
        ${itk-module}_NO_SRC
  )
    set_property(
      GLOBAL
      APPEND
      PROPERTY
        ITKTargets_MODULES
          ${itk-module}
    )
    add_subdirectory(src)
  endif()

  # Target ${itk-module} may not exist if the module only contains header files
  if(TARGET ${itk-module})
    if(ITK_MODULE_${itk-module}_ENABLE_SHARED)
      if(ITK_SOURCE_DIR)
        set(_export_header_file "${ITKCommon_BINARY_DIR}/${itk-module}Export.h")
      else()
        set(
          _export_header_file
          "${${itk-module}_BINARY_DIR}/include/${itk-module}Export.h"
        )
      endif()

      # Generate the export macro header for symbol visibility/Windows DLL declspec
      generate_export_header(
        ${itk-module}
        EXPORT_FILE_NAME ${_export_header_file}
        EXPORT_MACRO_NAME ${itk-module}_EXPORT
        NO_EXPORT_MACRO_NAME ${itk-module}_HIDDEN
        STATIC_DEFINE ITK_STATIC
      )
      install(
        FILES
          ${_export_header_file}
        DESTINATION ${${itk-module}_INSTALL_INCLUDE_DIR}
        COMPONENT Development
      )
    endif()
    if(
      (
        ITK_MODULE_${itk-module}_ENABLE_SHARED
        AND
          BUILD_SHARED_LIBS
      )
      OR
        (
          APPLE
          AND
            NOT
              BUILD_SHARED_LIBS
        )
    )
      if(USE_COMPILER_HIDDEN_VISIBILITY)
        # Prefer to use target properties supported by newer cmake
        set_target_properties(
          ${itk-module}
          PROPERTIES
            CXX_VISIBILITY_PRESET
              hidden
        )
        set_target_properties(
          ${itk-module}
          PROPERTIES
            C_VISIBILITY_PRESET
              hidden
        )
        set_target_properties(
          ${itk-module}
          PROPERTIES
            VISIBILITY_INLINES_HIDDEN
              1
        )
      endif()
    endif()
  endif()

  ####
  # Create ${itk-module}Module interface library for ITK Modules
  ####
  add_library(${itk-module}Module INTERFACE)

  target_link_libraries(
    ${itk-module}Module
    INTERFACE
      ${${itk-module}_LIBRARIES}
  )

  # Add include directories with generator expressions
  target_include_directories(
    ${itk-module}Module
    INTERFACE
      ${${itk-module}_GENEX_INCLUDE_DIRS}
  )
  target_include_directories(
    ${itk-module}Module
    SYSTEM
    INTERFACE
      ${${itk-module}_SYSTEM_GENEX_INCLUDE_DIRS}
  )

  # Link transitive dependencies (public + compile depends) through ${itk-module}Module interface
  foreach(dep IN LISTS ITK_MODULE_${itk-module}_TRANSITIVE_DEPENDS)
    target_link_libraries(
      ${itk-module}Module
      INTERFACE
        ${ITK_MODULE_${itk-module}_TARGETS_NAMESPACE}${dep}Module
    )
  endforeach()

  # Link this module to factory meta-module interfaces if it provides factories
  if(ITK_MODULE_${itk-module}_FACTORY_NAMES)
    foreach(_factory_format ${ITK_MODULE_${itk-module}_FACTORY_NAMES})
      # Extract factory name from <factory_name>::<format>
      string(
        REGEX
        REPLACE
        "^(.*)::(.*)$"
        "\\1"
        _factory_name
        "${_factory_format}"
      )
      set(_meta_module ITK${_factory_name})

      if(NOT TARGET ${_meta_module})
        # When building a module outside ITK source tree, the factory meta-module may not be defined yet.
        message(
          STATUS
          "Not adding ${itk-module}Module(${_factory_format}) to factory meta-module ${_meta_module} because it is outside ITK source tree."
        )
      else()
        get_target_property(_is_imported ${_meta_module} IMPORTED)
        if(_is_imported)
          message(
            DEBUG
            "Not adding ${itk-module}Module(${_factory_format}) to factory meta-module ${_meta_module} because it is an imported target. "
          )
        else()
          # Check if this module is already linked to the factory meta-module
          set(
            _module_target
            ${ITK_MODULE_${itk-module}_TARGETS_NAMESPACE}${itk-module}Module
          )
          get_target_property(
            _existing_deps
            ${_meta_module}
            INTERFACE_LINK_LIBRARIES
          )
          if(_existing_deps AND "${_module_target}" IN_LIST _existing_deps)
            message(
              DEBUG
              "Module ${itk-module}Module is already linked to factory meta-module ${_meta_module}."
            )
          else()
            # Add this module to the factory meta-module
            target_link_libraries(${_meta_module} INTERFACE ${_module_target})
          endif()
        endif()
      endif()
    endforeach()
  endif()

  # Export and install the interface library
  itk_module_target_export(${itk-module}Module)
  itk_module_target_install(${itk-module}Module)
  ####
  # End ITK Modules interface library creation
  ####

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
  set(
    itk-module-TRANSITIVE_DEPENDS
    "${ITK_MODULE_${itk-module}_TRANSITIVE_DEPENDS}"
  )
  set(itk-module-PRIVATE_DEPENDS "${ITK_MODULE_${itk-module}_PRIVATE_DEPENDS}")
  set(itk-module-FACTORY_NAMES "${ITK_MODULE_${itk-module}_FACTORY_NAMES}")
  set(itk-module-LIBRARIES "${${itk-module}_LIBRARIES}")
  set(
    itk-module-INTERFACE_LIBRARY
    "${ITK_MODULE_${itk-module}_TARGETS_NAMESPACE}${itk-module}Module"
  )

  set(itk-module-INCLUDE_DIRS-build "${${itk-module}_INCLUDE_DIRS}")
  set(
    itk-module-INCLUDE_DIRS-install
    "\${ITK_INSTALL_PREFIX}/${${itk-module}_INSTALL_INCLUDE_DIR}"
  )
  if(${itk-module}_SYSTEM_INCLUDE_DIRS)
    list(
      APPEND
      itk-module-INCLUDE_DIRS-build
      "${${itk-module}_SYSTEM_INCLUDE_DIRS}"
    )
    list(
      APPEND
      itk-module-INCLUDE_DIRS-install
      "${${itk-module}_SYSTEM_INCLUDE_DIRS}"
    )
  endif()
  if(WIN32)
    set(
      itk-module-RUNTIME_LIBRARY_DIRS-build
      "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
    )
    set(
      itk-module-RUNTIME_LIBRARY_DIRS-install
      "\${ITK_INSTALL_PREFIX}/${ITK_INSTALL_RUNTIME_DIR}"
    )
  else()
    set(
      itk-module-RUNTIME_LIBRARY_DIRS-build
      "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
    )
    set(
      itk-module-RUNTIME_LIBRARY_DIRS-install
      "\${ITK_INSTALL_PREFIX}/${ITK_INSTALL_LIBRARY_DIR}"
    )
  endif()
  set(itk-module-LIBRARY_DIRS "${${itk-module}_SYSTEM_LIBRARY_DIRS}")
  set(
    itk-module-RUNTIME_LIBRARY_DIRS
    "${itk-module-RUNTIME_LIBRARY_DIRS-build}"
  )
  set(itk-module-INCLUDE_DIRS "${itk-module-INCLUDE_DIRS-build}")
  # set itk-module-GENEX_INCLUDE_DIRS so that includes both install interface and build interface path in appropriate generator expressions
  set(itk-module-GENEX_INCLUDE_DIRS "")
  foreach(_dir ${itk-module-INCLUDE_DIRS-build})
    list(APPEND itk-module-GENEX_INCLUDE_DIRS "$<BUILD_INTERFACE:${_dir}>")
  endforeach()

  set(itk-module-EXPORT_CODE "${itk-module-EXPORT_CODE-build}")
  set(itk-module-TARGETS_FILE "${itk-module-TARGETS_FILE-build}")
  configure_file(
    ${_ITKModuleMacros_DIR}/ITKModuleInfo.cmake.in
    ${ITK_MODULES_DIR}/${itk-module}.cmake
    @ONLY
  )
  set(itk-module-INCLUDE_DIRS "${itk-module-INCLUDE_DIRS-install}")
  set(itk-module-EXPORT_CODE "${itk-module-EXPORT_CODE-install}")
  set(itk-module-TARGETS_FILE "${itk-module-TARGETS_FILE-install}")
  set(
    itk-module-RUNTIME_LIBRARY_DIRS
    "${itk-module-RUNTIME_LIBRARY_DIRS-install}"
  )
  configure_file(
    ${_ITKModuleMacros_DIR}/ITKModuleInfo.cmake.in
    CMakeFiles/${itk-module}.cmake
    @ONLY
  )
  install(
    FILES
      ${${itk-module}_BINARY_DIR}/CMakeFiles/${itk-module}.cmake
    DESTINATION ${ITK_INSTALL_PACKAGE_DIR}/Modules
    COMPONENT Development
  )
  itk_module_doxygen(${itk-module}) # module name
endmacro()

# itk_module_link_dependencies()
#
# Macro for linking to modules dependencies. Links this module to every
# dependency given to itk_module either publicly or privately.
macro(itk_module_link_dependencies)
  # link to dependencies
  foreach(_link IN ITEMS PUBLIC INTERFACE PRIVATE)
    foreach(dep IN LISTS ITK_MODULE_${itk-module}_${_link}_DEPENDS)
      if(
        ITK_MODULE_${dep}_LOADED
        OR
          TARGET
            ${ITK_MODULE_${itk-module}_TARGETS_NAMESPACE}${dep}Module
      )
        target_link_libraries(
          ${itk-module}
          ${_link}
          ${ITK_MODULE_${itk-module}_TARGETS_NAMESPACE}${dep}Module
        )
      elseif(DEFINED ${dep})
        target_link_libraries(
          ${itk-module}
          ${_link}
          ${${dep}}
        )
      else()
        message(
          FATAL_ERROR
          "${_link} Dependency \"${dep}\" not found: could not find [${dep}] or [${ITK_MODULE_${itk-module}_TARGETS_NAMESPACE}${dep}Module]"
        )
      endif()
    endforeach()
  endforeach()
endmacro()

macro(itk_module_test)
  include(../itk-module.cmake) # Load module meta-data
  set(${itk-module-test}_LIBRARIES "")
  foreach(dep IN LISTS ITK_MODULE_${itk-module-test}_DEPENDS)
    itk_module_load("${dep}")
    list(
      APPEND
      ${itk-module-test}_LIBRARIES
      "${ITK_MODULE_${itk-module}_TARGETS_NAMESPACE}${dep}Module"
    )
  endforeach()
  set(ITK_TEST_OUTPUT_DIR "${ITK_TEST_OUTPUT_DIR}/${itk-module}")
  file(MAKE_DIRECTORY "${ITK_TEST_OUTPUT_DIR}")
endmacro()

macro(itk_module_examples)
  #Some modules have examples, and those should be hidden if the module is disabled, or examples are not requested
  cmake_dependent_option(
    Module_${itk-module}_BUILD_EXAMPLES
    "Build the examples for Module_${itk-module}"
    ON
    "BUILD_EXAMPLES OR ITK_BUILD_EXAMPLES;Module_${itk-module};NOT ITK_BUILD_DOCUMENTATION"
    OFF
  )
  if(Module_${itk-module}_BUILD_EXAMPLES)
    if(ITK_SOURCE_DIR)
      # If configuration is done from within ITK,
      # point to internal ITKConfig.cmake
      set(ITK_DIR ${ITK_BINARY_DIR}/CMakeTmp)
    else()
      # Ensure that executables get added to the current build tree instead of
      # ITK's build tree when building as an external module.
      if(CMAKE_RUNTIME_OUTPUT_DIRECTORY STREQUAL "${ITK_DIR}/bin")
        set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)
      endif()
    endif()
    # Adds example subdirectory
    add_subdirectory(examples)
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
      string(
        REGEX
        REPLACE
        "(^|)[/-]W[0-4]( |$)"
        " "
        CMAKE_${lang}_FLAGS
        "${CMAKE_${lang}_FLAGS}"
      )
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
  set_property(
    TARGET
      ${_target_name}
    PROPERTY
      LABELS
        ${_label}
  )
endmacro()

macro(itk_module_target_name _name)
  if(NOT ${CMAKE_SYSTEM_NAME} MATCHES "OpenBSD")
    set_property(
      TARGET
        ${_name}
      PROPERTY
        VERSION
          1
    )
    set_property(
      TARGET
        ${_name}
      PROPERTY
        SOVERSION
          1
    )
  endif()
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
  set_property(
    TARGET
      ${_name}
    PROPERTY
      OUTPUT_NAME
        ${_itk}${_name}${_lib_suffix}
  )
endmacro()

# itk_module_target_export(_name [NAMESPACE <namespace>])
#
# Macro for exporting a target from an ITK module to the build tree. This macro
# handles the export of targets to make them available to other ITK modules and
# external projects that use ITK.
#
# For library targets with a defined namespace (ITK_MODULE_${itk-module}_TARGETS_NAMESPACE),
# this macro creates an ALIAS target with the namespace prefix and sets the
# EXPORT_NAME property accordingly. This ensures consistent naming when the target
# is used through find_package(ITK).
#
# All targets are exported to the module's build tree targets file
# (${itk-module}-targets-build) for use during the build process.
#
# Arguments:
#   _name - The name of the target to export (typically ${itk-module} or ${itk-module}Module)
#   NAMESPACE - Optional namespace prefix. If not provided, uses ITK_MODULE_${itk-module}_TARGETS_NAMESPACE
macro(itk_module_target_export _name)
  cmake_parse_arguments(_export "" "NAMESPACE" "" ${ARGN})
  if(_export_UNPARSED_ARGUMENTS)
    message(FATAL_ERROR "Unknown arguments: ${_export_UNPARSED_ARGUMENTS}")
  endif()

  if(_export_NAMESPACE)
    set(_export_namespace "${_export_NAMESPACE}")
  else()
    set(_export_namespace "${ITK_MODULE_${itk-module}_TARGETS_NAMESPACE}")
  endif()

  get_property(_ttype TARGET ${_name} PROPERTY TYPE)
  if(
    _ttype
      MATCHES
      ".*_LIBRARY$"
    AND
      _export_namespace
    AND
      NOT
        ${_name}
          MATCHES
          "^(.*)::(.*)$"
  )
    if(NOT TARGET ${_export_namespace}${_name})
      add_library(${_export_namespace}${_name} ALIAS ${_name})
    endif()
    set_target_properties(
      ${_name}
      PROPERTIES
        EXPORT_NAME
          ${_export_namespace}${_name}
    )
  endif()
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

  # Add support to allow remote modules to use FILE_SET for headers, but only if CMake version is 3.23 or higher.
  if(CMAKE_VERSION VERSION_GREATER_EQUAL "3.23")
    set(
      _FILE_SET_INSTALL_ARGS
      FILE_SET
      HEADERS
      DESTINATION
      ${${itk-module}_INSTALL_INCLUDE_DIR}
      COMPONENT
      Development
    )
  else()
    get_target_property(_has_fileset ${_name} FILE_SET)
    # Issue error that this target does not support FILE_SET if _has_fileset is not defined
    if(_has_fileset)
      message(
        FATAL_ERROR
        "Target ${_name} has FILE_SET and requires CMake version is 3.23 or higher."
      )
    endif()
  endif()
  install(
    TARGETS
      ${_name}
    EXPORT ${${itk-module}-targets}
    RUNTIME
      DESTINATION ${${itk-module}_INSTALL_RUNTIME_DIR}
      COMPONENT ${runtime_component}
    LIBRARY
      DESTINATION ${${itk-module}_INSTALL_LIBRARY_DIR}
      COMPONENT RuntimeLibraries
    ARCHIVE
      DESTINATION ${${itk-module}_INSTALL_ARCHIVE_DIR}
      COMPONENT Development
    ${_FILE_SET_INSTALL_ARGS}
  )
endmacro()

# itk_module_target(_name [NO_INSTALL] [NAMESPACE <namespace>])
#
# Function for configuring, labeling, exporting, and installing a target from an ITK module.
# This is a convenience function that calls itk_module_target_name, itk_module_target_label,
# itk_module_target_export, and optionally itk_module_target_install for a given target.
#
# This function should be called for each library or executable target created in an ITK module
# to ensure proper naming conventions, labeling, export configuration, and installation.
#
# Arguments:
#   _name - The name of the target to configure (e.g., ${itk-module} for the main library)
#
# Options:
#   NO_INSTALL - If specified, the target will not be installed (useful for test executables)
#   NAMESPACE <namespace> - Optional namespace prefix for export. If not provided, uses
#                          ITK_MODULE_${itk-module}_TARGETS_NAMESPACE
#
# Example usage:
#   itk_module_target(${itk-module})                   # Standard library with installation
#   itk_module_target(MyTestExe NO_INSTALL)            # Test executable without installation
#   itk_module_target(MyLib NAMESPACE "Custom::")      # Library with custom namespace
function(itk_module_target _name)
  cmake_parse_arguments(_target "NO_INSTALL" "NAMESPACE" "" ${ARGN})
  if(_target_UNPARSED_ARGUMENTS)
    message(FATAL_ERROR "Unknown arguments: ${_target_UNPARSED_ARGUMENTS}")
  endif()

  itk_module_target_name(${_name})
  itk_module_target_label(${_name})
  if(_target_NAMESPACE)
    itk_module_target_export(${_name} NAMESPACE ${_target_NAMESPACE})
  else()
    itk_module_target_export(${_name})
  endif()
  if(NOT _target_NO_INSTALL)
    itk_module_target_install(${_name})
  endif()
endfunction()

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
  add_library(
    ${_name}
    ${_LIBRARY_BUILD_TYPE}
    ${ARGN}
  )
  target_compile_features(${_name} PUBLIC cxx_std_${CMAKE_CXX_STANDARD})

  # Add module include directories to target
  target_include_directories(
    ${_name}
    PUBLIC
      ${${itk-module}_GENEX_INCLUDE_DIRS}
  )

  # Add module system include directories to target
  target_include_directories(
    ${_name}
    SYSTEM
    PUBLIC
      ${${itk-module}_SYSTEM_GENEX_INCLUDE_DIRS}
  )

  # Add module library directories to target
  target_link_directories(${_name} PUBLIC ${${itk-module}_SYSTEM_LIBRARY_DIRS})

  target_link_options(
    ${_name}
    PUBLIC
      "$<$<AND:$<C_COMPILER_ID:AppleClang>,$<VERSION_GREATER_EQUAL:$<C_COMPILER_VERSION>,15.0>>:LINKER:-no_warn_duplicate_libraries>"
  )
  itk_module_link_dependencies()
  itk_module_target(${_name})
endmacro()
