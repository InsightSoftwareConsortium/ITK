get_filename_component(_ITKModuleMacros_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)

set(_ITKModuleMacros_DEFAULT_LABEL "ITKModular")

include(${_ITKModuleMacros_DIR}/ITKModuleAPI.cmake)
include(${_ITKModuleMacros_DIR}/ITKModuleDoxygen.cmake)
include(${_ITKModuleMacros_DIR}/ITKModuleHeaderTest.cmake)

# With Apple's GGC <=4.2 and LLVM-GCC <=4.2 visibility of template
# don't work. Set the option to off and hide it.
if(APPLE AND CMAKE_COMPILER_IS_GNUCXX AND CMAKE_CXX_COMPILER_VERSION  VERSION_LESS "4.3")
  set( USE_COMPILER_HIDDEN_VISIBILITY OFF CACHE INTERNAL "" )
endif()
include(GenerateExportHeader)

if(ITK_CPPCHECK_TEST)
  include(${_ITKModuleMacros_DIR}/ITKModuleCPPCheckTest.cmake)
endif()

macro(itk_module _name)
  itk_module_check_name(${_name})
  set(itk-module ${_name})
  set(itk-module-test ${_name}-Test)
  set(_doing "")
  set(ITK_MODULE_${itk-module}_DECLARED 1)
  set(ITK_MODULE_${itk-module-test}_DECLARED 1)
  set(ITK_MODULE_${itk-module}_DEPENDS "")
  set(ITK_MODULE_${itk-module-test}_DEPENDS "${itk-module}")
  set(ITK_MODULE_${itk-module}_DESCRIPTION "description")
  set(ITK_MODULE_${itk-module}_ENABLE_SHARED 0)
  foreach(arg ${ARGN})
    if("${arg}" MATCHES "^(DEPENDS|TEST_DEPENDS|DESCRIPTION|DEFAULT)$")
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
    elseif("${arg}" MATCHES "^[A-Z][A-Z][A-Z]$")
      set(_doing "")
      message(AUTHOR_WARNING "Unknown argument [${arg}]")
    elseif("${_doing}" MATCHES "^DEPENDS$")
      list(APPEND ITK_MODULE_${itk-module}_DEPENDS "${arg}")
    elseif("${_doing}" MATCHES "^TEST_DEPENDS$")
      list(APPEND ITK_MODULE_${itk-module-test}_DEPENDS "${arg}")
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
  list(SORT ITK_MODULE_${itk-module-test}_DEPENDS) # Deterministic order.
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


  if( ITK_MODULE_${itk-module}_ENABLE_SHARED )

    # Need to use relative path to work around CMake ISSUE 12645 fixed
    # in CMake 2.8.8, to support older versions
    set(_export_header_file "${ITKCommon_BINARY_DIR}/${itk-module}Export.h")
    file(RELATIVE_PATH _export_header_file ${CMAKE_CURRENT_BINARY_DIR} ${_export_header_file} )

    # Generate the export macro header for symbol visibility/Windows DLL declspec
    generate_export_header(${itk-module}
      EXPORT_FILE_NAME ${_export_header_file}
      EXPORT_MACRO_NAME ${itk-module}_EXPORT
      NO_EXPORT_MACRO_NAME ${itk-module}_HIDDEN
      STATIC_DEFINE ITK_STATIC )
    install(FILES
      ${ITKCommon_BINARY_DIR}/${itk-module}Export.h
      DESTINATION ${${itk-module}_INSTALL_INCLUDE_DIR}
      COMPONENT Development
      )

    if (BUILD_SHARED_LIBS)
      # export flags are only added when building shared libs, they cause
      # mismatched visibility warnings when building statically.
      add_compiler_export_flags(my_abi_flags)
      set_property(TARGET ${itk-module} APPEND
        PROPERTY COMPILE_FLAGS "${my_abi_flags}")
    endif()
  endif()

  set(itk-module-EXPORT_CODE-build "${${itk-module}_EXPORT_CODE_BUILD}")
  set(itk-module-EXPORT_CODE-install "${${itk-module}_EXPORT_CODE_INSTALL}")

  set(itk-module-DEPENDS "${ITK_MODULE_${itk-module}_DEPENDS}")
  set(itk-module-LIBRARIES "${${itk-module}_LIBRARIES}")
  set(itk-module-INCLUDE_DIRS-build "${${itk-module}_INCLUDE_DIRS}")
  set(itk-module-INCLUDE_DIRS-install "\${ITK_INSTALL_PREFIX}/${${itk-module}_INSTALL_INCLUDE_DIR}")
  if(${itk-module}_SYSTEM_INCLUDE_DIRS)
    list(APPEND itk-module-INCLUDE_DIRS-build "${${itk-module}_SYSTEM_INCLUDE_DIRS}")
    list(APPEND itk-module-INCLUDE_DIRS-install "${${itk-module}_SYSTEM_INCLUDE_DIRS}")
  endif()
  set(itk-module-LIBRARY_DIRS "${${itk-module}_SYSTEM_LIBRARY_DIRS}")
  set(itk-module-INCLUDE_DIRS "${itk-module-INCLUDE_DIRS-build}")
  set(itk-module-EXPORT_CODE "${itk-module-EXPORT_CODE-build}")
  configure_file(${_ITKModuleMacros_DIR}/ITKModuleInfo.cmake.in ${ITK_MODULES_DIR}/${itk-module}.cmake @ONLY)
  set(itk-module-INCLUDE_DIRS "${itk-module-INCLUDE_DIRS-install}")
  set(itk-module-EXPORT_CODE "${itk-module-EXPORT_CODE-install}")
  configure_file(${_ITKModuleMacros_DIR}/ITKModuleInfo.cmake.in CMakeFiles/${itk-module}.cmake @ONLY)
  install(FILES
    ${${itk-module}_BINARY_DIR}/CMakeFiles/${itk-module}.cmake
    DESTINATION ${ITK_INSTALL_PACKAGE_DIR}/Modules
    COMPONENT Development
    )
  itk_module_doxygen( ${itk-module} )   # module name
endmacro()

macro(itk_module_test)
  include(../itk-module.cmake) # Load module meta-data
  set(${itk-module-test}_LIBRARIES "")
  itk_module_use(${ITK_MODULE_${itk-module-test}_DEPENDS})
  foreach(dep IN LISTS ITK_MODULE_${itk-module-test}_DEPENDS)
    list(APPEND ${itk-module-test}_LIBRARIES "${${dep}_LIBRARIES}")
  endforeach()
endmacro()

macro(itk_module_warnings_disable)
  foreach(lang ${ARGN})
    if(MSVC)
      string(REGEX REPLACE "(^| )[/-]W[0-4]( |$)" " "
        CMAKE_${lang}_FLAGS "${CMAKE_${lang}_FLAGS} -w")
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
  set_property(TARGET ${_name} PROPERTY OUTPUT_NAME ${_itk}${_name}-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR})
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
