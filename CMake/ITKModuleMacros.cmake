get_filename_component(_ITKModuleMacros_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)

include(${_ITKModuleMacros_DIR}/ITKModuleAPI.cmake)

macro(itk_module _name)
  set(itk-module ${_name})
  set(itk-module-test ${_name}-Test)
  set(_doing "")
  set(ITK_MODULE_${itk-module}_DECLARED 1)
  set(ITK_MODULE_${itk-module-test}_DECLARED 1)
  set(ITK_MODULE_${itk-module}_DEPENDS "")
  set(ITK_MODULE_${itk-module-test}_DEPENDS "${itk-module}")
  set(ITK_MODULE_${itk-module}_DEFAULT OFF)
  foreach(arg ${ARGN})
    if("${arg}" MATCHES "^(DEPENDS|TEST_DEPENDS|DEFAULT)$")
      set(_doing "${arg}")
    elseif("${arg}" MATCHES "^[A-Z][A-Z][A-Z]$")
      set(_doing "")
      message(AUTHOR_WARNING "Unknown argument [${arg}]")
    elseif("${_doing}" MATCHES "^DEPENDS$")
      list(APPEND ITK_MODULE_${itk-module}_DEPENDS "${arg}")
    elseif("${_doing}" MATCHES "^TEST_DEPENDS$")
      list(APPEND ITK_MODULE_${itk-module-test}_DEPENDS "${arg}")
    elseif("${_doing}" MATCHES "^DEFAULT")
      set(ITK_MODULE_${itk-module}_DEFAULT "${arg}")
    else()
      set(_doing "")
      message(AUTHOR_WARNING "Unknown argument [${arg}]")
    endif()
  endforeach()
  list(SORT ITK_MODULE_${itk-module}_DEPENDS) # Deterministic order.
  list(SORT ITK_MODULE_${itk-module-test}_DEPENDS) # Deterministic order.
endmacro()

macro(itk_module_impl)
  include(itk-module.cmake) # Load module meta-data
  set(${itk-module}_INSTALL_RUNTIME_DIR ${ITK_INSTALL_RUNTIME_DIR})
  set(${itk-module}_INSTALL_LIBRARY_DIR ${ITK_INSTALL_LIBRARY_DIR})
  set(${itk-module}_INSTALL_ARCHIVE_DIR ${ITK_INSTALL_ARCHIVE_DIR})
  set(${itk-module}_INSTALL_INCLUDE_DIR ${ITK_INSTALL_INCLUDE_DIR})

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
    install(DIRECTORY include/ DESTINATION ${${itk-module}_INSTALL_INCLUDE_DIR})
  endif()

  if(${itk-module}_INCLUDE_DIRS)
    include_directories(${${itk-module}_INCLUDE_DIRS})
  endif()
  if(${itk-module}_SYSTEM_INCLUDE_DIRS)
    include_directories(${${itk-module}_SYSTEM_INCLUDE_DIRS})
  endif()

  if(${itk-module}_THIRD_PARTY)
    itk_module_warnings_disable(C CXX)
  endif()

  set(itk-module-export-targets 0)
  if(EXISTS ${${itk-module}_SOURCE_DIR}/src/CMakeLists.txt AND NOT ${itk-module}_NO_SRC)
    set(itk-module-export-targets 1)
    add_subdirectory(src)
  endif()

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
  configure_file(${_ITKModuleMacros_DIR}/ITKModuleInfo.cmake.in ${ITK_MODULES_DIR}/${itk-module}.cmake @ONLY)
  set(itk-module-INCLUDE_DIRS "${itk-module-INCLUDE_DIRS-install}")
  configure_file(${_ITKModuleMacros_DIR}/ITKModuleInfo.cmake.in CMakeFiles/${itk-module}.cmake @ONLY)
  install(FILES
    ${${itk-module}_BINARY_DIR}/CMakeFiles/${itk-module}.cmake
    DESTINATION ${ITK_INSTALL_PACKAGE_DIR}/Modules
    )
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

macro(itk_module_target_name _name)
  set_property(TARGET ${_name} PROPERTY VERSION 1)
  set_property(TARGET ${_name} PROPERTY SOVERSION 1)
  set_property(TARGET ${_name} PROPERTY OUTPUT_NAME ${_name}-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR})
endmacro()

macro(itk_module_target_export _name)
  export(TARGETS ${_name} APPEND FILE ${${itk-module}-targets-build})
endmacro()

macro(itk_module_target_install _name)
  install(TARGETS ${_name}
    EXPORT  ${${itk-module}-targets}
    RUNTIME DESTINATION ${${itk-module}_INSTALL_RUNTIME_DIR}
    LIBRARY DESTINATION ${${itk-module}_INSTALL_ARCHIVE_DIR}
    ARCHIVE DESTINATION ${${itk-module}_INSTALL_LIBRARY_DIR}
    )
endmacro()

macro(itk_module_target _name)
  itk_module_target_name(${_name})
  itk_module_target_export(${_name})
  itk_module_target_install(${_name})
endmacro()
