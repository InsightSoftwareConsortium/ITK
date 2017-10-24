# This project builds the test directories from all ITK modules as a separate
# project outside the main ITK build tree as if they were an application.
cmake_minimum_required(VERSION 2.8.12 FATAL_ERROR)

foreach(p
    CMP0025 # CMake 3.0
    CMP0042 # CMake 3.0
    CMP0054 # CMake 3.1
    CMP0056 # CMake 3.2
    CMP0058 # CMake 3.3
    )
  if(POLICY ${p})
    cmake_policy(SET ${p} NEW)
  endif()
endforeach()

project(ITKTestExternal)
if(ITK_SOURCE_DIR OR ITK_BINARY_DIR)
  message(FATAL_ERROR "This directory may build only outside ITK!")
endif()

include(CTest)

# Find the top of the main ITK source tree.
get_filename_component(ITK_TOP_DIR ${ITKTestExternal_SOURCE_DIR}/../.. ABSOLUTE)
set(ExternalData_SOURCE_ROOT ${ITK_TOP_DIR})

# Load module infrastructure macros.
include(${ITK_TOP_DIR}/CMake/ITKModuleMacros.cmake)
include(${ITK_TOP_DIR}/CMake/ITKExternalData.cmake)
include(${ITK_TOP_DIR}/CMake/ITKModuleTest.cmake)

# Tell modules how to add their tests.
function(itk_add_test)
  # Add tests with data in the ITKData group.
  ExternalData_add_test(ITKData ${ARGN})
endfunction()

# Find the ITK build or install tree.  Assume the version matches exactly.
# One should provide ITK_DIR explicitly in our intended use cases.
find_package(ITK REQUIRED NO_MODULE)

# Use ITK's flags.
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${ITK_REQUIRED_C_FLAGS}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${ITK_REQUIRED_CXX_FLAGS}")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")

# This is a cross-platform project so we cannot use the MS _s API.
if(WIN32 AND "${CMAKE_C_COMPILER_ID}" MATCHES "^(Intel)$")
  set(_INTEL_WINDOWS 1)
endif()
if(MSVC OR _INTEL_WINDOWS)
  add_definitions(
    -D_CRT_NONSTDC_NO_DEPRECATE
    -D_CRT_SECURE_NO_DEPRECATE
    -D_SCL_SECURE_NO_DEPRECATE
    )
endif()

# Glob the set modules in the source tree.
file(GLOB meta RELATIVE "${ITK_TOP_DIR}"
   "${ITK_TOP_DIR}/*/*/*/itk-module.cmake" # grouped modules
  )
# Identify the subset of modules that have tests.
foreach(f ${meta})
  get_filename_component(_base ${f} PATH)
  if(EXISTS ${ITK_TOP_DIR}/${_base}/test)
    include(${ITK_TOP_DIR}/${f})
    set(${itk-module}_TEST_DIR ${_base}/test)
    unset(itk-module)
  endif()
endforeach()

# Input information for test build files.
set(ITK_DATA_ROOT ${ITK_TOP_DIR}/Testing/Data)
set(ITK_EXAMPLE_DATA_ROOT ${ITK_TOP_DIR}/Examples/Data)
set(ITK_TEST_OUTPUT_DIR "${ITKTestExternal_BINARY_DIR}/Testing/Temporary")

# Add the test directory for each enabled module that has tests.
foreach(itk-module ${ITK_MODULES_ENABLED})
  if(${itk-module}_TEST_DIR)
    add_subdirectory(${ITK_TOP_DIR}/${${itk-module}_TEST_DIR}
                     ${ITKTestExternal_BINARY_DIR}/${${itk-module}_TEST_DIR})
  endif()
endforeach()


# Create target to download data from the ITKData group.  This must come after
# all tests have been added that reference the group, so we put it last.
ExternalData_Add_Target(ITKData)
