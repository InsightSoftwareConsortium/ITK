# This module sets the variable "ITK_PRIVATE_DYNAMIC_CAST".
#
# This modules performs a try compile and execution to determine if
# the compiler and standard C++ run-time library supports
# dynamic_cast-ing of a private (hidden) Run-Time-Type-Information
# (RTTI). This can happen when private symbols (of instantiated
# templates) are duplicated in different libraries or executables.
#

function(_itkCheckPrivateDynamicCast)

  set(VARIABLE "ITK_PRIVATE_DYNAMIC_CAST")

  if(MSVC)
    set("${VARIABLE}" 1 CACHE INTERNAL
      "MSVC is know to support dynamic_cast of private symbols." FORCE)
    return()
  endif()

  set(test_project_dir "${PROJECT_BINARY_DIR}/CMakeTmp/${VARIABLE}")


  file(WRITE "${test_project_dir}/base.h" "
struct __attribute__ ((visibility (\"default\"))) base
{
  virtual ~base() = 0;
};

template <typename T> struct derived : public base{ ~derived() {};};

base* create(void) __attribute__ ((visibility (\"default\")));
")

  file(WRITE "${test_project_dir}/base.cxx" "
#include \"base.h\"

base::~base() {}
base* create(void) { return new derived<int>(); }
")


  file(WRITE "${test_project_dir}/main.cxx" "
#include \"base.h\"

int main(void)
{
  return bool(dynamic_cast<derived<int>*>(create()))?0:1;
}")

# we cannot use a "try_run" here because of the complexity of the
# test project with shared libraries, and visibility flags. The run is
# accomplished with a custom command as a post build step for the
# compilation of the executable.
  file(WRITE "${test_project_dir}/CMakeLists.txt" "
cmake_minimum_required(VERSION 2.8.12 FATAL_ERROR)
cmake_policy(VERSION 2.8.12)
project(support_private_dynamic_cast CXX)
add_library(base SHARED \"base.cxx\")
set_target_properties(base PROPERTIES CXX_VISIBILITY_PRESET hidden)
add_executable(test_cast \"main.cxx\")
target_link_libraries(test_cast PRIVATE base)
add_custom_command(TARGET test_cast POST_BUILD COMMAND $<TARGET_FILE:test_cast>)
")



  try_compile(${VARIABLE}
    "${test_project_dir}"
    "${test_project_dir}"
    support_private_dynamic_cast
    CMAKE_FLAGS
    "-DCMAKE_MACOSX_RPATH=OFF"
    OUTPUT_VARIABLE output)

  if(${VARIABLE})
    message(STATUS "Performing Test ${VARIABLE} - Success")
  else()
      message(STATUS "Performing Test ${VARIABLE} - Failed")
      file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
        "Performing Test ${VARIABLE} failed with the following output:\n"
        "${OUTPUT}\n")
    endif()

endfunction()


_itkCheckPrivateDynamicCast()
