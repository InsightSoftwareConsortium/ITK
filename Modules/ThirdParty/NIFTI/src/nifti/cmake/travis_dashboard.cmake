
# set_from_env
# ------------
#
# Sets a CMake variable from an environment variable. If the
# environment variable  is not  defined then the CMake variable is not
# modified. If DEFAULT is specified then if the environment variable
# is not defined the default value is used. Alternatively, if REQUIRED
# is specified then a FATAL_ERROR is generated.
#
# set_from_env( <variable> <environment variable> [REQUIRED|DEFAULT value] )
function(set_from_env var env_var)
  if(NOT DEFINED ENV{${env_var}})
    if (ARGV2 STREQUAL "REQUIRED")
      message(FATAL_ERROR "Required environment variable \"${env_var}\" not defined.")
    elseif (ARGV2 STREQUAL "DEFAULT")
      set(${var} ${ARGV3} PARENT_SCOPE)
    endif()
  else()
    set(${var} $ENV{${env_var}} PARENT_SCOPE)
  endif()
endfunction()

set_from_env(CTEST_SITE "TRAVIS_APP_HOST" REQUIRED)
set(CTEST_SITE "travis.${CTEST_SITE}")
set(CTEST_UPDATE_VERSION_ONLY 1)

# https://gitlab.kitware.com/cmake/community/wikis/doc/ctest/Scripting-Of-CTest
find_program(CTEST_GIT_COMMAND NAMES git)
find_program(CTEST_COVERAGE_COMMAND NAMES gcov)
find_program(CTEST_MEMORYCHECK_COMMAND NAMES valgrind)

set_from_env(PARALLEL_LEVEL "PARALLEL_LEVEL" DEFAULT 3)
set(CTEST_TEST_ARGS ${CTEST_TEST_ARGS} PARALLEL_LEVEL ${PARALLEL_LEVEL})


# The absolute path to the directory where the repository being built has been copied on the worker.
set_from_env(workspace "AGENT_BUILDDIRECTORY" REQUIRED)
file(TO_CMAKE_PATH "${workspace}" CTEST_DASHBOARD_ROOT)
file(RELATIVE_PATH dashboard_source_name "${workspace}" "$ENV{BUILD_SOURCESDIRECTORY}")
# Make environment variables to CMake variables for CTest
#set_from_env(CTEST_CMAKE_GENERATOR "CTEST_CMAKE_GENERATOR" DEFAULT "Ninja")
set_from_env(CTEST_CMAKE_GENERATOR "CTEST_CMAKE_GENERATOR" DEFAULT "Unix Makefiles")
set_from_env(CTEST_BUILD_CONFIGURATION "CTEST_BUILD_CONFIGURATION" DEFAULT "Debug")

set_from_env(BUILD_SHARED_LIBS "BUILD_SHARED_LIBS" DEFAULT "ON")
set_from_env(BUILD_EXAMPLES "BUILD_EXAMPLES" DEFAULT "ON")
set_from_env(INTERPROCEDURAL_OPTIMIZATION "INTERPROCEDURAL_OPTIMIZATION" DEFAULT "ON")
set_from_env(NIFTI_SHELL_SCRIPT_TESTS "NIFTI_SHELL_SCRIPT_TESTS" DEFAULT "ON")
set_from_env(dashboard_do_coverage "WITH_COVERAGE" DEFAULT "OFF")
set_from_env(dashboard_do_memcheck "WITH_MEMCHECK" DEFAULT "OFF")

if(NOT CTEST_BUILD_NAME)
  if(DEFINED ENV{SYSTEM_PULLREQUEST_SOURCEBRANCH})
    set(branch "-$ENV{SYSTEM_PULLREQUEST_SOURCEBRANCH}")
    set(dashboard_git_branch "$ENV{SYSTEM_PULLREQUEST_SOURCEBRANCH}")
    set(dashboard_model "Experimental")
  elseif(ENV{BUILD_SOURCEBRANCHNAME} STREQUAL "master")
    set(branch "-master")
    set(dashboard_git_branch "$ENV{BUILD_SOURCEBRANCHNAME}")
    set(dashboard_model "Continuous")
  elseif(ENV{BUILD_SOURCEBRANCHNAME} STREQUAL "nightly-master")
    set(branch "-nightly-master")
    set(dashboard_git_branch "$ENV{BUILD_SOURCEBRANCHNAME}")
    set(dashboard_model "Nightly")
  else()
    set(branch "-master")
    set(dashboard_git_branch "master")
    set(dashboard_model "Experimental")
  endif()

  if(DEFINED ENV{SYSTEM_PULLREQUEST_PULLREQUESTNUMBER})
    set(pr "-PR$ENV{SYSTEM_PULLREQUEST_PULLREQUESTNUMBER}")
  else()
    set(pr "")
  endif()

  set(CTEST_BUILD_NAME
    "$ENV{BLDPREFIX}_$ENV{TRAVIS_OS_NAME}-$ENV{BUILD_BUILDID}_${pr}_${branch}")
endif()

set(dashboard_cache "
    CMAKE_BUILD_TYPE:STRING=None 
    BUILD_TESTING:BOOL=ON
    BUILD_EXAMPLES:BOOL=${BUILD_EXAMPLES}
    BUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS}
    CMAKE_INTERPROCEDURAL_OPTIMIZATION:BOOL=${INTERPROCEDURAL_OPTIMIZATION}
    NIFTI_BUILD_APPLICATIONS:BOOL=ON
    NIFTI_SHELL_SCRIPT_TESTS:BOOL=${NIFTI_SHELL_SCRIPT_TESTS}
    USE_FSL_CODE:BOOL=ON
    USE_CIFTI_CODE:BOOL=ON
    USE_NIFTI2_CODE:BOOL=ON
    CMAKE_C_COMPILER:STRING=$ENV{CC}
    CMAKE_C_FLAGS:STRING=$ENV{CFLAGS}
    CMAKE_CXX_COMPILER:STRING=$ENV{CXX}
    CMAKE_CXX_FLAGS:STRING=$ENV{CXXFLAGS}
    CMAKE_EXE_LINKER_FLAGS:STRING=$ENV{LDFLAGS}
    CMAKE_MODULE_LINKER_FLAGS:STRING=$ENV{LDFLAGS}
" )
# Eventually USE_NIFTI1_CODE:BOOL=ON must be added.

string(TIMESTAMP build_date "%Y-%m-%d")
message(STATUS "CDash Build Identifier: ${build_date} ${CTEST_BUILD_NAME}")
message(STATUS "CTEST_SITE = ${CTEST_SITE}")
include("${CTEST_SCRIPT_DIRECTORY}/nifti_common.cmake")
