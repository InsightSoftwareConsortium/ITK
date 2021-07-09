##
## Intended to be run from a local build with 
set(DUMMY_VARIABLE_FOR_EASY_COMMANDLINE_PARSING
"""
  export AGENT_BUILDDIRECTORY=/tmp/NIFTIworkspace
  export BUILD_SOURCESDIRECTORY=${AGENT_BUILDDIRECTORY}/nifti_clib
  export BUILD_SOURCEBRANCHNAME=master 
  export CTEST_SCRIPT_DIRECTORY=$(pwd)
  ctest -S ${CTEST_SCRIPT_DIRECTORY}/local_dashboard.cmake -V -j 4
"""
)
##
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

#set_from_env(CTEST_SITE "TRAVIS_APP_HOST" REQUIRED)
cmake_host_system_information(RESULT CTEST_SITE QUERY HOSTNAME)
set(CTEST_SITE "travis.${CTEST_SITE}")
set(CTEST_UPDATE_VERSION_ONLY 1)

set_from_env(PARALLEL_LEVEL "PARALLEL_LEVEL" DEFAULT 8)
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
    set(branch "-$ENV{BUILD_SOURCEBRANCHNAME}")
    set(dashboard_git_branch "$ENV{BUILD_SOURCEBRANCHNAME}")
    set(dashboard_model "Experimental")
  endif()

  if(DEFINED ENV{SYSTEM_PULLREQUEST_PULLREQUESTNUMBER})
    set(pr "-PR$ENV{SYSTEM_PULLREQUEST_PULLREQUESTNUMBER}")
  else()
    set(pr "")
  endif()

  set(CTEST_BUILD_NAME
    "$ENV{TRAVIS_OS_NAME}-$ENV{BUILD_BUILDID}${pr}${branch}")
endif()

set(dashboard_cache "
    CMAKE_BUILD_TYPE:STRING=None 
    BUILD_TESTING:BOOL=ON
    BUILD_EXAMPLES:BOOL=${BUILD_EXAMPLES}
    BUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS}
    CMAKE_INTERPROCEDURAL_OPTIMIZATION:BOOL=${INTERPROCEDURAL_OPTIMIZATION}
    NIFTI_BUILD_APPLICATIONS:BOOL=ON
    USE_FSL_CODE:BOOL=ON
    USE_CIFTI_CODE:BOOL=ON
    USE_NIFTI2_CODE:BOOL=ON
" )
# Eventually USE_NIFTI1_CODE:BOOL=ON must be added.

string(TIMESTAMP build_date "%Y-%m-%d")
message("CDash Build Identifier: ${build_date} ${CTEST_BUILD_NAME}")
message("CTEST_SITE = ${CTEST_SITE}")
include("${CTEST_SCRIPT_DIRECTORY}/nifti_common.cmake")
