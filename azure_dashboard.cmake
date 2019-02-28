
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

set_from_env(CTEST_SITE "AGENT_MACHINENAME" REQUIRED)
set(CTEST_SITE "Azure.${CTEST_SITE}")
set(CTEST_UPDATE_VERSION_ONLY 1)

set_from_env(PARALLEL_LEVEL "PARALLEL_LEVEL" DEFAULT 3)
set(CTEST_TEST_ARGS ${CTEST_TEST_ARGS} PARALLEL_LEVEL ${PARALLEL_LEVEL})


set_from_env(workspace "AGENT_BUILDDIRECTORY" REQUIRED)
file(TO_CMAKE_PATH "${workspace}" CTEST_DASHBOARD_ROOT)
file(RELATIVE_PATH dashboard_source_name "${workspace}" "$ENV{BUILD_SOURCESDIRECTORY}")
# Make environment variables to CMake variables for CTest
set_from_env(CTEST_CMAKE_GENERATOR "CTEST_CMAKE_GENERATOR" DEFAULT "Ninja")
set_from_env(CTEST_BUILD_CONFIGURATION "CTEST_BUILD_CONFIGURATION" DEFAULT "Release")

set_from_env(dashboard_do_coverage "DASHBOARD_DO_COVERAGE" DEFAULT 0)
set_from_env(CTEST_COVERAGE_COMMAND "CTEST_COVERAGE_COMMAND")

set_from_env(BUILD_SHARED_LIBS "BUILD_SHARED_LIBS" DEFAULT "OFF")
set_from_env(BUILD_EXAMPLES "BUILD_EXAMPLES" DEFAULT "ON")
set_from_env(ITK_WRAP_PYTHON "ITK_WRAP_PYTHON" DEFAULT "OFF")
set_from_env(ITK_BUILD_DEFAULT_MODULES "ITK_BUILD_DEFAULT_MODULES" DEFAULT "ON")

if(NOT CTEST_BUILD_NAME)
  if(DEFINED ENV{SYSTEM_PULLREQUEST_SOURCEBRANCH})
    set(branch "-$ENV{SYSTEM_PULLREQUEST_SOURCEBRANCH}")
    set(dashboard_model "Experimental")
  elseif(ENV{BUILD_SOURCEBRANCHNAME} STREQUAL "master")
    set(branch "-master")
    set(dashboard_model "Continuous")
  elseif(ENV{BUILD_SOURCEBRANCHNAME} STREQUAL "nightly-master")
    set(branch "-nightly-master")
    set(dashboard_model "Nightly")
  else()
    set(branch "-$ENV{BUILD_SOURCEBRANCHNAME}")
    set(dashboard_model "Experimental")
  endif()

  if(DEFINED ENV{SYSTEM_PULLREQUEST_PULLREQUESTNUMBER})
    set(pr "-PR$ENV{SYSTEM_PULLREQUEST_PULLREQUESTNUMBER}")
  else()
    set(pr "")
  endif()

  set(wrapping )
  if(ITK_WRAP_PYTHON)
    set(wrapping "-Python")
  endif()

  set(CTEST_BUILD_NAME
    "$ENV{AGENT_OS}-Build$ENV{BUILD_BUILDID}${pr}${branch}${wrapping}")
endif()

set(_dashboard_cache "
    BUILD_EXAMPLES:BOOL=${BUILD_EXAMPLES}
    BUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS}
    ITK_BUILD_DEFAULT_MODULES:BOOL=${ITK_BUILD_DEFAULT_MODULES}
    ITKGroup_Core:BOOL=ON
    ITK_WRAP_PYTHON:BOOL=${ITK_WRAP_PYTHON}
" )

set_from_env(dashboard_cache "CTEST_CACHE" DEFAULT ${_dashboard_cache})

string(TIMESTAMP build_date "%Y-%m-%d")
message("CDash Build Identifier: ${build_date} ${CTEST_BUILD_NAME}")
message("CTEST_SITE = ${CTEST_SITE}")
include("${CTEST_SCRIPT_DIRECTORY}/itk_common.cmake")
