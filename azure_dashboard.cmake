
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

set_from_env(dashboard_do_coverage "DASHBOARD_DO_COVERAGE" DEFAULT 0)
set_from_env(CTEST_COVERAGE_COMMAND "CTEST_COVERAGE_COMMAND")

set(dashboard_loop 0)

if(NOT CTEST_BUILD_NAME)
  if(DEFINED ENV{SYSTEM_PULLREQUEST_SOURCEBRANCH})
    set(branch "-$ENV{SYSTEM_PULLREQUEST_SOURCEBRANCH}")
    set(dashboard_model "Experimental")
  elseif("$ENV{BUILD_SOURCEBRANCHNAME}" STREQUAL "master" OR
         "$ENV{BUILD_SOURCEBRANCHNAME}" MATCHES "release.*" )
    set(branch "-$ENV{BUILD_SOURCEBRANCHNAME}")
    set(dashboard_model "Continuous")
  elseif("$ENV{BUILD_SOURCEBRANCHNAME}" STREQUAL "nightly-master")
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

  set(CTEST_BUILD_NAME
    "$ENV{AGENT_OS}-Build$ENV{BUILD_BUILDID}${pr}${branch}${BUILD_NAME_SUFFIX}")
endif()

set(CTEST_CUSTOM_WARNING_EXCEPTION
  \${CTEST_CUSTOM_WARNING_EXCEPTION}
  # macOS Azure Pipelines
  "ld: warning: text-based stub file"
  )

string(TIMESTAMP build_date "%Y-%m-%d")
message("CDash Build Identifier: ${build_date} ${CTEST_BUILD_NAME}")
message("CTEST_SITE = ${CTEST_SITE}")
include("${CTEST_SCRIPT_DIRECTORY}/itk_common.cmake")
