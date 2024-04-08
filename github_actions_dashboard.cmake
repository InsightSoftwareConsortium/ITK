
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

function(set_if_undef var default)
  if(NOT DEFINED var)
      set(${var} ${default} PARENT_SCOPE)
  endif()
endfunction()


set_from_env(CTEST_SITE "GITHUB_REPOSITORY" REQUIRED)
set(CTEST_SITE "GitHub.${CTEST_SITE}")
set(CTEST_UPDATE_VERSION_ONLY 1)

set_from_env(PARALLEL_LEVEL "PARALLEL_LEVEL" DEFAULT 3)
set(CTEST_TEST_ARGS ${CTEST_TEST_ARGS} PARALLEL_LEVEL ${PARALLEL_LEVEL})


set_from_env(workspace "GITHUB_WORKSPACE" REQUIRED)
file(TO_CMAKE_PATH "${workspace}" CTEST_DASHBOARD_ROOT)
file(RELATIVE_PATH dashboard_source_name "${workspace}" "$ENV{GITHUB_WORKSPACE}")

if (NOT DEFINED CTEST_BINARY_DIRECTORY)
  set(CTEST_BINARY_DIRECTORY "${CTEST_DASHBOARD_ROOT}/build")
endif()

set_if_undef(dashboard_do_coverage 0)

set(dashboard_loop 0)

if(NOT CTEST_BUILD_NAME)
  if(DEFINED ENV{GITHUB_HEAD_REF})
    set(branch "-$ENV{GITHUB_HEAD_REF}")
    set(dashboard_model "Experimental")
  elseif("$ENV{GITHUB_REF}" STREQUAL "refs/heads/master" OR
         "$ENV{GITHUB_REF}" MATCHES "refs/heads/release.*" )
    set(branch "-$ENV{GITHUB_REF}")
    set(dashboard_model "Continuous")
  elseif("$ENV{GITHUB_REF}" STREQUAL "refs/heads/nightly-master")
    set(branch "-nightly-master")
    set(dashboard_model "Nightly")
  else()
    set(branch "-$ENV{GITHUB_REF}")
    set(dashboard_model "Experimental")
  endif()

  if(DEFINED ENV{GITHUB_PR_NUMBER})
    set(pr "-PR$ENV{GITHUB_PR_NUMBER}")
  else()
    set(pr "")
  endif()

  set(CTEST_BUILD_NAME
    "$ENV{RUNNER_OS}-Build$ENV{GITHUB_RUN_ID}${pr}${branch}${BUILD_NAME_SUFFIX}")
endif()

set(CTEST_CUSTOM_WARNING_EXCEPTION
  \${CTEST_CUSTOM_WARNING_EXCEPTION}
  # macOS GitHub Actions
  "ld: warning: text-based stub file"
  )

string(TIMESTAMP build_date "%Y-%m-%d")
message("CDash Build Identifier: ${build_date} ${CTEST_BUILD_NAME}")
message("CTEST_SITE = ${CTEST_SITE}")
include("${CTEST_SCRIPT_DIRECTORY}/itk_common.cmake")
