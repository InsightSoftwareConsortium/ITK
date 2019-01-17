
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

set(CTEST_SITE "CircleCI")
set(CTEST_UPDATE_VERSION_ONLY 1)

set_from_env(PARALLEL_LEVEL "PARALLEL_LEVEL" DEFAULT 2 )
set( CTEST_TEST_ARGS ${CTEST_TEST_ARGS} PARALLEL_LEVEL  ${PARALLEL_LEVEL})


# Make environment variables to CMake variables for CTest
set_from_env(CTEST_CMAKE_GENERATOR "CTEST_CMAKE_GENERATOR" DEFAULT "Unix Makefiles" )
set_from_env(CTEST_BINARY_DIRECTORY "CTEST_BINARY_DIRECTORY")
set_from_env(CTEST_DASHBOARD_ROOT  "CTEST_DASHBOARD_ROOT" REQUIRED)
set_from_env(CTEST_SOURCE_DIRECTORY "CTEST_SOURCE_DIRECTORY" REQUIRED)
set_from_env(CTEST_CONFIGURATION_TYPE "CTEST_CONFIGURATION_TYPE" DEFAULT "Release")

# Legacy support for MAKEJ environment variable
# Please set CTEST_BUILD_FLAGS directly
set_from_env(makej "MAKEJ")
set_from_env(CTEST_BUILD_FLAGS "CTEST_BUILD_FLAGS" DEFAULT ${makej})

# Construct build name based on what is being built
string(SUBSTRING $ENV{CIRCLE_SHA1} 0 7 commit_sha1)
set(CTEST_BUILD_NAME "CircleCI-$ENV{CIRCLE_BRANCH}-${commit_sha1}")

set_from_env(dashboard_git_branch "CIRCLE_BRANCH")
set_from_env(dashboard_model "DASHBOARD_MODEL" DEFAULT "Continuous" )
set(dashboard_loop 0)

if( EXISTS "${CTEST_SOURCE_DIRECTORY}/circle.yml")
  list(APPEND CTEST_NOTES_FILES
    "${CTEST_SOURCE_DIRECTORY}/circle.yml"
    )
endif()

if( EXISTS  "${CTEST_SOURCE_DIRECTORY}/.circleci/config.yml")
  list(APPEND CTEST_NOTES_FILES
    "${CTEST_SOURCE_DIRECTORY}/.circleci/config.yml"
    )
endif()


set(dashboard_cache "
    BUILD_DOCUMENTATION:BOOL=OFF
    BUILD_EXAMPLES:BOOL=ON
    BUILD_SHARED_LIBS:BOOL=ON
    BUILD_TESTING:BOOL=ON
    ITK_USE_KWSTYLE:BOOL=OFF
    ITK_BUILD_DEFAULT_MODULES:BOOL=ON
" )


if(DEFINED ENV{DISTCC_DIR})
  set(dashboard_cache "${dashboard_cache}
    CMAKE_CXX_COMPILER_LAUNCHER:STRING=distcc
    CMAKE_C_COMPILER_LAUNCHER:STRING=distcc
")
endif()



include("${CTEST_SCRIPT_DIRECTORY}/itk_common.cmake")
