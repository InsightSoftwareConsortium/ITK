set(CTEST_SITE "CircleCI")

function(set_from_env var env_var)
  if(NOT DEFINED ENV{${env_var}})
    message(FATAL_ERROR "Required environment variable \"${env_var}\" not defined.")
  endif()
  set(${var} $ENV{${env_var}} PARENT_SCOPE)
endfunction()


set_from_env(CTEST_DASHBOARD_ROOT  "CTEST_SOURCE_DIRECTORY")
set_from_env(CTEST_DASHBOARD_ROOT  "CTEST_DASHBOARD_ROOT")
set_from_env(CTEST_CONFIGURATION_TYPE "CTEST_CONFIGURATION_TYPE")

string(SUBSTRING $ENV{CIRCLE_SHA1} 0 7 commit_sha1)

set(CTEST_BUILD_NAME "CircleCI-$ENV{CIRCLE_BRANCH}-${commit_sha1}")

if( NOT "$ENV{CI_PULL_REQUEST}" STREQUAL "" )
  set(CTEST_BUILD_NAME "${CTEST_BUILD_NAME}-pull")
endif()


set(CTEST_CMAKE_GENERATOR "Unix Makefiles")

if (DEFINED ENV{MAKEJ})
  set( CTEST_BUILD_FLAGS -j$ENV{MAKEJ})
endif()

set( CTEST_TEST_ARGS ${CTEST_TEST_ARGS} PARALLEL_LEVEL 2 )

set_from_env(CTEST_SOURCE_DIRECTORY "CTEST_SOURCE_DIRECTORY" )
set_from_env(CTEST_BINARY_DIRECTORY "CTEST_BINARY_DIRECTORY")
set_from_env(dashboard_model "DASHBOARD_MODEL")

if (DEFINED ENV{CONFIGURATION})
  set(CTEST_BUILD_CONFIGURATION "$ENV{CTEST_BUILD_CONFIGURATION}")
endif()

# CircleCI already has the repository check out to the hash to be tested.
set(dashboard_no_update 1)

set(dashboard_loop 0)

list(APPEND CTEST_NOTES_FILES
  "${CTEST_SOURCE_DIRECTORY}/circleci.yml"
  )

SET (dashboard_cache "
    BUILD_DOCUMENTATION:BOOL=OFF
    BUILD_EXAMPLES:BOOL=OFF
    BUILD_SHARED_LIBS:BOOL=ON
    BUILD_TESTING:BOOL=ON
    ITK_USE_KWSTYLE:BOOL=OFF
    ITK_BUILD_DEFAULT_MODULES:BOOL=ON
    CMAKE_CXX_COMPILER_LAUNCHER:STRING=distcc
    CMAKE_C_COMPILER_LAUNCHER:STRING=distcc
" )


include("${CTEST_SCRIPT_DIRECTORY}/itk_common.cmake")
