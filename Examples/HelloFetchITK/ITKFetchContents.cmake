#-----------------------------------------------------------------------------
# Get and build ITK using FetchContent

include(FetchContent)

# Set ITK Git repository and tag
set(ITK_GIT_REPOSITORY "https://github.com/InsightSoftwareConsortium/ITK.git")

set(ITK_GIT_TAG "main")

# Set ITK build options
set(ITK_BUILD_DEFAULT_MODULES ON)
set(ITK_USE_KWSTYLE OFF)
set(BUILD_TESTING OFF)
set(BUILD_EXAMPLES OFF)

FetchContent_Declare(
  ITK
  GIT_REPOSITORY "${ITK_GIT_REPOSITORY}"
  GIT_TAG "${ITK_GIT_TAG}"
  EXCLUDE_FROM_ALL
  FIND_PACKAGE_ARGS
    NAMES
    ITK
)

FetchContent_MakeAvailable(ITK)

# Check if FetchContent used find_package() or fetched from source
FetchContent_GetProperties(ITK)
if(ITK_SOURCE_DIR)
  message(STATUS "ITK fetched from repository and built from source")
  message(STATUS "  Source directory: ${ITK_SOURCE_DIR}")
  message(STATUS "  Binary directory: ${ITK_BINARY_DIR}")
elseif(DEFINED ITK_FOUND)
  message(STATUS "ITK found via find_package()")
  # ITK_DIR should already be set by find_package()
else()
  message(FATAL_ERROR "ITK configuration failed - no targets available")
endif()
