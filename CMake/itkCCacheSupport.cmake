###############################################################################
# ccache management for building ITK
if(CMAKE_CXX_COMPILER_LAUNCHER OR CMAKE_C_COMPILER_LAUNCHER)
  set(_default_ITK_USE_CCACHE ON)
else()
  set(_default_ITK_USE_CCACHE OFF)
endif()

option(
  ITK_USE_CCACHE
  "Use ccache to cache swig/castxml/... output and speedup the rebuild."
  ${_default_ITK_USE_CCACHE}
)
unset(_default_ITK_USE_CCACHE)
mark_as_advanced(ITK_USE_CCACHE)
if(ITK_USE_CCACHE)
  find_program(
    CCACHE_EXECUTABLE
    NAMES
      ${CMAKE_C_COMPILER_LAUNCHER}
      ${CMAKE_CXX_COMPILER_LAUNCHER}
      ccache
    DOC "ccache executable is needed for ITK_USE_CCACHE=ON"
    REQUIRED
  )

  if(CCACHE_EXECUTABLE)
    execute_process(
      COMMAND
        ${CCACHE_EXECUTABLE} --version
      OUTPUT_VARIABLE CCACHE_OUTPUT
      OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    string(
      REGEX
      MATCH
      "ccache *version *([0-9]+\\.[0-9]+\\.[0-9]+)"
      _match
      "${CCACHE_OUTPUT}"
    )
    set(CCACHE_VERSION "${CMAKE_MATCH_1}")
    message(STATUS "ccache version: ${CCACHE_VERSION} at ${CCACHE_EXECUTABLE}")
    set(CMAKE_C_COMPILER_LAUNCHER "${CCACHE_EXECUTABLE}")
    set(CMAKE_CXX_COMPILER_LAUNCHER "${CCACHE_EXECUTABLE}")
    # ccache hashes absolute source and include paths into the cache key.
    # ITK developers utilizing ccache often will be using git worktrees
    # where the absolute paths differ, producing cache misses.
    # By setting CCACHE_BASEDIR, increased cache hits occur based on
    # hits relative to the ITK_SOURCE_DIR.
    # Export CCACHE_BASEDIR for all build commands to the base of the
    set(ENV{CCACHE_BASEDIR} "${ITK_SOURCE_DIR}")
    message(STATUS "Set CCACHE_BASEDIR = $ENV{CCACHE_BASEDIR}")
  else()
    message(FATAL_ERROR "ccache not found, turn off ITK_USE_CCACHE")
  endif()
  if(CCACHE_VERSION VERSION_LESS 4.0)
    message(
      FATAL_ERROR
      "Only ccache greater than 4.0 is supported, ${CCACHE_VERSION} found."
    )
  endif()
endif()
