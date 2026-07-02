# Locate the wrapper script relative to this file so the default also
# resolves when ITK_SOURCE_DIR is not defined in the including scope.
get_filename_component(
  _itk_wrap_castxml_cache_src_root
  "${CMAKE_CURRENT_LIST_DIR}/.."
  ABSOLUTE
)
set(
  _ITK_WRAP_CASTXML_CACHE_SCRIPT_DEFAULT
  "${_itk_wrap_castxml_cache_src_root}/Wrapping/Generators/CastXML/itk-castxml-cache.py"
)
unset(_itk_wrap_castxml_cache_src_root)

option(
  ITK_WRAP_CASTXML_CACHE
  "Use a content-addressed two-level cache for CastXML wrapping steps."
  ON
)
mark_as_advanced(ITK_WRAP_CASTXML_CACHE)

if(ITK_WRAP_CASTXML_CACHE)
  set(
    ITK_WRAP_CASTXML_CACHE_SCRIPT
    "${_ITK_WRAP_CASTXML_CACHE_SCRIPT_DEFAULT}"
    CACHE FILEPATH
    "Path to the CastXML content-addressed cache wrapper script"
  )
  mark_as_advanced(ITK_WRAP_CASTXML_CACHE_SCRIPT)

  if(NOT EXISTS "${ITK_WRAP_CASTXML_CACHE_SCRIPT}")
    message(
      FATAL_ERROR
      "ITK_WRAP_CASTXML_CACHE is ON but the wrapper script was not found:\n"
      "  ${ITK_WRAP_CASTXML_CACHE_SCRIPT}\n"
      "Set ITK_WRAP_CASTXML_CACHE_SCRIPT to the correct path or turn off ITK_WRAP_CASTXML_CACHE."
    )
  endif()

  if(NOT Python3_EXECUTABLE)
    message(
      FATAL_ERROR
      "ITK_WRAP_CASTXML_CACHE requires Python3_EXECUTABLE to be set."
    )
  endif()

  set(
    ITK_WRAP_CASTXML_CACHE_MAX_DAYS
    "13.9"
    CACHE STRING
    "Days before a CastXML cache entry is evicted by --evict after each build (default 13.9)"
  )
  mark_as_advanced(ITK_WRAP_CASTXML_CACHE_MAX_DAYS)

  message(STATUS "CastXML content-addressed cache enabled")
  message(STATUS "  Script: ${ITK_WRAP_CASTXML_CACHE_SCRIPT}")
  message(
    STATUS
    "  Cache root: set ITK_WRAP_CACHE env var at build time (default: ~/.cache/itk-wrap)"
  )
  message(
    STATUS
    "  Eviction (after each build): entries older than ${ITK_WRAP_CASTXML_CACHE_MAX_DAYS} days, then oldest trimmed to ITK_WRAP_CACHE_MAX_SIZE GB (default 2.0)"
  )
endif()

unset(_ITK_WRAP_CASTXML_CACHE_SCRIPT_DEFAULT)
