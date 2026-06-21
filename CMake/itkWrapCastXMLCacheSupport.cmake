###############################################################################
# Content-addressed two-level cache for CastXML wrapping steps.
#
# When ITK_WRAP_CASTXML_CACHE is ON, a Python wrapper replaces
# `ccache castxml` for every .xml generation step. The wrapper computes
# a two-level SHA-256 key:
#   L1 (fast, ~0.2s): direct inputs (cxx + castxml.inc + compiler flags)
#   L2 (robust, ~1s): sha256(L1_key + `castxml -E` preprocessed output)
#
# On L2 hit: restores .xml from cache, saving the full CastXML run.
# On miss:   runs castxml normally and populates the cache.
#
# Cache location: $ITK_WRAP_CACHE env var (default: ~/.cache/itk-wrap)

set(
  _ITK_WRAP_CASTXML_CACHE_SCRIPT_DEFAULT
  "${ITK_SOURCE_DIR}/Wrapping/Generators/CastXML/itk-castxml-cache.py"
)

option(
  ITK_WRAP_CASTXML_CACHE
  "Use a content-addressed two-level cache for CastXML wrapping steps."
  OFF
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

  message(STATUS "CastXML content-addressed cache enabled")
  message(STATUS "  Script: ${ITK_WRAP_CASTXML_CACHE_SCRIPT}")
  message(
    STATUS
    "  Cache root: set ITK_WRAP_CACHE env var at build time (default: ~/.cache/itk-wrap)"
  )
endif()

unset(_ITK_WRAP_CASTXML_CACHE_SCRIPT_DEFAULT)
