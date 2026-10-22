# Settings that must match the ITK build at ITK_DIR: seeded from ITK on the
# first configure, then verified on every reconfigure.
function(_itk_module_external_match_setting var itk_value type doc)
  if("${itk_value}" STREQUAL "")
    return()
  endif()
  # A normal variable hides the cache entry, so check the value in effect.
  if(DEFINED ${var})
    set(value "${${var}}")
  else()
    set(value "${itk_value}")
  endif()
  if(NOT DEFINED CACHE{${var}})
    set(${var} "${itk_value}" CACHE ${type} "${doc}")
  endif()
  if(type STREQUAL "BOOL")
    if((value AND itk_value) OR (NOT value AND NOT itk_value))
      return()
    endif()
  elseif(type STREQUAL "FILEPATH")
    file(REAL_PATH "${value}" real_value)
    file(REAL_PATH "${itk_value}" real_itk_value)
    if(real_value STREQUAL real_itk_value)
      return()
    endif()
  elseif(value STREQUAL itk_value)
    return()
  endif()
  set(
    _itk_mismatched_settings
    "${_itk_mismatched_settings}\n  -D${var}=${itk_value} (currently ${value})"
    PARENT_SCOPE
  )
endfunction()

set(_itk_mismatched_settings "")
_itk_module_external_match_setting(
  BUILD_SHARED_LIBS
  "${ITK_BUILD_SHARED}"
  BOOL
  "Build ITK with shared libraries."
)
if(ITK_WRAP_PYTHON)
  _itk_module_external_match_setting(
    Python3_EXECUTABLE
    "${ITK_WRAP_PYTHON_EXECUTABLE}"
    FILEPATH
    "Path to the Python interpreter"
  )
  _itk_module_external_match_setting(
    ITK_USE_PYTHON_LIMITED_API
    "${ITK_WRAP_PYTHON_LIMITED_API}"
    BOOL
    "Configure Python's limited API for Python minor version compatibility."
  )
endif()
if(_itk_mismatched_settings)
  message(
    FATAL_ERROR
    "Settings differ from the ITK build at ${ITK_DIR}. Reconfigure with${_itk_mismatched_settings}\n"
    "or configure a fresh build tree (cmake --fresh)."
  )
endif()
unset(_itk_mismatched_settings)
