macro(itk_set_with_default var value)
  if(NOT ${var})
    set(${var} "${value}")
  endif()
endmacro()

# Bridge an old, deprecated, setting to a new replacement setting.
#
# Use this function when a user-visible flag is being renamed or otherwise
# replaced. If the old value is set, it will be given as the default value,
# otherwise the given default value will be used. This returned value should
# then be used in the ``set(CACHE)`` or ``option()`` call for the new value.
#
# If the old value is set, it will warn that it is deprecated for the new name.
#
# If replacing the setting ``OLD_SETTING`` with ``NEW_SETTING``, its usage
# would look like:
#
#   itk_deprecated_setting(default_setting NEW_SETTING OLD_SETTING "default value")
#   set(NEW_SETTING "${default_setting}"
#     CACHE STRING "Documentation for the setting.")
function(itk_deprecated_setting output_default new old intended_default)
  set(default "${intended_default}")
  if(DEFINED "${old}")
    message(WARNING "The '${old}' variable is deprecated for '${new}'.")
    set(default "${${old}}")
  endif()

  set("${output_default}" "${default}" PARENT_SCOPE)
endfunction()

# Usage:
#   set_boolean_with_change_warning(<VAR> <VALUE>)
#
# Examples:
#   set_boolean_with_change_warning(BUILD_TESTING OFF)
#   set_boolean_with_change_warning(ITK_USE_GPU YES)
#
# VALUE may be: ON/OFF, TRUE/FALSE, 1/0, YES/NO, Y/N (case-insensitive).
function(set_boolean_with_change_warning VAR VALUE)
  # Normalize input VALUE to ON/OFF
  string(TOUPPER "${VALUE}" _FB_VALUE_U)
  set(
    _FB_TRUE
    ON
    TRUE
    YES
    Y
    1
  )
  set(
    _FB_FALSE
    OFF
    FALSE
    NO
    N
    0
  )

  list(FIND _FB_TRUE "${_FB_VALUE_U}" _FB_TIDX)
  list(FIND _FB_FALSE "${_FB_VALUE_U}" _FB_FIDX)
  if(_FB_TIDX GREATER -1)
    set(_FB_NORM ON)
  elseif(_FB_FIDX GREATER -1)
    set(_FB_NORM OFF)
  else()
    message(
      FATAL_ERROR
      "set_boolean_with_change_warning(${VAR} ${VALUE}): VALUE must be a boolean-like token "
      "(ON/OFF, TRUE/FALSE, 1/0, YES/NO, Y/N)."
    )
  endif()

  # If cache entry exists, check whether we are changing it and warn if so.
  if(DEFINED CACHE{${VAR}})
    set(_FB_OLD "${${VAR}}")
    string(TOUPPER "${_FB_OLD}" _FB_OLD_U)
    if(_FB_OLD_U IN_LIST _FB_TRUE)
      set(_FB_OLD_NORM ON)
    else()
      set(_FB_OLD_NORM OFF)
    endif()

    if(NOT _FB_OLD_NORM STREQUAL _FB_NORM)
      message(
        WARNING
        "Changing cached BOOL '${VAR}' from '${_FB_OLD_NORM}' to '${_FB_NORM}' "
        "at ${CMAKE_CURRENT_LIST_FILE}:${CMAKE_CURRENT_LIST_LINE}"
      )
    endif()
  endif()

  # Force into cache as a BOOL
  set(
    ${VAR}
    ${_FB_NORM}
    CACHE BOOL
    "Forced by ${CMAKE_CURRENT_LIST_FILE}"
    FORCE
  )
endfunction()
