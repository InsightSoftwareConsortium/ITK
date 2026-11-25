# Remove transient test output files matching the given path patterns.
#
# Invoked as a fixture cleanup command:
#   cmake -P itkRemoveTestFiles.cmake <pattern> [<pattern> ...]
#
# Each pattern is expanded with file(GLOB) at test time and only the matching
# regular files are removed.  Directories are never removed, so an unintended
# pattern can delete individual files but can never recurse into a tree.
#
# A pattern may contain a single brace set "{a,b,c}" listing the extensions a
# test actually writes for a shared stem, e.g. "foo.{mhd,raw}" expands to
# "foo.mhd" and "foo.raw" before globbing.
if(CMAKE_ARGC LESS 4)
  return()
endif()

# Expand one "{a,b,c}" brace set in PATTERN into OUT_VAR (a list).
function(_itk_expand_braces PATTERN OUT_VAR)
  string(FIND "${PATTERN}" "{" _open)
  string(FIND "${PATTERN}" "}" _close)
  if(_open EQUAL -1 OR _close EQUAL -1 OR _close LESS _open)
    set(${OUT_VAR} "${PATTERN}" PARENT_SCOPE)
    return()
  endif()
  string(SUBSTRING "${PATTERN}" 0 ${_open} _prefix)
  math(EXPR _glen "${_close} - ${_open} - 1")
  math(EXPR _gstart "${_open} + 1")
  string(SUBSTRING "${PATTERN}" ${_gstart} ${_glen} _group)
  math(EXPR _sstart "${_close} + 1")
  string(SUBSTRING "${PATTERN}" ${_sstart} -1 _suffix)
  string(REPLACE "," ";" _opts "${_group}")
  set(_result "")
  foreach(_o IN LISTS _opts)
    list(APPEND _result "${_prefix}${_o}${_suffix}")
  endforeach()
  set(${OUT_VAR} "${_result}" PARENT_SCOPE)
endfunction()

math(EXPR _last "${CMAKE_ARGC} - 1")
foreach(_i RANGE 3 ${_last})
  _itk_expand_braces("${CMAKE_ARGV${_i}}" _patterns)
  foreach(_pat IN LISTS _patterns)
    file(GLOB _matches "${_pat}")
    foreach(_match IN LISTS _matches)
      if(NOT IS_DIRECTORY "${_match}")
        file(REMOVE "${_match}")
      endif()
    endforeach()
  endforeach()
endforeach()
