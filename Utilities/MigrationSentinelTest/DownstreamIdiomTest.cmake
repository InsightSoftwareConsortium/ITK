# Verifies the two-arm downstream idiom in all four states.
cmake_minimum_required(VERSION 3.16)

set(_failures 0)
macro(expect_branch _cond_result _expected _what)
  if(NOT "${_cond_result}" STREQUAL "${_expected}")
    message(SEND_ERROR "${_what}: expected ${_expected} got ${_cond_result}")
    math(EXPR _failures "${_failures} + 1")
  endif()
endmacro()

# State 1: ITK predating the mechanism. ITK_MIGRATION_SENTINELS is never set.
set(ITK_VERSION 6.0.0)
unset(ITK_MIGRATION_SENTINELS)
if(
  ITK_VERSION
    VERSION_GREATER_EQUAL
    6.1.0
  OR
    ITK_MIGRATION_PR6532
      IN_LIST
      ITK_MIGRATION_SENTINELS
)
  set(_r TRUE)
else()
  set(_r FALSE)
endif()
expect_branch("${_r}" "FALSE" "old ITK, list unset -> legacy branch")

# State 2: sentinel present, version still below the future tag.
set(
  ITK_MIGRATION_SENTINELS
  ITK_MIGRATION_MATH_SVD
  ITK_MIGRATION_PR6532
)
if(
  ITK_VERSION
    VERSION_GREATER_EQUAL
    6.1.0
  OR
    ITK_MIGRATION_PR6532
      IN_LIST
      ITK_MIGRATION_SENTINELS
)
  set(_r TRUE)
else()
  set(_r FALSE)
endif()
expect_branch("${_r}" "TRUE" "sentinel present -> new branch")

# State 3: mechanism present but this sentinel absent.
if(
  ITK_VERSION
    VERSION_GREATER_EQUAL
    6.1.0
  OR
    ITK_MIGRATION_PR9999
      IN_LIST
      ITK_MIGRATION_SENTINELS
)
  set(_r TRUE)
else()
  set(_r FALSE)
endif()
expect_branch("${_r}" "FALSE" "sentinel absent -> legacy branch")

# State 4: after the tag. Sentinels expired and removed, version arm carries it.
set(ITK_VERSION 6.1.0)
unset(ITK_MIGRATION_SENTINELS)
if(
  ITK_VERSION
    VERSION_GREATER_EQUAL
    6.1.0
  OR
    ITK_MIGRATION_PR6532
      IN_LIST
      ITK_MIGRATION_SENTINELS
)
  set(_r TRUE)
else()
  set(_r FALSE)
endif()
expect_branch("${_r}" "TRUE" "post-tag, sentinels expired -> new branch")

if(_failures GREATER 0)
  message(FATAL_ERROR "${_failures} downstream-idiom check(s) failed")
endif()
message(STATUS "DownstreamIdiomTest: all checks passed")
