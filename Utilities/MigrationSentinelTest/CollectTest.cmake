# Verifies itk_collect_migration_sentinels() against fixed fixtures.
# Fixtures are used rather than the real sentinel directory so these tests do
# not break when real sentinels are expired at tag time.
cmake_minimum_required(VERSION 3.16)
include("${ITK_SOURCE_DIR}/CMake/itkMigrationSentinels.cmake")

set(_failures 0)
macro(expect_eq _actual _expected _what)
  if(NOT "${_actual}" STREQUAL "${_expected}")
    message(SEND_ERROR "${_what}: expected [${_expected}] got [${_actual}]")
    math(EXPR _failures "${_failures} + 1")
  endif()
endmacro()

# Populated fixture: two sentinels, plus a README.md and a stray file that the
# glob must ignore.
itk_collect_migration_sentinels(
  "${CMAKE_CURRENT_LIST_DIR}/Fixtures/Populated" _got
)
expect_eq("${_got}" "ITK_MIGRATION_MATH_SVD;ITK_MIGRATION_PR6532"
          "populated fixture (sorted, README.md excluded)"
)

# Empty fixture: models ITK immediately after a tag, when all sentinels have
# been expired. Must yield an empty list, not an error.
itk_collect_migration_sentinels(
  "${CMAKE_CURRENT_LIST_DIR}/Fixtures/Empty" _got_empty
)
expect_eq("${_got_empty}" "" "empty fixture")

if(_failures GREATER 0)
  message(FATAL_ERROR "${_failures} collection check(s) failed")
endif()
message(STATUS "CollectTest: all checks passed")
