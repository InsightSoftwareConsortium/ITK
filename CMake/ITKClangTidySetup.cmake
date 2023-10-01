# Provides itk_clangtidy_setup to assist with maintaining consistent
# c++ best practices across the ITK toolkit
#
# Clang-tidy version 8.0 or greater is required
#
# The ITK C++ best practices guidelines are represented by clang-tidy
# rules defined in ${ITK_SOURCE_DIR}/.clang-tidy
#
option(ITK_USE_CLANGTIDY "Enable the use of clang-tidy to enforce coding best practices." OFF)
mark_as_advanced(ITK_USE_CLANGTIDY)

if(ITK_USE_CLANGTIDY)
  if(ITK_USE_CLANGTIDY AND NOT EXISTS "${CLANGTIDY_EXECUTABLE}")
    find_program(
      CLANGTIDY_EXECUTABLE
      NAMES clang-tidy-13
            clang-tidy-12
            clang-tidy-11
            clang-tidy-10
            clang-tidy-9
            clang-tidy-9
            clang-tidy-8
            clang-tidy)
  endif()

  if(CLANGTIDY_EXECUTABLE AND EXISTS "${CLANGTIDY_EXECUTABLE}")
    mark_as_advanced(CLANGTIDY_EXECUTABLE)
    set(CLANG_TIDY_CHECKS "-*,modernize-use-*")
    set(CMAKE_CXX_CLANG_TIDY
        "${CLANGTIDY_EXECUTABLE};-checks=${CLANG_TIDY_CHECKS};-header-filter='${CMAKE_SOURCE_DIR}/Modules/*/*/include'")

    # Compile commands are required by clang-tidy
    set(CMAKE_EXPORT_COMPILE_COMMANDS
        ON
        CACHE BOOL "compile_commands.json file is needed by clang-tidy")
  else()
    unset(CLANGTIDY_EXECUTABLE)
    unset(CMAKE_CXX_CLANG_TIDY)
    message(FATAL_ERROR "Missing suitable clang-tidy executable, set CLANGTIDY_EXECUTABLE variable to desired path")
  endif()
endif()
