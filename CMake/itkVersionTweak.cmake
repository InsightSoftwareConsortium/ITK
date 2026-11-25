# Compute ITK_VERSION_TWEAK: the YYYYMMDD date of the most recent commit, at
# configure time. The tweak must never reach itkConfigure.h, so a per-commit
# change of this value cannot invalidate ccache.
#
# .git_archival.txt is consulted before git so that an archive, or a fork that
# commits a substituted file, pins the date it declares. In this repository the
# file holds unexpanded $Format:$ placeholders, so a working tree falls through
# to `git log`, which succeeds even in a shallow or tagless clone.
function(_itk_version_tweak_from_date date outvar)
  if(date MATCHES "([0-9][0-9][0-9][0-9])-([0-9][0-9])-([0-9][0-9])")
    set(
      ${outvar}
      "${CMAKE_MATCH_1}${CMAKE_MATCH_2}${CMAKE_MATCH_3}"
      PARENT_SCOPE
    )
  else()
    set(${outvar} "" PARENT_SCOPE)
  endif()
endfunction()

function(_itk_compute_version_tweak outvar)
  set(tweak "")

  if(EXISTS "${ITK_SOURCE_DIR}/.git_archival.txt")
    file(
      STRINGS
      "${ITK_SOURCE_DIR}/.git_archival.txt"
      archival
      REGEX "^node-date:"
    )
    if(archival)
      list(GET archival 0 node_date)
      # Unexpanded placeholders mean this is a checkout, not an archive.
      if(NOT node_date MATCHES "\\$Format:")
        _itk_version_tweak_from_date("${node_date}" tweak)
      endif()
    endif()
  endif()

  if(NOT tweak MATCHES "^[0-9]+$")
    find_package(Git QUIET)
    if(Git_FOUND)
      execute_process(
        COMMAND
          "${GIT_EXECUTABLE}" -C "${ITK_SOURCE_DIR}" log -1 --format=%cd
          --date=short
        OUTPUT_VARIABLE commit_date
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
      )
      _itk_version_tweak_from_date("${commit_date}" tweak)
    endif()
  endif()

  # Neither archive metadata nor git history, e.g. an unpacked plain tarball.
  if(NOT tweak MATCHES "^[0-9]+$")
    set(tweak "0")
  endif()
  set(${outvar} "${tweak}" PARENT_SCOPE)
endfunction()

_itk_compute_version_tweak(ITK_VERSION_TWEAK)
set(ITK_VERSION_FULL "${ITK_VERSION}.${ITK_VERSION_TWEAK}")
