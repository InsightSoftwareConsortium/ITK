# Migration sentinels: one Markdown file per downstream-visible change.
#
# The resulting list is exported through ITKConfig.cmake ONLY. It must never
# reach itkConfigure.h or any other compiled header: the value changes on most
# merges, and that header is included nearly everywhere, so embedding it would
# invalidate ccache for every translation unit on every build machine.
function(itk_collect_migration_sentinels _sentinel_dir _out_var)
  # CONFIGURE_DEPENDS is required during a real configure: without it, adding
  # a sentinel file does not re-run CMake and the new sentinel is silently
  # missed. It is rejected in -P script mode (CMAKE_SCRIPT_MODE_FILE set),
  # which the CollectTest.cmake unit test runs under.
  if(CMAKE_SCRIPT_MODE_FILE)
    file(GLOB _sentinel_files "${_sentinel_dir}/ITK_MIGRATION_*.md")
  else()
    file(
      GLOB _sentinel_files
      CONFIGURE_DEPENDS
      "${_sentinel_dir}/ITK_MIGRATION_*.md"
    )
  endif()
  set(_names "")
  foreach(_file IN LISTS _sentinel_files)
    get_filename_component(_name "${_file}" NAME_WE)
    list(APPEND _names "${_name}")
  endforeach()
  list(SORT _names)
  set(${_out_var} "${_names}" PARENT_SCOPE)
endfunction()
