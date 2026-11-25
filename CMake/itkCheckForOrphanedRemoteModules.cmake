#
# Detect remote-module clones left in the source tree without a controlling
# <name>.remote.cmake. Such orphans are discovered by the module DAG glob and
# break the build, commonly after a module is ingested into ITK core.
function(itk_check_for_orphaned_remote_modules)
  set(_remote_dir "${ITK_SOURCE_DIR}/Modules/Remote")
  file(GLOB _entries CONFIGURE_DEPENDS "${_remote_dir}/*")

  set(_orphans)
  foreach(_entry ${_entries})
    if(NOT IS_DIRECTORY "${_entry}")
      continue()
    endif()
    get_filename_component(_name "${_entry}" NAME)
    string(REGEX REPLACE "^ITK" "" _name "${_name}")
    if(
      _name
        STREQUAL
        "Deprecated"
      OR
        _name
          STREQUAL
          "ITK-Wasm"
      OR
        _name
          STREQUAL
          "scifio-imageio"
    )
      continue()
    endif()
    if(
      NOT
        EXISTS
          "${_remote_dir}/${_name}.remote.cmake"
      AND
        NOT
          EXISTS
            "${_remote_dir}/Deprecated/${_name}.remote.cmake"
    )
      list(APPEND _orphans "${_entry}")
    endif()
  endforeach()

  if(_orphans)
    set(_rm_lines)
    foreach(_orphan ${_orphans})
      if(CMAKE_HOST_WIN32)
        string(APPEND _rm_lines "    rmdir /s /q \"${_orphan}\"\n")
      else()
        string(APPEND _rm_lines "    rm -rf \"${_orphan}\"\n")
      endif()
    endforeach()
    message(
      FATAL_ERROR
      "Stale (orphaned) remote-module source directories detected.\n"
      "These were fetched into the source tree but no longer have a "
      "controlling <name>.remote.cmake (the module was likely ingested into "
      "ITK core). They are picked up by the module dependency scan and will "
      "break the build. Delete them and re-configure:\n\n"
      "${_rm_lines}"
    )
  endif()
endfunction()

itk_check_for_orphaned_remote_modules()
