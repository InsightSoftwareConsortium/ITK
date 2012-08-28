
unset(WRAP_ITK_MODULES CACHE)

foreach(m ${ITK_CONFIG_MODULES_ENABLED})
  if(EXISTS "${${m}_SOURCE_DIR}/wrapping/CMakeLists.txt")
    set(WRAP_ITK_MODULES ${WRAP_ITK_MODULES} ${m})
    add_subdirectory("${${m}_SOURCE_DIR}/wrapping" ${CMAKE_CURRENT_BINARY_DIR}/${m})
  endif()
endforeach()
set(WRAP_ITK_MODULES ${WRAP_ITK_MODULES} CACHE INTERNAL "Internal library list.")
