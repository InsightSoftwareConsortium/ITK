
set(WRAP_ITK_LIBRARIES )

foreach(m ${ITK_CONFIG_MODULES_ENABLED})
  if(EXISTS "${${m}_SOURCE_DIR}/wrapping")
    set(WRAP_ITK_LIBRARIES ${WRAP_ITK_LIBRARIES} ${m})
    add_subdirectory("${${m}_SOURCE_DIR}/wrapping" ${CMAKE_CURRENT_BINARY_DIR}/${m})
  endif()
endforeach(m)
set(WRAP_ITK_LIBRARIES ${WRAP_ITK_LIBRARIES} CACHE INTERNAL "Internal library list.")
