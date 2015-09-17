
unset(WRAP_ITK_MODULES CACHE)

if(ITK_SOURCE_DIR)
  foreach(m ${ITK_CONFIG_MODULES_ENABLED})
    if(EXISTS "${${m}_SOURCE_DIR}/wrapping/CMakeLists.txt")
      list(APPEND WRAP_ITK_MODULES ${m})
      add_subdirectory("${${m}_SOURCE_DIR}/wrapping" ${CMAKE_CURRENT_BINARY_DIR}/${m})
    endif()
  endforeach()
else() # Building a module externally
  if(EXISTS "${CMAKE_SOURCE_DIR}/wrapping/CMakeLists.txt")
    list(APPEND WRAP_ITK_MODULES ${itk-module} ${ITK_MODULE_${itk-module}_DEPENDS})
    add_subdirectory("${CMAKE_SOURCE_DIR}/wrapping" ${CMAKE_CURRENT_BINARY_DIR}/${itk-module})
  endif()
endif()
set(WRAP_ITK_MODULES ${WRAP_ITK_MODULES} CACHE INTERNAL "Internal library list.")
