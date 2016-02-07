link_libraries(netlib)

include(${VXL_CMAKE_DIR}/FindNetlib.cmake)
if(NETLIB_FOUND)
  include_directories(${NETLIB_INCLUDE_DIR})

  find_package(X11)
  if(X11_FOUND)
    include_directories(${X11_INCLUDE_DIR})
    add_executable(netlib_showme showme.c)
    target_link_libraries(netlib_showme ${X11_LIBRARIES})
  endif()

endif()
