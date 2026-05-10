# vcl/CMakeLists.txt

if(WIN32)
  # MSVC.NET produces lots of warnings like
  # "warning LNK4221: no public symbols found; archive member will be inaccessible"
  # that depend on subtle logic at compile time.
  # This is a bit indescriminate, but it may be the only feasible way to suppress them.
  if(CMAKE_CXX_COMPILER MATCHES "^cl$")
    set( CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} /WARN:0")
    set( CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} /WARN:0")
  endif()
endif()


project( vcl )

# If VXL_INSTALL_INCLUDE_DIR is the default value
if("${VXL_INSTALL_INCLUDE_DIR}" STREQUAL "include/vxl")
  set(_config_destination ${VXL_INSTALL_INCLUDE_DIR}/vcl)
else()
  set(_config_destination ${VXL_INSTALL_INCLUDE_DIR})
endif()
vxl_configure_file(${CMAKE_CURRENT_LIST_DIR}/vcl_compiler.h.in ${PROJECT_BINARY_DIR}/vcl_compiler.h ${_config_destination})
vxl_configure_file(${CMAKE_CURRENT_LIST_DIR}/vcl_where_root_dir.h.in ${PROJECT_BINARY_DIR}/vcl_where_root_dir.h  ${_config_destination})

include_directories(${PROJECT_BINARY_DIR})
set( vcl_sources
  vcl.cxx
  vcl_legacy_aliases.h

  vcl_deque.h
  vcl_msvc_warnings.h
  vcl_vector.h
  vcl_sys/time.h
  vcl_sys/types.h

  # The following shall not be used other than as reference count for smart pointers
  vcl_atomic_count.h
  internal/vcl_atomic_count_gcc.h
  internal/vcl_atomic_count_gcc_x86.h
  internal/vcl_atomic_count_pthreads.h
  internal/vcl_atomic_count_solaris.h
  internal/vcl_atomic_count_sync.h
  internal/vcl_atomic_count_win32.h
  internal/vcl_interlocked.h

)


# We use implicit instantiation of the standard library now,
# so we shouldn't be explicitly instantiating anything.
# This also avoid trouble where the instantiation hack conflict
# with modern, more compliant compilers.
#aux_source_directory(Templates vcl_sources)
vxl_add_library(LIBRARY_NAME ${VXL_LIB_PREFIX}vcl
  LIBRARY_SOURCES ${vcl_sources}
  )
target_link_libraries(${VXL_LIB_PREFIX}vcl PRIVATE vxl_platform_math )

if( BUILD_TESTING)
  add_subdirectory(tests)
endif()

if (VXL_BUILD_OBJECT_LIBRARIES)
  add_library(vcl-obj OBJECT ${vcl_sources})
endif()
