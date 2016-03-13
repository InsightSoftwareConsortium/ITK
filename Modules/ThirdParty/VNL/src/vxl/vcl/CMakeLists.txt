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

#
# Do platform-specific configuration.
# configure files
#
include_directories(${PROJECT_BINARY_DIR})
vxl_configure_file(${CMAKE_CURRENT_LIST_DIR}/vcl_config_compiler.h.in ${PROJECT_BINARY_DIR}/vcl_config_compiler.h include/vxl/vcl)
vxl_configure_file(${CMAKE_CURRENT_LIST_DIR}/vcl_config_headers.h.in  ${PROJECT_BINARY_DIR}/vcl_config_headers.h  include/vxl/vcl)
vxl_configure_file(${CMAKE_CURRENT_LIST_DIR}/vcl_config_manual.h.in   ${PROJECT_BINARY_DIR}/vcl_config_manual.h   include/vxl/vcl)
vxl_configure_file(${CMAKE_CURRENT_LIST_DIR}/vcl_where_root_dir.h.in  ${PROJECT_BINARY_DIR}/vcl_where_root_dir.h  include/vxl/vcl)

set( vcl_sources
  vcl_compiler.h
  vcl_deprecated.cxx     vcl_deprecated.h

  vcl_cmath.h
  vcl_cassert.h
  vcl_cstdlib.h

  vcl_algorithm.h
  vcl_bitset.h
  vcl_cctype.h
  vcl_cerrno.h
  vcl_cfloat.h
  vcl_ciso646.h
  vcl_climits.h
  vcl_clocale.h
  vcl_complex.h
  vcl_csetjmp.h
  vcl_csignal.h
  vcl_cstdarg.h
  vcl_cstddef.h
  vcl_cstdio.h
  vcl_cstring.h
  vcl_ctime.h
  vcl_cwchar.h
  vcl_cwctype.h
  vcl_deprecated_header.h
  vcl_deque.h
  vcl_exception.h
  vcl_fstream.h
  vcl_functional.h
  vcl_iomanip.h
  vcl_ios.h
  vcl_iosfwd.h
  vcl_iostream.h
  vcl_istream.h
  vcl_iterator.h
  vcl_limits.h
  vcl_list.h
  vcl_locale.h
  vcl_map.h
  vcl_memory.h
  vcl_new.h
  vcl_numeric.h
  vcl_ostream.h
  vcl_queue.h
  vcl_set.h
  vcl_sstream.h
  vcl_stack.h
  vcl_stdexcept.h
  vcl_stlfwd.h
  vcl_streambuf.h
  vcl_string.h
  vcl_typeinfo.h
  vcl_utility.h
  vcl_valarray.h
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
  HEADER_INSTALL_DIR vcl)
if(UNIX)
  target_link_libraries(${VXL_LIB_PREFIX}vcl m)
endif()

if( BUILD_TESTING)
  add_subdirectory(tests)
endif()
