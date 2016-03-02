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

  vcl_cmath.cxx          vcl_cmath.h
  vcl_cassert.cxx        vcl_cassert.h
  vcl_cstdlib.cxx        vcl_cstdlib.h

  vcl_algorithm.hxx      vcl_algorithm.h
  vcl_bitset.h
  vcl_cctype.h
  vcl_cerrno.h
  vcl_cfloat.h
  vcl_ciso646.h
  vcl_climits.h
  vcl_clocale.h
  vcl_complex.hxx        vcl_complex.h
  vcl_complex_fwd.h
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
  vcl_deque.hxx          vcl_deque.h
  vcl_exception.h
  vcl_fstream.h
  vcl_functional.hxx     vcl_functional.h
  vcl_iomanip.h
  vcl_ios.h
  vcl_iosfwd.h
  vcl_iostream.h
  vcl_istream.h
  vcl_iterator.hxx       vcl_iterator.h
  vcl_limits.h
  vcl_list.hxx           vcl_list.h
  vcl_locale.h
  vcl_map.hxx            vcl_map.h
  vcl_memory.h
  vcl_new.h
  vcl_numeric.h
  vcl_ostream.h
  vcl_queue.hxx          vcl_queue.h
  vcl_set.hxx            vcl_set.h
  vcl_sstream.h
  vcl_stack.hxx          vcl_stack.h
  vcl_stdexcept.h
  vcl_stlfwd.h
  vcl_streambuf.h
  vcl_string.hxx         vcl_string.h
  vcl_typeinfo.h
  vcl_utility.hxx        vcl_utility.h
  vcl_valarray.h
  vcl_vector.hxx         vcl_vector.h
  vcl_sys/time.h
  vcl_sys/types.h

  generic/vcl_algorithm.h
  generic/vcl_bitset.h
  generic/vcl_blah.h
  generic/vcl_cassert.h
  generic/vcl_cctype.h
  generic/vcl_cerrno.h
  generic/vcl_cfloat.h
  generic/vcl_ciso646.h
  generic/vcl_climits.h
  generic/vcl_clocale.h
  generic/vcl_cmath.h
  generic/vcl_complex.h
  generic/vcl_csetjmp.h
  generic/vcl_csignal.h
  generic/vcl_cstdarg.h
  generic/vcl_cstddef.h
  generic/vcl_cstdio.h
  generic/vcl_cstdlib.h
  generic/vcl_cstring.h
  generic/vcl_ctime.h
  generic/vcl_cwchar.h
  generic/vcl_cwctype.h
  generic/vcl_deque.h
  generic/vcl_exception.h
  generic/vcl_fstream.h
  generic/vcl_functional.h
  generic/vcl_iomanip.h
  generic/vcl_ios.h
  generic/vcl_iosfwd.h
  generic/vcl_iostream.h
  generic/vcl_istream.h
  generic/vcl_iterator.h
  generic/vcl_limits.h
  generic/vcl_list.h
  generic/vcl_locale.h
  generic/vcl_map.h
  generic/vcl_memory.h
  generic/vcl_memory_tr1.h
  generic/vcl_numeric.h
  generic/vcl_ostream.h
  generic/vcl_queue.h
  generic/vcl_set.h
  generic/vcl_sstream.h
  generic/vcl_stack.h
  generic/vcl_stdexcept.h
  generic/vcl_streambuf.h
  generic/vcl_string.h
  generic/vcl_typeinfo.h
  generic/vcl_utility.h
  generic/vcl_valarray.h
  generic/vcl_vector.h

  iso/vcl_algorithm.hxx  iso/vcl_algorithm.h
  iso/vcl_complex.hxx    iso/vcl_complex.h
  iso/vcl_deque.hxx      iso/vcl_deque.h
  iso/vcl_functional.hxx iso/vcl_functional.h
  iso/vcl_iterator.hxx   iso/vcl_iterator.h
  iso/vcl_list.hxx       iso/vcl_list.h
  iso/vcl_map.hxx        iso/vcl_map.h
  iso/vcl_queue.hxx      iso/vcl_queue.h
  iso/vcl_set.hxx        iso/vcl_set.h
  iso/vcl_stack.hxx      iso/vcl_stack.h
  iso/vcl_string.hxx     iso/vcl_string.h
  iso/vcl_utility.hxx    iso/vcl_utility.h
  iso/vcl_vector.hxx     iso/vcl_vector.h
  iso/vcl_bitset.h
  iso/vcl_cassert.h
  iso/vcl_cctype.h
  iso/vcl_cerrno.h
  iso/vcl_cfloat.h
  iso/vcl_ciso646.h
  iso/vcl_climits.h
  iso/vcl_clocale.h
  iso/vcl_cmath.h
  iso/vcl_csetjmp.h
  iso/vcl_csignal.h
  iso/vcl_cstdarg.h
  iso/vcl_cstddef.h
  iso/vcl_cstdio.h
  iso/vcl_cstdlib.h
  iso/vcl_cstring.h
  iso/vcl_ctime.h
  iso/vcl_cwchar.h
  iso/vcl_cwctype.h
  iso/vcl_exception.h
  iso/vcl_fstream.h
  iso/vcl_iomanip.h
  iso/vcl_ios.h
  iso/vcl_iosfwd.h
  iso/vcl_iostream.h
  iso/vcl_istream.h
  iso/vcl_limits.h
  iso/vcl_locale.h
  iso/vcl_memory.h
  iso/vcl_memory_tr1.h
  iso/vcl_new.h
  iso/vcl_numeric.h
  iso/vcl_ostream.h
  iso/vcl_sstream.h
  iso/vcl_stdexcept.h
  iso/vcl_streambuf.h
  iso/vcl_typeinfo.h
  iso/vcl_valarray.h

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
vxl_add_library(LIBNAME ${VXL_LIB_PREFIX}vcl LIBSRCS ${vcl_sources} )
if(UNIX)
  target_link_libraries( ${VXL_LIB_PREFIX}vcl m )
endif()

if( BUILD_TESTING)
  add_subdirectory(tests)
endif()
