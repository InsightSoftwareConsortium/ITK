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
  vcl_legacy_aliases.h
  vcl_deprecated.cxx
  vcl_deprecated.h

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
  vcl_msvc_warnings.h
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
  
  # Compiler specific files produced by WriteCompilerDetectionHeader
  vcl_compiler_detection.h
  compilers/VXL_COMPILER_INFO_AppleClang_CXX.h
  compilers/VXL_COMPILER_INFO_Clang_CXX.h
  compilers/VXL_COMPILER_INFO_GNU_CXX.h
  compilers/VXL_COMPILER_INFO_Intel_CXX.h
  compilers/VXL_COMPILER_INFO_MSVC_CXX.h
  compilers/VXL_COMPILER_INFO_SunPro_CXX.h
)


# We use implicit instantiation of the standard library now,
# so we shouldn't be explicitly instantiating anything.
# This also avoid trouble where the instantiation hack conflict
# with modern, more compliant compilers.
#aux_source_directory(Templates vcl_sources)
vxl_add_library(LIBRARY_NAME ${VXL_LIB_PREFIX}vcl
  LIBRARY_SOURCES ${vcl_sources}
  )
if(UNIX)
  target_link_libraries(${VXL_LIB_PREFIX}vcl m)
endif()

if( BUILD_TESTING)
  add_subdirectory(tests)
endif()

if (VXL_BUILD_OBJECT_LIBRARIES)
  add_library(vcl-obj OBJECT ${vcl_sources})
endif()
