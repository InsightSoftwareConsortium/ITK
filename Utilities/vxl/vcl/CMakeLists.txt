# vcl/CMakeLists.txt

PROJECT( vcl )

SET( VCL_INCLUDE_DIR ${VCL_SOURCE_DIR} CACHE STATIC "VCL include files" )

# Set up vcl_where_root_dir.h to have a macro set to $VCL_SOURCE_ROOT_DIR
CONFIGURE_FILE(
      ${vxl_SOURCE_DIR}/vcl/vcl_where_root_dir.h.in
      ${vxl_BINARY_DIR}/vcl/vcl_where_root_dir.h ESCAPE_QUOTES
)

SET( vcl_sources
  vcl_compiler.h
  vcl_deprecated.cxx vcl_deprecated.h

  vcl_where_root_dir.h.in
  vcl_config_compiler.h.in
  vcl_config_headers.h.in
  vcl_config_manual.h.in

  vcl_cmath.cxx vcl_cmath.h
  vcl_cassert.cxx vcl_cassert.h
  vcl_cstdlib.cxx vcl_cstdlib.h
  vcl_algorithm.txx vcl_algorithm.h
  vcl_bitset.h
  vcl_cctype.h
  vcl_cerrno.h
  vcl_cfloat.h
  vcl_ciso646.h
  vcl_climits.h
  vcl_clocale.h
  vcl_complex.txx vcl_complex.h
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
  vcl_deque.txx vcl_deque.h
  vcl_exception.h
  vcl_fstream.h
  vcl_functional.txx vcl_functional.h
  vcl_iomanip.h
  vcl_ios.h
  vcl_iosfwd.h
  vcl_iostream.h
  vcl_istream.h
  vcl_iterator.txx vcl_iterator.h
  vcl_limits.h
  vcl_list.txx vcl_list.h
  vcl_locale.h
  vcl_map.txx vcl_map.h
  vcl_memory.h
  vcl_new.h
  vcl_numeric.h
  vcl_ostream.h
  vcl_queue.txx vcl_queue.h
  vcl_set.txx vcl_set.h
  vcl_sstream.h
  vcl_stack.txx vcl_stack.h
  vcl_stdexcept.h
  vcl_stlfwd.h
  vcl_streambuf.h
  vcl_string.txx vcl_string.h
  vcl_typeinfo.h
  vcl_utility.txx vcl_utility.h
  vcl_valarray.h
  vcl_vector.txx vcl_vector.h
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

  iso/vcl_algorithm.txx iso/vcl_algorithm.h
  iso/vcl_complex.txx iso/vcl_complex.h
  iso/vcl_deque.txx iso/vcl_deque.h
  iso/vcl_functional.txx iso/vcl_functional.h
  iso/vcl_iterator.txx iso/vcl_iterator.h
  iso/vcl_list.txx iso/vcl_list.h
  iso/vcl_map.txx iso/vcl_map.h
  iso/vcl_queue.txx iso/vcl_queue.h
  iso/vcl_set.txx iso/vcl_set.h
  iso/vcl_stack.txx iso/vcl_stack.h
  iso/vcl_string.txx iso/vcl_string.h
  iso/vcl_utility.txx iso/vcl_utility.h
  iso/vcl_vector.txx iso/vcl_vector.h
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
  iso/vcl_new.h
  iso/vcl_numeric.h
  iso/vcl_ostream.h
  iso/vcl_sstream.h
  iso/vcl_stdexcept.h
  iso/vcl_streambuf.h
  iso/vcl_typeinfo.h
  iso/vcl_valarray.h

  emulation/vcl_string_instances.cxx
  emulation/vcl_straits.cxx emulation/vcl_straits.h
  emulation/vcl_rbtree_instances.cxx
  emulation/vcl_hashtable.cxx emulation/vcl_hashtable.h
  emulation/vcl_alloc.cxx emulation/vcl_alloc.h
  emulation/vcl_algorithm.txx emulation/vcl_algorithm.h
  emulation/vcl_complex.txx emulation/vcl_complex.h
  emulation/vcl_deque.txx emulation/vcl_deque.h
  emulation/vcl_functional.txx emulation/vcl_functional.h
  emulation/vcl_hash.txx
  emulation/vcl_hash_map.txx emulation/vcl_hash_map.h
  emulation/vcl_iterator.txx emulation/vcl_iterator.h
  emulation/vcl_list.txx emulation/vcl_list.h
  emulation/vcl_map.txx emulation/vcl_map.h
  emulation/vcl_multimap.txx emulation/vcl_multimap.h
  emulation/vcl_rbtree.txx
  emulation/vcl_set.txx emulation/vcl_set.h
  emulation/vcl_string.txx emulation/vcl_string.h
  emulation/vcl_utility.txx emulation/vcl_utility.h
  emulation/vcl_vector.txx emulation/vcl_vector.h
  emulation/vcl_algobase.h
  emulation/vcl_bool.h
  emulation/vcl_bvector.h
  emulation/vcl_complex_fwd.h
  emulation/vcl_defalloc.h
  emulation/vcl_functionx.h
  emulation/vcl_hash_set.h
  emulation/vcl_heap.h
  emulation/vcl_multiset.h
  emulation/vcl_new.h
  emulation/vcl_pair.h
  emulation/vcl_rel_ops.h
  emulation/vcl_stack.h
  emulation/vcl_stlfwd.h
  emulation/vcl_tempbuf.h
  emulation/vcl_tree.h

  emulation/vcl_stlconf.h
  emulation/gcc-272-stlconf.h
  emulation/gcc-2800-stlconf.h
  emulation/gcc-281-stlconf.h
  emulation/gcc-295-stlconf.h
  emulation/sgi-CC-stlconf.h
  emulation/sun-CC4.1-stlconf.h
  emulation/sun-CC5.0-stlconf.h
  emulation/win32-vc50-stlconf.h
  emulation/stlcomp.h

  gcc/vcl_cmath.h
  gcc/vcl_cstdlib.h
  gcc-295/vcl_algorithm.txx
  gcc-295/vcl_complex.txx
  gcc-295/vcl_deque.txx
  gcc-295/vcl_functional.txx
  gcc-295/vcl_iterator.txx
  gcc-295/vcl_list.txx
  gcc-295/vcl_map.txx
  gcc-295/vcl_queue.txx
  gcc-295/vcl_set.txx
  gcc-295/vcl_string.txx
  gcc-295/vcl_utility.txx
  gcc-295/vcl_vector.txx
  gcc-295/vcl_iomanip.h
  gcc-295/vcl_ios.h
  gcc-libstdcxx-v3/vcl_algorithm.txx
  gcc-libstdcxx-v3/vcl_deque.txx
  gcc-libstdcxx-v3/vcl_functional.txx
  gcc-libstdcxx-v3/vcl_iterator.txx
  gcc-libstdcxx-v3/vcl_list.txx
  gcc-libstdcxx-v3/vcl_map.txx
  gcc-libstdcxx-v3/vcl_set.txx
  gcc-libstdcxx-v3/vcl_string.txx
  gcc-libstdcxx-v3/vcl_utility.txx
  gcc-libstdcxx-v3/vcl_vector.txx
  egcs/vcl_algorithm.txx
  egcs/vcl_complex.txx
  egcs/vcl_deque.txx
  egcs/vcl_functional.txx
  egcs/vcl_iterator.txx
  egcs/vcl_list.txx
  egcs/vcl_map.txx
  egcs/vcl_set.txx
  egcs/vcl_string.txx
  egcs/vcl_utility.txx
  egcs/vcl_vector.txx

  sgi/vcl_algorithm.txx
  sgi/vcl_bitset.h
  sgi/vcl_cmath.h
  sgi/vcl_complex.txx sgi/vcl_complex.h
  sgi/vcl_cstdlib.h
  sgi/vcl_deque.txx
  sgi/vcl_functional.txx
  sgi/vcl_iomanip.h
  sgi/vcl_ios.h
  sgi/vcl_iostream.h
  sgi/vcl_iterator.txx
  sgi/vcl_list.txx
  sgi/vcl_map.txx
  sgi/vcl_set.txx
  sgi/vcl_string.txx sgi/vcl_string.h
  sgi/vcl_utility.txx
  sgi/vcl_vector.txx

  sunpro/vcl_algorithm.txx
  sunpro/vcl_cmath.h
  sunpro/vcl_complex.txx sunpro/vcl_complex.h
  sunpro/vcl_complex_fwd.h
  sunpro/vcl_cstdlib.h
  sunpro/vcl_deque.txx
  sunpro/vcl_functional.txx
  sunpro/vcl_iterator.txx sunpro/vcl_iterator.h
  sunpro/vcl_list.txx
  sunpro/vcl_map.txx sunpro/vcl_map.h
  sunpro/vcl_set.txx
  sunpro/vcl_stack.txx
  sunpro/vcl_utility.txx
  sunpro/vcl_vector.txx sunpro/vcl_vector.h

  win32-vc60/vcl_cmath.h
  win32-vc60/vcl_complex.h
  win32-vc60/vcl_complex_fwd.h
  win32-vc60/vcl_cstdlib.h
  win32-vc60/vcl_string.h

  win32-vc70/vcl_cmath.h
  win32-vc70/vcl_cstdlib.h

  mwerks/vcl_cmath.h
  mwerks/vcl_complex.h
  mwerks/vcl_cstdlib.h
)

AUX_SOURCE_DIRECTORY(Templates vcl_sources)

ADD_LIBRARY(vcl ${vcl_sources})

IF(WIN32)
  IF(NOT CYGWIN)
    ADD_DEFINITIONS( -DBUILDING_VCL_DLL )
    INCLUDE_DIRECTORIES( config.win32 )
  ENDIF(NOT CYGWIN)
ENDIF(WIN32)

IF(UNIX)
  TARGET_LINK_LIBRARIES( vcl m )
ENDIF(UNIX)

IF( BUILD_TESTING)
  SUBDIRS(tests)
ENDIF( BUILD_TESTING)
