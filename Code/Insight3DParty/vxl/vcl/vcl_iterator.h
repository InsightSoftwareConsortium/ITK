#ifndef vcl_iterator_h_
#define vcl_iterator_h_

#include "vcl_compiler.h"

// This file should aim to provide at least the following :
//
//   vcl_iterator_traits<>
//   vcl_iterator<>
//   vcl_reverse_iterator<>
//   vcl_istream_iterator<>
//   vcl_ostream_iterator<>
//   vcl_back_insert_iterator<>
//   vcl_front_insert_iterator<>
//   vcl_insert_iterator<>
//
//   vcl_input_iterator_tag
//   vcl_output_iterator_tag
//   vcl_forward_iterator_tag : public vcl_iterator_tag
//   vcl_bidirectional_iterator_tag : public vcl_forward_iterator_tag
//   vcl_random_access_iterator_tag : public vcl_bidirectional_iterator_tag
//
//   vcl_advance()
//   vcl_distance()
//
// Note that there is no ISO forward_iterator<>; it's just called iterator<>.

// ---------- emulation
#if !VCL_USE_NATIVE_STL
# include "emulation/vcl_iterator.h"
# define vcl_iterator forward_iterator


// ---------- later versions of gcc, e.g. egcs and 2.95
#elif defined(VCL_GCC_EGCS) && !defined(GNU_LIBSTDCXX_V3)
# include <iterator.h>
# define vcl_iterator_traits       /*std::*/iterator_traits
# define vcl_iterator              /*non-std*/forward_iterator
# define vcl_reverse_iterator      /*std::*/reverse_iterator
# define vcl_istream_iterator      /*std::*/istream_iterator
# define vcl_ostream_iterator      /*std::*/ostream_iterator
# define vcl_back_insert_iterator  /*std::*/back_insert_iterator
# define vcl_front_insert_iterator /*std::*/front_insert_iterator
# define vcl_insert_iterator       /*std::*/insert_iterator

# define vcl_input_iterator_tag         /*std::*/input_iterator_tag
# define vcl_output_iterator_tag        /*std::*/output_iterator_tag
# define vcl_forward_iterator_tag       /*std::*/forward_iterator_tag
# define vcl_bidirectional_iterator_tag /*std::*/bidirectional_iterator_tag
# define vcl_random_access_iterator_tag /*std::*/random_access_iterator_tag

# define vcl_advance  /*std::*/advance
# define vcl_distance /*std::*/distance


// ---------- ISO
#else
# include "iso/vcl_iterator.h"
// SUNPRO 5 has no reverse_bidirectional_iterator
// vc has no raw_storage_iterator
// using std::raw_storage_iterator;
// #define vcl_reverse_bidirectional_iterator reverse_bidirectional_iterator
// #define vcl_raw_storage_iterator raw_storage_iterator
#endif

// Needed for emulation STL with WinNT?
#ifdef VCL_WIN32
# define vcl_iterator_category_Iter_cat
#endif

#endif // vcl_iterator_h_
