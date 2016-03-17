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

// ---------- ISO
# include "iso/vcl_iterator.h"
// vc has no raw_storage_iterator
#include "vcl_iterator.hxx"

#endif // vcl_iterator_h_
