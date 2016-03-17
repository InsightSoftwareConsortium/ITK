#ifndef vcl_new_h_
#define vcl_new_h_
/*
  fsm
*/

#include "vcl_compiler.h"

#include <new>

// Provide vcl_destroy() and vcl_construct() :
template <class T>
inline
void vcl_destroy(T *p) { p->~T(); }

template <class U, class V>
inline
void vcl_construct(U * p, V const & value) { new (p) U(value); }

#define vcl_bad_alloc std::bad_alloc
#define vcl_set_new_handler std::set_new_handler

#endif // vcl_new_h_
