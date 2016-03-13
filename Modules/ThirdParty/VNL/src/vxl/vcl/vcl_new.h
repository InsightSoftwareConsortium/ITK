#ifndef vcl_new_h_
#define vcl_new_h_

#include <new>
#include "vcl_compiler.h"

// Provide vcl_destroy() and vcl_construct() :
template <class T>
inline
void vcl_destroy(T *p) { p->~T(); }

template <class U, class V>
inline
void vcl_construct(U * p, V const & value) { new (p) U(value); }

#endif // vcl_new_h_
