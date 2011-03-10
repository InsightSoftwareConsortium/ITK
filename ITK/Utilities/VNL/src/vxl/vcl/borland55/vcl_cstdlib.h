#ifndef vcl_borland_cstdlib_h_
#define vcl_borland_cstdlib_h_
// This is a first attempt at a <cstdlib> for the Borland compiler - PVr,Dec.2003.
#include <cstdlib>

// If we define vcl_abs, for example, to ::abs, we have conflicts
// with std::abs(std::complex<T>) which *is* declared in the
// std namespace. To avoid these issues, we inject the math
// functions into the std namespace.

namespace std {
  //inline int abs(int x) { return x >= 0 ? x : -x; }
  inline long abs(long x) { return x >= 0 ? x : -x; }
  inline long labs(long x) { return x >= 0 ? x : -x; }
  // where do these functions live with the Borland compiler?
  void abort();
  void qsort(void*, size_t, size_t, int(*)(const void*, const void*));
  void* malloc(size_t);
  void* calloc(size_t, size_t);
  void* realloc(void*, size_t);
  void free(void*);
}

#ifndef vcl_abs
#define vcl_abs std::abs
#endif
#ifndef vcl_labs
#define vcl_labs std::labs
#endif
#ifndef vcl_div
#define vcl_div std::div
#endif
#ifndef vcl_ldiv
#define vcl_ldiv std::ldiv
#endif
#ifndef vcl_abort
#define vcl_abort ::abort
#endif
#ifndef vcl_qsort
#define vcl_qsort ::qsort
#endif
#ifndef vcl_malloc
#define vcl_malloc ::malloc
#endif
#ifndef vcl_calloc
#define vcl_calloc ::calloc
#endif
#ifndef vcl_realloc
#define vcl_realloc ::realloc
#endif
#ifndef vcl_free
#define vcl_free ::free
#endif

#define vcl_generic_cstdlib_STD

#include "../generic/vcl_cstdlib.h"

#endif // vcl_borland_cstdlib_h_
