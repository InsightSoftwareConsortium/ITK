#ifndef vcl_vc_cstdlib_h_
#define vcl_vc_cstdlib_h_


#include <cstdlib>

// abs
#ifndef vcl_abs
#define vcl_abs ::abs
#endif
// div
#ifndef vcl_div
#define vcl_div ::div
#endif
// labs
#ifndef vcl_labs
#define vcl_labs ::labs
#endif
// ldiv
#ifndef vcl_ldiv
#define vcl_ldiv ::ldiv
#endif
// srand
#ifndef vcl_srand
#define vcl_srand ::srand
#endif
// rand
#ifndef vcl_rand
#define vcl_rand ::rand
#endif
// atexit
#ifndef vcl_atexit
#define vcl_atexit ::atexit
#endif
// getenv
#ifndef vcl_getenv
#define vcl_getenv ::getenv
#endif
// system
#ifndef vcl_system
#define vcl_system ::system
#endif
// exit
#ifndef vcl_exit
#define vcl_exit ::exit
#endif
// abort
#ifndef vcl_abort
#define vcl_abort ::abort
#endif
// qsort
#ifndef vcl_qsort
#define vcl_qsort ::qsort
#endif
// calloc
#ifndef vcl_calloc
#define vcl_calloc ::calloc
#endif
// malloc
#ifndef vcl_malloc
#define vcl_malloc ::malloc
#endif
// free
#ifndef vcl_free
#define vcl_free ::free
#endif
// realloc
#ifndef vcl_realloc
#define vcl_realloc ::realloc
#endif
// atol
#ifndef vcl_atol
#define vcl_atol ::atol
#endif
// atof
#ifndef vcl_atof
#define vcl_atof ::atof
#endif
// atoi
#ifndef vcl_atoi
#define vcl_atoi ::atoi
#endif
// mblen
#ifndef vcl_mblen
#define vcl_mblen ::mblen
#endif
// mbstowcs
#ifndef vcl_mbstowcs
#define vcl_mbstowcs ::mbstowcs
#endif
// mbtowc
#ifndef vcl_mbtowc
#define vcl_mbtowc ::mbtowc
#endif
// strtod
#ifndef vcl_strtod
#define vcl_strtod ::strtod
#endif
// strtol
#ifndef vcl_strtol
#define vcl_strtol ::strtol
#endif
// strtoul
#ifndef vcl_strtoul
#define vcl_strtoul ::strtoul
#endif
// wctomb
#ifndef vcl_wctomb
#define vcl_wctomb ::wctomb
#endif
// wcstombs
#ifndef vcl_wcstombs
#define vcl_wcstombs ::wcstombs
#endif

#ifdef _DEBUG
// abs is an "intrinsic" if optimizing....
inline int  abs(int  x) { return x >= 0 ? x : -x; }
#endif
inline long double abs(long double x) { return x >= 0 ? x : -x; }
inline long abs(long x) { return x >= 0 ? x : -x; }

#endif // vcl_cstdlib_h_
