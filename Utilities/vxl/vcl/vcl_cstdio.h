#ifndef vcl_cstdio_h_
#define vcl_cstdio_h_
/*
  fsm
*/

#include "vcl_compiler.h"

#if !VCL_CXX_HAS_HEADER_CSTDIO
# include <stdio.h>
# define vcl_generic_cstdio_STD /* */
# include "generic/vcl_cstdio.h"
#elif defined(VCL_SUNPRO_CC_50)
# include <iosfwd> // <cstdio> breaks <iosfwd>
# include "iso/vcl_cstdio.h"
#elif defined(VCL_BORLAND_55)
# include <stdio.h>
# define vcl_generic_cstdio_STD /* */
# include "generic/vcl_cstdio.h"
#elif defined(VCL_VC60)
# include <cstdio>
# define vcl_generic_cstdio_STD /**/
# include "generic/vcl_cstdio.h"
#elif defined(VCL_METRO_WERKS)
# include <cstdio>
# define vcl_generic_cstdio_STD /* */
# include "generic/vcl_cstdio.h"
#else
# include "vcl_cstddef.h" // for size_t
# include "iso/vcl_cstdio.h"
#endif

// Some compilers (gcc 2.95.3, SGI CC) seem to define these as macros. Sigh.
#ifdef putchar
# undef vcl_putchar
inline int vcl_putchar(int x) { return putchar(x); }
# define vcl_putchar vcl_putchar
#endif
#ifdef putc
# undef vcl_putc
inline int vcl_putc(int x, vcl_FILE* f) { return putc(x,f); }
# define vcl_putc vcl_putc
#endif
#ifdef getchar
# undef vcl_getchar
inline int vcl_getchar(void) { return getchar(); }
# define vcl_getchar vcl_getchar
#endif
#ifdef getc
# undef vcl_getc
inline int vcl_getc(vcl_FILE* f) { return getc(f); }
# define vcl_getc vcl_getc
#endif
#ifdef feof
# undef vcl_feof
inline int vcl_feof(vcl_FILE* f) { return feof(f); }
# define vcl_feof vcl_feof
#endif
#ifdef ferror
# undef vcl_ferror
inline int vcl_ferror(vcl_FILE* f) { return ferror(f); }
# define vcl_ferror vcl_ferror
#endif
#ifdef clearerr
# undef vcl_clearerr
inline void vcl_clearerr(vcl_FILE* f) { clearerr(f); }
# define vcl_clearerr vcl_clearerr
#endif

#endif // vcl_cstdio_h_
