#ifndef vcl_win32_cstdio_h_
#define vcl_win32_cstdio_h_

#include <cstdio>

// FILE
#ifndef vcl_FILE
#define vcl_FILE ::FILE
#endif
// fpos_t
#ifndef vcl_fpos_t
#define vcl_fpos_t ::fpos_t
#endif
// size_t
#ifndef vcl_size_t
#define vcl_size_t ::size_t
#endif
// clearerr
#ifndef vcl_clearerr
#define vcl_clearerr ::clearerr
#endif
// fclose
#ifndef vcl_fclose
#define vcl_fclose ::fclose
#endif
// feof
#ifndef vcl_feof
#define vcl_feof ::feof
#endif
// ferror
#ifndef vcl_ferror
#define vcl_ferror ::ferror
#endif
// fflush
#ifndef vcl_fflush
#define vcl_fflush ::fflush
#endif
// fgetc
#ifndef vcl_fgetc
#define vcl_fgetc ::fgetc
#endif
// fgetpos
#ifndef vcl_fgetpos
#define vcl_fgetpos ::fgetpos
#endif
// fgets
#ifndef vcl_fgets
#define vcl_fgets ::fgets
#endif
// ...
// printf
#ifndef vcl_printf
#define vcl_printf ::printf
#endif
// sprintf
#ifndef vcl_sprintf
#define vcl_sprintf ::sprintf
#endif
// fprintf
#ifndef vcl_fprintf
#define vcl_fprintf ::fprintf
#endif
// vprintf
#ifndef vcl_vprintf
#define vcl_vprintf ::vprintf
#endif
// vsprintf
#ifndef vcl_vsprintf
#define vcl_vsprintf ::vsprintf
#endif
// vfprintf
#ifndef vcl_vfprintf
#define vcl_vfprintf ::vfprintf
#endif

#endif // vcl_iso_cstdio_h_
