#ifndef vxl_config_h_config_win32_vc60_
#define vxl_config_h_config_win32_vc60_

// This file was *not* generated from vxl_config.h.in
// It is maintained manually.

// these are 0 or 1, never empty.
#define VXL_LITTLE_ENDIAN 1
#define VXL_BIG_ENDIAN    0

typedef char      vxl_int_8;
typedef short     vxl_int_16;
typedef int       vxl_int_32;
typedef void      vxl_int_64;           // !

typedef unsigned char      vxl_uint_8;
typedef unsigned short     vxl_uint_16;
typedef unsigned int       vxl_uint_32;
typedef void               vxl_uint_64; // !

typedef signed char      vxl_sint_8;
typedef signed short     vxl_sint_16;
typedef signed int       vxl_sint_32;
typedef void             vxl_sint_64;   // !

typedef float       vxl_ieee_32;
typedef double      vxl_ieee_64;
//typedef long double vxl_ieee_96;  // ?
//typedef long double vxl_ieee_128; // ?

// -------------------- library quirks

// these should be 1 if the symbol in question is declared
// in the relevant header file and 0 otherwise.

#define VXL_UNISTD_HAS_USECONDS_T 0
#define VXL_UNISTD_HAS_INTPTR_T   0
#define VXL_UNISTD_HAS_UALARM     1
#define VXL_UNISTD_HAS_USLEEP     1
#define VXL_UNISTD_HAS_LCHOWN     1
#define VXL_UNISTD_HAS_PREAD      1
#define VXL_UNISTD_HAS_PWRITE     1
#define VXL_UNISTD_HAS_TELL       1

// true if <stdlib.h> declares qsort()
#define VXL_STDLIB_HAS_QSORT      1

// true if <stdlib.h> declares lrand48()
#define VXL_STDLIB_HAS_LRAND48    1 // ?

// true if <stdlib.h> declares drand48()
#define VXL_STDLIB_HAS_DRAND48    0

// true if <ieeefp.h> declares finite()
#define VXL_IEEEFP_HAS_FINITE     0 // ?

// true if <math.h> declares finite()
#define VXL_MATH_HAS_FINITE       0 // ?

// true if usleep() returns void
#define VXL_UNISTD_USLEEP_IS_VOID 0

// true if gettime() takes two arguments
#define VXL_TWO_ARG_GETTIME       0

// true if <ieeefp.h> is available
#define VXL_HAS_IEEEFP_H          1


#if 0
// -------------------- availability of 3rd party packages

// these should be 0 or 1, never empty.
#define VXL_HAS_JPEG   0
#define VXL_HAS_TIFF   0
#define VXL_HAS_PNG    0
#define VXL_HAS_HERMES 0
#define VXL_HAS_OPENGL 1
#define VXL_HAS_GLUT   1
#define VXL_HAS_FLTK   0
#define VXL_HAS_GTK    1
#define VXL_HAS_SDL    0
#endif

#endif // vxl_config_h_config_win32_vc60_
