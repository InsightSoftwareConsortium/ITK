#ifndef vxl_config_h_config_win32_vc70_
#define vxl_config_h_config_win32_vc70_

/* This file was *not* generated from vxl_config.h.in */
/* It is maintained manually. */

/* -------------------- machine word characteristics */

/* these are 0 or 1, never empty. */
#define VXL_LITTLE_ENDIAN 1
#define VXL_BIG_ENDIAN    0

/* we can't just use typedefs, because on systems where there are   */
/* no 64bit integers we have to #define vxl_int_64 to `void' in     */
/* order to catch illegitimate uses. However, typedefs are superior */
/* to #defines, especially for the two keyword types, so we use     */
/* typedefs for the valid cases.                                    */

#define VXL_HAS_BYTE 1
#define VXL_BYTE_STRING "char"
#if 1
  typedef   signed char  vxl_sbyte;
  typedef unsigned char  vxl_byte;
#else
# define vxl_sbyte  void
# define vxl_byte  void
#endif

#define VXL_HAS_INT_8 1
#define VXL_INT_8_STRING "char"
#if 1
  typedef          char  vxl_int_8;
  typedef   signed char  vxl_sint_8;
  typedef unsigned char  vxl_uint_8;
#else
# define vxl_int_8   void
# define vxl_sint_8  void
# define vxl_uint_8  void
#endif

#define VXL_HAS_INT_16 1
#define VXL_INT_16_STRING "short"
#if 1
  typedef          short vxl_int_16;
  typedef   signed short vxl_sint_16;
  typedef unsigned short vxl_uint_16;
#else
# define vxl_int_16  void
# define vxl_sint_16 void
# define vxl_uint_16 void
#endif

#define VXL_HAS_INT_32 1
#define VXL_INT_32_STRING "int"
#if 1
  typedef          int vxl_int_32;
  typedef   signed int vxl_sint_32;
  typedef unsigned int vxl_uint_32;
#else
# define vxl_int_32  void
# define vxl_sint_32 void
# define vxl_uint_32 void
#endif

#define VXL_HAS_INT_64 1
/* !!! different from VC6 */
#define VXL_INT_64_STRING "__int64"
#if 1
  typedef          __int64 vxl_int_64;
  typedef          __int64 vxl_sint_64;
  typedef unsigned __int64 vxl_uint_64;
#else
# define vxl_int_64  void
# define vxl_sint_64 void
# define vxl_uint_64 void
#endif

#define VXL_INT_64_IS_LONG 0

#define VXL_HAS_IEEE_32 1
#define VXL_IEEE_32_STRING "float"
#if 1
  typedef float vxl_ieee_32;
#else
# define vxl_ieee_32 void
#endif

#define VXL_HAS_IEEE_64 1
#define VXL_IEEE_64_STRING "double"
#if 1
  typedef double vxl_ieee_64;
#else
# define vxl_ieee_64 void
#endif

#define VXL_HAS_IEEE_96 0
#define VXL_IEEE_96_STRING "void"
#if 0
  typedef void vxl_ieee_96;
#else
# define vxl_ieee_96 void
#endif

#define VXL_HAS_IEEE_128 0
#define VXL_IEEE_128_STRING "void"
#if 0
  typedef void vxl_ieee_128;
#else
# define vxl_ieee_128 void
#endif

/* -------------------- operating system services */

#define VXL_HAS_PTHREAD_H         0
#define VXL_HAS_SEMAPHORE_H       0

/* -------------------- library quirks */

/* these should be 1 if the symbol in question is declared */
/* in the relevant header file and 0 otherwise. */

#define VXL_UNISTD_HAS_USECONDS_T 0
#define VXL_UNISTD_HAS_INTPTR_T   0
#define VXL_UNISTD_HAS_UALARM     1
#define VXL_UNISTD_HAS_USLEEP     1
#define VXL_UNISTD_HAS_LCHOWN     1
#define VXL_UNISTD_HAS_PREAD      1
#define VXL_UNISTD_HAS_PWRITE     1
#define VXL_UNISTD_HAS_TELL       1

/* true if <stdlib.h> declares qsort() */
#define VXL_STDLIB_HAS_QSORT      1

/* true if <stdlib.h> declares lrand48() */
/* !!! different from VC6 */
#define VXL_STDLIB_HAS_LRAND48    0

/* true if <stdlib.h> declares drand48() */
#define VXL_STDLIB_HAS_DRAND48    0

/* true if <stdlib.h> declares srand48() */
#define VXL_STDLIB_HAS_SRAND48    0

/* true if <ieeefp.h> declares finite() */
#define VXL_IEEEFP_HAS_FINITE     0 /* ? */

/* true if <math.h> declares finite() */
#define VXL_C_MATH_HAS_FINITEF     0 /* ? */

/* true if <math.h> declares finite() */
#define VXL_C_MATH_HAS_FINITE     0 /* ? */

/* true if <math.h> declares finite() */
#define VXL_C_MATH_HAS_FINITEL     0 /* ? */

/* true if <math.h> declares sqrtf() for the C compiler */
#define VXL_C_MATH_HAS_SQRTF      0 /* ? */

/* true if usleep() returns void */
#define VXL_UNISTD_USLEEP_IS_VOID 0

/* true if gettime() takes two arguments */
#define VXL_TWO_ARG_GETTIME       0

/* true if <ieeefp.h> is available */
/* !!! different from VC6 */
#define VXL_HAS_IEEEFP_H          0

#endif /* vxl_config_h_config_win32_vc70_ */
