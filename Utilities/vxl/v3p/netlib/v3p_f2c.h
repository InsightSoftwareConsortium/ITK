#ifndef v3p_f2c_h
#define v3p_f2c_h

/* Disable some warnings inside v3p_netlib sources.  */
#ifdef V3P_NETLIB_SRC
# if defined(_MSC_VER)
#  pragma warning (disable: 4244) /* conversion with possible loss of data */
#  if !defined(_COMPLEX_DEFINED)
    struct _complex { double x,y; };
#   define _COMPLEX_DEFINED /* block math.h from defining complex macro */
#  endif
# endif
#endif

/* Mangle the f2c symbols and types to have a v3p_netlib prefix.  */
#include "v3p_f2c_mangle.h"

/* Avoid f2c namespace violations.  */
#ifndef V3P_NETLIB_SRC
# define V3P_F2C_SKIP_UNDEFS
#endif

/* Include the renamed original f2c.h file with a C interface.  */
#ifdef __cplusplus
extern "C" {
#endif
#include "v3p_f2c_original.h"
double pow_dd(doublereal *ap, doublereal *bp);
double pow_di(doublereal *ap, integer *bp);
double d_lg10(doublereal *x);
double d_sign(doublereal *a, doublereal *b);
#ifdef __cplusplus
}
#endif

/* Cleanup the namespace if not inside a v3p_netlib source.  */
#ifndef V3P_NETLIB_SRC
# undef qbit_clear
# undef qbit_set
# undef TRUE_
# undef FALSE_
# undef Extern
# undef VOID
# undef abs
# undef dabs
# undef min
# undef max
# undef dmin
# undef dmax
# undef bit_test
# undef bit_clear
# undef bit_set
# undef F2C_proc_par_types
# include "v3p_f2c_unmangle.h"
#endif

#endif
