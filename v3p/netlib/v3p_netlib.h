#ifndef v3p_netlib_h
#define v3p_netlib_h

/* Include the basic f2c interface.  */
#include "v3p_f2c.h"

/* Mangle the netlib symbols and types to have a v3p_netlib prefix.  */
#include "v3p_netlib_mangle.h"

/* Enforce const-correctness only outside v3p_netlib sources.  */
#ifdef V3P_NETLIB_SRC
# define v3p_netlib_const
#else
# define v3p_netlib_const const
#endif

/* Modify the interface for C++.  */
#ifdef __cplusplus
#include <complex>
# undef complex
# define v3p_netlib_complex std::complex<float>
# define v3p_netlib_doublecomplex std::complex<double>
#endif

/* Include the netlib interface.  */
#ifdef __cplusplus
extern "C" {
#endif
extern void v3p_netlib_initialize();
#include "v3p_netlib_prototypes.h"
#ifdef __cplusplus
}
#endif

/* Cleanup the namespace if not inside a v3p_netlib source.  */
#ifndef V3P_NETLIB_SRC
# include "v3p_netlib_unmangle.h"
#endif

/* Automatically initialize the netlib library for C++ clients.  */
#if defined(__cplusplus) && !defined(V3P_NETLIB_SRC)
struct v3p_netlib_initializer
{
  v3p_netlib_initializer() { v3p_netlib_initialize(); }
};
static v3p_netlib_initializer v3p_netlib_initializer_instance;
#endif

#endif
