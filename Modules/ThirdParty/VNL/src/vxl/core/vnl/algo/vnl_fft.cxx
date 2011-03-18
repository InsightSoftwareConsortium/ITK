// This is core/vnl/algo/vnl_fft.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author fsm

#include "vnl_fft.h"

#include <vnl/algo/vnl_netlib.h> // dgpfa_()

void vnl_fft_setgpfa(float *triggs, long size, long pqr[3], long *info)
{
  v3p_netlib_setgpfa_(triggs, &size, pqr, info);
}

void vnl_fft_setgpfa(double *triggs, long size, long pqr[3], long *info)
{
  v3p_netlib_setdgpfa_(triggs, &size, pqr, info);
}

//----------------------------------------------------------------------

void vnl_fft_gpfa(float  *a, float  *b, float const  *triggs,
                  long inc, long jump, long n,
                  long lot, long isign, long const pqr[3], long *info)
{
  v3p_netlib_gpfa_(a, b, triggs, &inc, &jump, &n, &lot, &isign, pqr);
  *info = 0;
}

void vnl_fft_gpfa(double *a, double *b, double const *triggs,
                  long inc, long jump, long n,
                  long lot, long isign, long const pqr[3], long *info)
{
  v3p_netlib_dgpfa_(a, b, triggs, &inc, &jump, &n, &lot, &isign, pqr);
  *info = 0;
}
