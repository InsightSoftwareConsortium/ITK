// This is vxl/vnl/algo/vnl_fft.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author fsm

#include "vnl_fft.h"

#include "vnl_netlib.h"

void vnl_fft_setgpfa(float *triggs, int size, int pqr[3], int *info)
{
  setgpfa_(triggs, &size, pqr, info);
}

void vnl_fft_setgpfa(double *triggs, int size, int pqr[3], int *info)
{
  dsetgpfa_(triggs, &size, pqr, info);
}

//----------------------------------------------------------------------

void vnl_fft_gpfa(float  *a, float  *b, float const  *triggs,
                  int inc, int jump, int n,
                  int lot, int isign, int const pqr[3], int *info)
{
  gpfa_(a, b, triggs, &inc, &jump, &n, &lot, &isign, pqr, info);
}

void vnl_fft_gpfa(double *a, double *b, double const *triggs,
                  int inc, int jump, int n,
                  int lot, int isign, int const pqr[3], int *info)
{
  dgpfa_(a, b, triggs, &inc, &jump, &n, &lot, &isign, pqr, info);
}
