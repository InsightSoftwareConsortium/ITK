/*
  fsm@robots.ox.ac.uk
*/
#ifdef __GNUC__
#pragma implementation "vnl_fft"
#endif
#include "vnl_fft.h"

#include <vnl/algo/vnl_netlib.h>

void vnl_fft_setgpfa(float *triggs, int size, int const pqr[3], int *info)
{
  setgpfa_(triggs, size, pqr, info);
}

void vnl_fft_setgpfa(double *triggs, int size, int const pqr[3], int *info)
{
  dsetgpfa_(triggs, size, pqr, info);
}

//----------------------------------------------------------------------

void vnl_fft_gpfa(float  *a, float  *b, float const  *triggs,
                  int inc, int jump, int n,
                  int lot, int isign, int const pqr[3], int *info)
{
  gpfa_(a, b, triggs, inc, jump, n, lot, isign, pqr, info);
}

void vnl_fft_gpfa(double *a, double *b, double const *triggs,
                  int inc, int jump, int n,
                  int lot, int isign, int const pqr[3], int *info)
{
  dgpfa_(a, b, triggs, inc, jump, n, lot, isign, pqr, info);
}
