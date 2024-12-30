// This is core/vnl/algo/vnl_fft.cxx
//:
// \file
// \author fsm

#include <vnl/algo/vnl_netlib.h> // dgpfa_()

void
vnl_fft_setgpfa(float * triggs, long size, long pqr[3], long * info)
{
  v3p_netlib_setgpfa_(triggs, &size, pqr, info);
}

void
vnl_fft_setgpfa(double * triggs, long size, long pqr[3], long * info)
{
  v3p_netlib_setdgpfa_(triggs, &size, pqr, info);
}

//----------------------------------------------------------------------

void
vnl_fft_gpfa(float * a,
             float * b,
             const float * triggs,
             long inc,
             long jump,
             long n,
             long lot,
             long isign,
             const long pqr[3],
             long * info)
{
  v3p_netlib_gpfa_(a, b, triggs, &inc, &jump, &n, &lot, &isign, pqr);
  *info = 0;
}

void
vnl_fft_gpfa(double * a,
             double * b,
             const double * triggs,
             long inc,
             long jump,
             long n,
             long lot,
             long isign,
             const long pqr[3],
             long * info)
{
  v3p_netlib_dgpfa_(a, b, triggs, &inc, &jump, &n, &lot, &isign, pqr);
  *info = 0;
}
