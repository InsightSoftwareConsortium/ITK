// This is core/vnl/algo/vnl_chi_squared.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file

#include "vnl_chi_squared.h"

// FORTRAN routine
#include <vnl/algo/vnl_netlib.h> // dchscdf_()

//: Compute cumulative distribution function for chi-squared distribution.
// This subroutine computes the cumulative distribution function
// value for the chi-squared distribution with integer degrees of
// freedom parameter = dof.  This distribution is defined for all
// non-negative chisq.  Thus if a random variable x is drawn from a
// chi-squared distribution with d degrees of freedom, then P(x < X) =
// vnl_chi_squared::vnl_chi_squaredCumulative(X,d).
double vnl_chi_squared_cumulative(double chisq, long dof)
{
  double cdf;
  v3p_netlib_dchscdf_(&chisq,&dof,&cdf);
  return cdf;
}

//------------------------------------------------------------

template <class T>
double vnl_chi_squared_statistic_1 (T const *A, T const *B, int n, bool normalize)
{
  double sum = 0;

  if (normalize)
  {
    T sumA = 0;
    T sumB = 0;
    for (int i=0; i<n; ++i) {
      sumA += A[i];
      sumB += B[i];
    }

    for (int i=0; i<n; ++i)
      if (A[i]) {
        double a = double(A[i])/sumA;
        double b = double(B[i])/sumB;
        double tmp = a - b;
        sum += tmp*tmp/a;
      }
  }
  else
  {
    for (int i=0; i<n; ++i)
      if (A[i]) {
        double tmp = A[i] - B[i];
        sum += tmp*tmp/A[i];
      }
  }

  return sum;
}

template <class T>
double vnl_chi_squared_statistic_2 (T const *A, T const *B, int n, bool normalize)
{
  return vnl_chi_squared_statistic_1(B, A, n, normalize);
}

template <class T>
double vnl_chi_squared_statistic_12(T const *A, T const *B, int n, bool normalize)
{
  double sum = 0;

  if (normalize)
  {
    T sumA = 0;
    T sumB = 0;
    for (int i=0; i<n; ++i) {
      sumA += A[i];
      sumB += B[i];
    }

    for (int i=0; i<n; ++i)
      if (A[i] || B[i]) {
        double a = double(A[i])/sumA;
        double b = double(B[i])/sumB;
        double tmp = a - b;
        sum += tmp*tmp/(a + b);
      }
  }
  else
  {
    for (int i=0; i<n; ++i)
      if (A[i] || B[i]) {
        double tmp = A[i] - B[i];
        sum += tmp*tmp/(A[i] + B[i]);
      }
  }

  return sum;
}

#define inst(T) \
template double vnl_chi_squared_statistic_1 (T const *, T const *, int, bool); \
template double vnl_chi_squared_statistic_2 (T const *, T const *, int, bool); \
template double vnl_chi_squared_statistic_12(T const *, T const *, int, bool)

inst(int);
inst(double);
