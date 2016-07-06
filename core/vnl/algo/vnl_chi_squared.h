// This is core/vnl/algo/vnl_chi_squared.h
#ifndef vnl_chi_squared_h_
#define vnl_chi_squared_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
#include "vnl/vnl_export.h"
#include <vnl/algo/vnl_algo_export.h>
//:
// \file
// \brief Name space for various (mostly templated) chi-squared distribution functions.
// \author Rupert Curwen, GE CRD
// \date   August 18th, 1998
//
// \verbatim
//  Modifications
//   26/03/2001   dac (Manchester) tidied up documentation
//   24 Mar 2010  Peter Vanroose   made vnl_chi_squared_cumulative() templated
// \endverbatim

//: Compute cumulative distribution function value for chi-squared distribution.
// This subroutine computes the cumulative distribution function
// value for the chi-squared distribution with integer degrees of
// freedom parameter = dof.  This distribution is defined for all
// non-negative chisq.  Thus if a random variable x is drawn from a
// chi-squared distribution with d degrees of freedom, then
//  $P(x < X) =$ vnl_chi_squared_cumulative(X,d).
// Internally, T=double is used.
template <class T>
double vnl_chi_squared_cumulative(T chisq, long dof);

//------------------------------------------------------------

//: Name space for various chi-squared distribution functions.
//
//  A[] and B[] are (pointers to) arrays containing histograms.
//  If the 'normalize' parameter is true, each histogram will
//  be implicitly normalized (so as to sum to 1) before the
//  statistic is calculated :
//
//  $a[i] = A[i] / \sum_j A[j]$
//
//  $b[i] = B[i] / \sum_j B[j]$
//
//  *DO NOT* add scale factors to these functions or you will break
//  the code written by those who read the documentation. fsm.
//
// $\displaystyle   \sum_i \frac{ (a[i] - b[i])^2 }{ a[i] } $
//

template <class T>
double vnl_chi_squared_statistic_1 (T const *A, T const *B,
                                    int n, bool normalize);

//:
// $\displaystyle   \sum_i \frac{ (a[i] - b[i])^2 }{ b[i] } $
template <class T>
double vnl_chi_squared_statistic_2 (T const *A, T const *B,
                                    int n, bool normalize);

//:
// $\displaystyle   \sum_i \frac{ (a[i] - b[i])^2 }{ a[i] + b[i] } $
template <class T>
double vnl_chi_squared_statistic_12(T const *A, T const *B,
                                    int n, bool normalize);

#define VNL_CHI_SQUARED_INSTANTIATE(T) \
extern "please include vnl/algo/vnl_chi_squared.hxx first"

#endif // vnl_chi_squared_h_
