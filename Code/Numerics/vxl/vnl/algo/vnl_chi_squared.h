#ifndef vnl_chi_squared_h_
#define vnl_chi_squared_h_
#ifdef __GNUC__
#pragma interface
#endif

//:
//  \file
//  \brief Name space for various chi-squared distribution functions.
//  \author Rupert Curwen, GE CRD, August 18th, 1998
//
//  \verbatim
//  Modifications
//  dac (Manchester) 26/03/2001: tidied up documentation
//  \endverbatim
//


//: Compute cumulative distribution function value for chi-squared distribution
extern float vnl_chi_squared_cumulative(float chisq, int dof);

//------------------------------------------------------------

//: Name space for various chi-squared distribution functions.
//
//\verbatim
//  A[] and B[] are (pointers to) arrays containing histograms.
//  If the 'normalize' parameter is true, each histogram will
//  be implicitly normalized (so as to sum to 1) before the
//  statistic is calculated :
// 
//  a[i] = A[i] / \sum_j A[j]
//  b[i] = B[i] / \sum_j B[j]
//
//  *DO NOT* add scale factors to these functions or you will break 
//  the code written by those who read the documentation. fsm.

//      (a[i] - b[i])^2
// \sum ---------------
//   i       a[i]
//\endverbatim
//

template <class T>
double vnl_chi_squared_statistic_1 (T const *A, T const *B, 
                                    int n, bool normalize);

//:
//\verbatim
//      (a[i] - b[i])^2
// \sum ---------------
//   i       b[i]
//\endverbatim
template <class T>
double vnl_chi_squared_statistic_2 (T const *A, T const *B, 
                                    int n, bool normalize);

//:
//\verbatim
//      (a[i] - b[i])^2
// \sum ---------------
//   i    a[i] + b[i]
//\endverbatim
template <class T>
double vnl_chi_squared_statistic_12(T const *A, T const *B, 
                                    int n, bool normalize);

#endif // vnl_chi_squared_h_
