#ifndef vnl_chi_squared_h_
#define vnl_chi_squared_h_
#ifdef __GNUC__
#pragma interface
#endif

// .NAME	vnl_chi_squared
// .LIBRARY	vnl-algo
// .HEADER	vxl Package
// .INCLUDE	vnl/algo/vnl_chi_squared.h
// .FILE	vnl_chi_squared.cxx
//
// .SECTION Description
//   Name space for various chi-squared distribution functions.  
//
// .SECTION Author
//   Rupert Curwen, GE CRD, August 18th, 1998

//: Compute cumulative distribution function value for chi-squared distribution
extern float vnl_chi_squared_cumulative(float chisq, int dof);

//------------------------------------------------------------

// A[] and B[] are (pointers to) arrays containing histograms.
//
// If the 'normalize' parameter is true, each histogram will
// be implicitly normalized (so as to sum to 1) before the
// statistic is calculated :
// 
//   a[i] = A[i] / \sum_j A[j]
//   b[i] = B[i] / \sum_j B[j]
//
// *DO NOT* add scale factors to these functions or you will break 
// the code written by those who read the documentation. fsm.

//      (a[i] - b[i])^2
// \sum ---------------
//   i       a[i]
template <class T>
double vnl_chi_squared_statistic_1 (T const *A, T const *B, int n, bool normalize);

//      (a[i] - b[i])^2
// \sum ---------------
//   i       b[i]
template <class T>
double vnl_chi_squared_statistic_2 (T const *A, T const *B, int n, bool normalize);

//      (a[i] - b[i])^2
// \sum ---------------
//   i    a[i] + b[i]
template <class T>
double vnl_chi_squared_statistic_12(T const *A, T const *B, int n, bool normalize);

#endif // vnl_chi_squared_h_
