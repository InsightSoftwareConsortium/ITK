#ifndef vnl_algo_determinant_h_
#define vnl_algo_determinant_h_
#ifdef __GNUC__
#pragma interface
#endif

// .NAME	vnl_determinant
// .LIBRARY	vnl-algo
// .HEADER	vxl Package
// .INCLUDE	vnl/algo/vnl_determinant.h
// .FILE	vnl_determinant.cxx
//
// .SECTION Description
// purpose:
// evaluation of determinants of any size. for small
// matrices, will use the direct routines (no netlib)
// but for larger matrices, a matrix decomposition 
// such as SVD or QR will be used.
//
// .SECTION Author
//    fsm@robots.ox.ac.uk
//

#include <vnl/vnl_determinant.h> // <-- not this file: different dir!

template <class T>
T vnl_determinant(T const * const *rows, unsigned n);

template <class T> class vnl_matrix;

template <class T>
T vnl_determinant(vnl_matrix<T> const &);

#endif // vnl_algo_determinant_h_
