#ifndef vnl_algo_determinant_h_
#define vnl_algo_determinant_h_
#ifdef __GNUC__
#pragma interface
#endif


//:
//  \file
//  \brief calculates the determinant of a matrix
//  \author fsm@robots.ox.ac.uk
// 
//  Modifications
//  dac (Manchester) 26/03/2001: tidied up documentation
//


#include <vnl/vnl_determinant.h> // <-- not this file: different dir!


//: calculates the determinant of a matrix
//  evaluation of determinants of any size. for small
//  matrices, will use the direct routines (no netlib)
//  but for larger matrices, a matrix decomposition 
//  such as SVD or QR will be used.

template <class T>
T vnl_determinant(T const * const *rows, unsigned n);

template <class T> class vnl_matrix;

template <class T>
T vnl_determinant(vnl_matrix<T> const &);

#endif // vnl_algo_determinant_h_
