#ifndef vnl_orthogonal_complement_h_
#define vnl_orthogonal_complement_h_
#ifdef __GNUC__
#pragma interface
#endif
// .NAME	vnl_orthogonal_complement
// .LIBRARY	vnl-algo
// .HEADER	vxl Package
// .INCLUDE	vnl/algo/vnl_orthogonal_complement.h
// .FILE	vnl_orthogonal_complement.txx
// .SECTION Description
// Convenience functions for computing the orthogonal
// complement to a linear subspace.
// .SECTION Author
//  fsm@robots.ox.ac.uk

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

//: return a matrix whose columns span is the orthogonal
// complement of v.
template <class T>
vnl_matrix<T> vnl_orthogonal_complement(vnl_vector<T> const &v);

// //: return a matrix whose column span is the orthogonal 
// // complement of the column span of M.
// template <typename T>
// vnl_matrix<T> vnl_orthogonal_complement(vnl_matrix<T> const &M);

#endif // vnl_orthogonal_complement_h_
