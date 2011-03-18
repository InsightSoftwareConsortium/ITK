// This is core/vnl/algo/vnl_orthogonal_complement.h
#ifndef vnl_orthogonal_complement_h_
#define vnl_orthogonal_complement_h_
//:
// \file
// \brief For computing the orthogonal complement to a linear subspace.
// \author fsm
//
// \verbatim
// Modifications
// 4/4/01 LSB(Manchester) Tidied documentation
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
// \endverbatim

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

//: Return a matrix whose columns span is the orthogonal complement of v.
//  \relatesalso vnl_matrix
template <class T>
vnl_matrix<T> vnl_orthogonal_complement(vnl_vector<T> const &v);

#if 0
//: Return a matrix whose column span is the orthogonal complement of the column span of M.
//  \relatesalso vnl_matrix
template <class T>
vnl_matrix<T> vnl_orthogonal_complement(vnl_matrix<T> const &M);
#endif

#endif // vnl_orthogonal_complement_h_
