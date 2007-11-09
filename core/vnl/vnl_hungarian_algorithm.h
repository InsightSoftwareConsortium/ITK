#ifndef vnl_hungarian_algorithm_h_
#define vnl_hungarian_algorithm_h_

//:
// \file
// \author Amitha Perera
// \date   Sep 2004

#include <vcl_vector.h>
#include <vnl/vnl_matrix.h>

//: Find the best column to row assignment given a cost matrix.
//
// This is an implementation of the Hungarian algorithm (also known
// as the Munkres algorithm). It finds the minimum cost assignment of
// the rows of the cost matrix \a cost (workers) to the columns
// (jobs).
//
// \param cost An N x M cost matrix. The costs cannot be -Infinity.
//
// \returns A vector v of size N such that v[i] = j means that row i
// should be assigned to column j. \code v[i] = -1u \endcode (= \code
// unsigned(-1) \endcode ) means that row i was not assigned to any
// column. If N \> M, then every column will be assigned to some
// row. If N \< M then every row will be assigned to some column.
//
vcl_vector<unsigned> vnl_hungarian_algorithm( vnl_matrix<double> const& cost );

#endif // vnl_hungarian_algorithm_h_
