// This is core/vnl/vnl_rank.h
#ifndef vnl_rank_h_
#define vnl_rank_h_
//:
//  \file
//  \author Peter Vanroose, Leuven
//  \date   27 March 2003
//  \brief Direct computation of the rank of a matrix, without using svd
//
//  The (row) rank of a matrix is its number of linearly independent rows.
//  This turns out to be equal to the number of linearly independent columns,
//  i.e., the column rank, so it is just called the rank of the matrix.
//  This can be computed by row-reducing (or column-reducing) the matrix
//  and then counting the number of non-zero rows (or columns).

#include <vnl/vnl_matrix.h>

typedef enum { vnl_rank_row, vnl_rank_column, vnl_rank_both } vnl_rank_type;
typedef enum { vnl_rank_pivot_one, vnl_rank_pivot_all } vnl_rank_pivot_type;

//: Returns the rank of a matrix
//  By default, the row rank of the matrix is determined.
//  Specify vnl_rank_column to obtain the column rank.
//
// \relatesalso vnl_matrix
template <class T>
unsigned int vnl_rank(vnl_matrix<T> const& mat, vnl_rank_type = vnl_rank_both);

//: Row reduce a matrix.
//  First try to use 1 or -1 as pivot element in each row, to avoid divisions;
//  then use any nonzero element as candidate pivot.
//  Repeat this process until the matrix does not change any more.
//  At that point, the matrix spans the same row space as before and contains
//  as many zeros as possible.
//
//  When specifying vnl_rank_pivot_one is given as second argument,
//  only elements with value 1 or -1 are used as candidate pivot elements.
//
//  Note that for integer matrices, the resulting matrix is still integer,
//  and is guaranteed to be row equivalent with the original matrix.
//
// \relatesalso vnl_matrix
//
template <class T>
vnl_matrix<T> vnl_rank_row_reduce(vnl_matrix<T> const& mat,
                                  vnl_rank_pivot_type = vnl_rank_pivot_all);

//: Column reduce a matrix.
//
// \relatesalso vnl_matrix
//
template <class T>
vnl_matrix<T> vnl_rank_column_reduce(vnl_matrix<T> const& mat,
                                     vnl_rank_pivot_type = vnl_rank_pivot_all);

//: Row and column reduce a matrix.
//  Perform both row reduction and column reduction on a matrix.
//  The resulting matrix will in general no longer span the same row space
//  (or column space) as the original matrix, but the rank will not have
//  changed, and the number of nonzero elements will be minimal (viz at most
//  one per row and one per column).
//
// \relatesalso vnl_matrix
//
template <class T>
vnl_matrix<T> vnl_rank_row_column_reduce(vnl_matrix<T> const& mat,
                                         vnl_rank_pivot_type = vnl_rank_pivot_all);

#define VNL_RANK_INSTANTIATE(T) extern "please #include vnl/vnl_rank.txx instead"

#endif // vnl_rank_h_
