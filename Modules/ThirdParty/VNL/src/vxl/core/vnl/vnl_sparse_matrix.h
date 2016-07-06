// This is core/vnl/vnl_sparse_matrix.h
#ifndef vnl_sparse_matrix_h_
#define vnl_sparse_matrix_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief Simple sparse matrix
//
//    Only those values which
//    are non-zero are stored. The sparse matrix currently supports
//    only getting/putting elements, and multiply by vector or another
//    sparse matrix.
//
//    Each row is stored as a vector of std::pair<unsigned int,T>, where the first
//    of the pair indicates the column index, and the second the
//    value.  All rows are stored, as std::vector< row >;
//
//  \author Rupert W. Curwen, GE CR&D
//  \date   20 Oct 98
//
// \verbatim
//  Modifications
//   Robin Flatland 5/31/99 Added pre_mult(lhs,result), where
//                          lhs is a vector.
//
//   Robin Flatland 6/08/99 Added iterator that allows sequential
//                          access to non-zero values in matrix.
//                          Iterator is controlled using reset, next,
//                          getrow, getcolumn, and value.
//
//   David Capel May 2000   Added set_row, scale_row, mult, vcat and const
//                          methods where appropriate.
//   Peter Vanroose - Jan.2009 - Added several methods, modelled after vnl_matrix<T>:
//     const version of operator()(unsigned int, unsigned int)
//     T get(unsigned int, unsigned int)
//     void put(unsigned int, unsigned int, T)
//     void clear()
//     vnl_sparse_matrix& normalize_rows()
//     bool operator==()
//     bool operator!=()
//     unary minus of a matrix
//     addition of two matrices
//     subtraction of two matrices
//     multiplication of two matrices
//     in-place addition of two matrices
//     in-place subtraction of two matrices
//     in-place multiplication of two matrices
//     scalar multiplication of a matrix
//     in-place scalar multiplication of a matrix
//     scalar division of a matrix
//     in-place scalar division of a matrix
//   Peter Vanroose - Oct.2010 - Added set_identity()
//   Peter Vanroose - Mar.2011 - Added transpose() and conjugate_transpose()
// \endverbatim

#include <vector>
#include <functional>
#include <vcl_compiler.h>
#include <vnl/vnl_vector.h>
#include "vnl/vnl_export.h"

//: Stores elements of sparse matrix
//  Only those values which
//  are non-zero are stored. The sparse matrix currently supports
//  only getting/putting elements, and multiply by vector or another
//  sparse matrix.
//
//  Each row is stored as a vector of std::pair<unsigned int,T>, where the first
//  of the pair indicates the column index, and the second the
//  value.  All rows are stored, as std::vector< row >;
//
template <class T>
class VNL_TEMPLATE_EXPORT vnl_sparse_matrix_pair
{
 public:
  unsigned int first;
  T second;

  //: Constructs a pair with null values
  vnl_sparse_matrix_pair() : first(0), second(T(0)) {}

  //: Constructs a pair with position a and value b
  vnl_sparse_matrix_pair(unsigned int const& a, T const& b) : first(a), second(b) {}

  vnl_sparse_matrix_pair(const vnl_sparse_matrix_pair<T>& o) : first(o.first), second(o.second) {}

  vnl_sparse_matrix_pair<T>& operator=(vnl_sparse_matrix_pair const &o) {
    if (&o != this) {
      first = o.first;
      second = o.second;
    }
    return *this;
  }

  struct less : public std::binary_function<vnl_sparse_matrix_pair, vnl_sparse_matrix_pair, bool>
  {
    bool operator() (vnl_sparse_matrix_pair const& p1, vnl_sparse_matrix_pair const& p2) {
      return p1.first < p2.first;
    }
  };
};


//: Simple sparse matrix
//  Stores non-zero elements as a sparse_matrix_pair
template <class T>
class VNL_TEMPLATE_EXPORT vnl_sparse_matrix
{
 public:
  typedef vnl_sparse_matrix_pair<T> pair_t;
  typedef std::vector < pair_t > row;
  typedef std::vector < row > vnl_sparse_matrix_elements;

  //: Construct an empty matrix
  vnl_sparse_matrix();

  //: Construct an empty m*n matrix
  vnl_sparse_matrix(unsigned int m, unsigned int n);

  //: Construct an m*n Matrix and copy rhs into it.
  vnl_sparse_matrix(vnl_sparse_matrix<T> const& rhs);

  //: Copy another vnl_sparse_matrix<T> into this.
  vnl_sparse_matrix<T>& operator=(vnl_sparse_matrix<T> const& rhs);

  //: Multiply this*rhs, where rhs is a vector.
  void mult(vnl_vector<T> const& rhs, vnl_vector<T>& result) const;

  //: Multiply this*p, a fortran order matrix.
  void mult(unsigned int n, unsigned int m, T const* p, T* q) const;

  //: Multiplies lhs*this, where lhs is a vector
  void pre_mult(const vnl_vector<T>& lhs, vnl_vector<T>& result) const;

  //: Get a reference to an entry in the matrix.
  T& operator()(unsigned int row, unsigned int column);

  //: Get the value of an entry in the matrix.
  T operator()(unsigned int row, unsigned int column) const;

  //: Get an entry in the matrix.
  //  This is the "const" version of operator().
  T get(unsigned int row, unsigned int column) const;

  //: Put (i.e., add or overwrite) an entry into the matrix.
  void put(unsigned int row, unsigned int column, T value);

  //: Get diag(A_transpose * A).
  // Useful for forming Jacobi preconditioners for linear solvers.
  void diag_AtA(vnl_vector<T>& result) const;

  //: Set a whole row at once. Much faster. Returns *this.
  vnl_sparse_matrix& set_row(unsigned int r,
                             std::vector<int> const& cols,
                             std::vector<T> const& vals);

  //: Return row as vector of pairs
  //  Added to aid binary I/O
  row& get_row(unsigned int r) {return elements[r];}

  //: Laminate matrix A onto the bottom of this one
  vnl_sparse_matrix<T>& vcat(vnl_sparse_matrix<T> const& A);

  //: Get the number of rows in the matrix.
  unsigned int rows() const { return rs_; }

  //: Get the number of columns in the matrix.
  unsigned int columns() const { return cs_; }

  //: Get the number of columns in the matrix.
  unsigned int cols() const { return cs_; }

  //: Return whether a given row is empty
  bool empty_row(unsigned int r) const { return elements[r].empty(); }

  //: This is occasionally useful.
  T sum_row(unsigned int r);

  //: Useful for normalizing row sums in convolution operators
  vnl_sparse_matrix& scale_row(unsigned int r, T scale);

  //: Set all elements to null
  void clear() { elements.clear(); }

  //: Resizes the array to have r rows and c cols -- sets elements to null
  void set_size( int r, int c );

  //: Resizes the array to have r rows and c cols
  void resize( int r, int c );

  //: Resets the internal iterator
  void reset() const;

  //: Moves the internal iterator to next non-zero entry in matrix.
  // Returns true if there is another value, false otherwise. Use
  // in combination with methods reset, getrow, getcolumn, and value.
  bool next() const;

  //: Returns the row of the entry pointed to by internal iterator.
  int getrow() const;

  //: Returns the column of the entry pointed to by internal iterator.
  int getcolumn() const;

  //: Returns the value pointed to by the internal iterator.
  T value() const;

  //: Comparison
  bool operator==(vnl_sparse_matrix<T> const& rhs) const;

  //: Inequality
  bool operator!=(vnl_sparse_matrix<T> const& rhs) const
  { return !operator==(rhs); }

  //: Unary minus
  vnl_sparse_matrix<T> operator-() const;

  //: addition
  vnl_sparse_matrix<T> operator+(vnl_sparse_matrix<T> const& rhs) const;

  //: subtraction
  vnl_sparse_matrix<T> operator-(vnl_sparse_matrix<T> const& rhs) const;

  //: multiplication
  vnl_sparse_matrix<T> operator*(vnl_sparse_matrix<T> const& rhs) const;

  //: in-place addition
  vnl_sparse_matrix<T>& operator+=(vnl_sparse_matrix<T> const& rhs);

  //: in-place subtraction
  vnl_sparse_matrix<T>& operator-=(vnl_sparse_matrix<T> const& rhs);

  //: in-place multiplication
  vnl_sparse_matrix<T>& operator*=(vnl_sparse_matrix<T> const& rhs);

  //: scalar multiplication
  vnl_sparse_matrix<T> operator*(T const& rhs) const;

  //: in-place scalar multiplication
  vnl_sparse_matrix<T>& operator*=(T const& rhs);

  //: scalar division
  vnl_sparse_matrix<T> operator/(T const& rhs) const;

  //: in-place scalar division
  vnl_sparse_matrix<T>& operator/=(T const& rhs);

  //: returns a new sparse matrix, viz. the transpose of this
  vnl_sparse_matrix<T> transpose() const;

  //: returns a new sparse matrix, viz. the conjugate (or Hermitian) transpose of this
  vnl_sparse_matrix<T> conjugate_transpose() const;

  //: Sets this matrix to an identity matrix, then returns "*this".
  //  Returning "*this" allows e.g. passing an identity matrix as argument to
  //  a function f, without having to name the constructed matrix:
  //  \code
  //     f(vnl_sparse_matrix<double>(5000,5000).set_identity());
  //  \endcode
  //  Returning "*this" also allows "chaining" two or more operations:
  //  e.g., to set a matrix to identity, then add an other matrix to it:
  //  \code
  //     M.set_identity() += M2;
  //  \endcode
  //  If the matrix is not square, anyhow set main diagonal to 1, the rest to 0.
  vnl_sparse_matrix& set_identity();

  //: Normalizes each row so it is a unit vector, and returns "*this".
  //  Zero rows are not modified
  //  Returning "*this" allows "chaining" two or more operations:
  //  \code
  //     M.normalize_rows() += M2;
  //  \endcode
  //  Note that there is no method normalize_columns() since its implementation
  //  would be much more inefficient than normalize_rows()!
  vnl_sparse_matrix& normalize_rows();

  // These three methods are used to implement their operator() variants
  // They should ideally be protected, but for backward compatibility reasons
  // they continue to be public for a while ...

  //: Add rhs to this.
  //  Deprecated for direct use: please use operator "+" instead.
  void add(const vnl_sparse_matrix<T>& rhs, vnl_sparse_matrix<T>& result) const;

  //: Subtract rhs from this.
  //  Deprecated for direct use: please use operator "-" instead.
  void subtract(const vnl_sparse_matrix<T>& rhs, vnl_sparse_matrix<T>& result) const;

  //: Multiply this*rhs, another sparse matrix.
  //  Deprecated for direct use: please use operator "*" instead.
  void mult(vnl_sparse_matrix<T> const& rhs, vnl_sparse_matrix<T>& result) const;

 protected:
  vnl_sparse_matrix_elements elements;
  unsigned int rs_, cs_;

  // internal iterator
  mutable unsigned int itr_row;
  mutable typename row::const_iterator itr_cur;
  mutable bool itr_isreset;
};

// non-member arithmetical operators.

//:
// \relatesalso vnl_matrix
template<class T>
inline vnl_sparse_matrix<T> operator*(T const& value, vnl_sparse_matrix<T> const& m)
{
  return m * value;
}


#endif // vnl_sparse_matrix_h_
