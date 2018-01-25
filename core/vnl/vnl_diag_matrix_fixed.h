// This is core/vnl/vnl_diag_matrix_fixed.h
#ifndef vnl_diag_matrix_fixed_h_
#define vnl_diag_matrix_fixed_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Contains class for diagonal matrices
// \author Andrew W. Fitzgibbon (Oxford RRG)
// \date   5 Aug 1996
//
// \verbatim
//  Modifications
//   IMS (Manchester) 16 Mar 2001: Tidied up the documentation + added binary_io
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
//   Sep.2002 - Peter Vanroose - Added operator+, operator-, operator*
//   Mar.2004 - Peter Vanroose - removed deprecated resize()
//   Oct.2010 - Peter Vanroose - mutators and setters now return *this
//   Jan.2011 - Peter Vanroose - added methods set_diagonal() & get_diagonal()
// \endverbatim

#include <iosfwd>
#include <vcl_cassert.h>
#include <vcl_compiler.h>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_matrix_fixed.h>
#include "vnl/vnl_export.h"

// forward declarations
template <class T, unsigned int N> class vnl_diag_matrix_fixed;
template <class T, unsigned int N> VNL_TEMPLATE_EXPORT vnl_vector_fixed<T,N> operator*(vnl_diag_matrix_fixed<T,N> const&, vnl_vector_fixed<T,N> const&);

//: stores a diagonal matrix as a single vector.
//  vnl_diag_matrix_fixed stores a diagonal matrix for time and space efficiency.
//  Specifically, only the diagonal elements are stored, and some matrix
//  operations (currently *, + and -) are overloaded to use more efficient
//  algorithms.

VCL_TEMPLATE_EXPORT
template <class T, unsigned int N>
class VNL_TEMPLATE_EXPORT vnl_diag_matrix_fixed
{
  vnl_vector_fixed<T,N> diagonal_;

 public:
  vnl_diag_matrix_fixed() : diagonal_() {}


  //: Construct a diagonal matrix with diagonal elements equal to value.
  vnl_diag_matrix_fixed(T const& value) : diagonal_(value) {}

  //: Construct a diagonal matrix from a vnl_vector_fixed.
  //  The vector elements become the diagonal elements.
  explicit vnl_diag_matrix_fixed(vnl_vector_fixed<T,N> const& that): diagonal_(that) {}
 ~vnl_diag_matrix_fixed() {}

  inline vnl_diag_matrix_fixed& operator=(vnl_diag_matrix_fixed<T,N> const& that) {
    this->diagonal_ = that.diagonal_;
    return *this;
  }

  // Operations----------------------------------------------------------------

  //: In-place arithmetic operation
  inline vnl_diag_matrix_fixed<T,N>& operator*=(T v) { diagonal_ *= v; return *this; }
  //: In-place arithmetic operation
  inline vnl_diag_matrix_fixed<T,N>& operator/=(T v) { diagonal_ /= v; return *this; }

  // Computations--------------------------------------------------------------

  inline vnl_diag_matrix_fixed& invert_in_place();
  T determinant() const;
  vnl_vector_fixed<T,N> solve(vnl_vector_fixed<T,N> const& b) const;
  void solve(vnl_vector_fixed<T,N> const& b, vnl_vector_fixed<T,N>* out) const;

  // Data Access---------------------------------------------------------------

  inline T operator () (unsigned i, unsigned j) const {
    return (i != j) ? T(0) : diagonal_[i];
  }

  inline T& operator () (unsigned i, unsigned j) {
    assert(i == j);
    return diagonal_[i];
  }
  inline T& operator() (unsigned i) { return diagonal_[i]; }
  inline T const& operator() (unsigned i) const { return diagonal_[i]; }

  inline T& operator[] (unsigned i) { return diagonal_[i]; }
  inline T const& operator[] (unsigned i) const { return diagonal_[i]; }

  //: Return a vector (copy) with the content of the (main) diagonal
  inline vnl_vector_fixed<T,N> get_diagonal() const { return diagonal_; }

  //: Return diagonal elements as a vector
  inline vnl_vector_fixed<T,N> const& diagonal() const { return diagonal_; }

  //: Set all diagonal elements of matrix to specified value.
  inline vnl_diag_matrix_fixed& fill_diagonal (T const& v) { diagonal_.fill(v); return *this; }

  //: Sets the diagonal elements of this matrix to the specified list of values.
  inline vnl_diag_matrix_fixed& set_diagonal(vnl_vector_fixed<T,N> const& v) { diagonal_ = v; return *this; }

  // iterators

  typedef typename vnl_vector_fixed<T,N>::iterator iterator;
  inline iterator begin() { return diagonal_.begin(); }
  inline iterator end() { return diagonal_.end(); }
  typedef typename vnl_vector_fixed<T,N>::const_iterator const_iterator;
  inline const_iterator begin() const { return diagonal_.begin(); }
  inline const_iterator end() const { return diagonal_.end(); }

  //: Return the total number of elements stored by the matrix.
  // Since vnl_diag_matrix_fixed only stores values on the diagonal
  // and must be square, size() == rows() == cols().
  inline unsigned int size() const { return diagonal_.size(); }

  //: Return the number of rows.
  inline unsigned int rows() const { return diagonal_.size(); }

  //: Return the number of columns.
  // A synonym for columns().
  inline unsigned int cols() const { return diagonal_.size(); }

  //: Return the number of columns.
  // A synonym for cols().
  inline unsigned int columns() const { return diagonal_.size(); }

  //: set element with boundary checks.
  inline void put (unsigned r, unsigned c, T const& v)
  {
    assert(r == c);
    (void)c;
#if VNL_CONFIG_CHECK_BOUNDS
    if (r >= this->size())                  // If invalid size specified
      vnl_error_matrix_row_index("put", r); // Raise exception
#endif
    diagonal_[r] = v;
  }

  //: get element with boundary checks.
  inline T get (unsigned r, unsigned c) const
  {
    assert(r == c);
    (void)c;
#if VNL_CONFIG_CHECK_BOUNDS
    if (r >= this->size())                  // If invalid size specified
      vnl_error_matrix_row_index("get", r); // Raise exception
#endif
    return diagonal_[r];
  }

  // Need this until we add a vnl_diag_matrix_fixed ctor to vnl_matrix;
  inline vnl_matrix_fixed<T,N,N> as_matrix_fixed() const;

  inline vnl_matrix_fixed<T,N,N> as_ref() const { return as_matrix_fixed(); }

  // This is as good as a vnl_diag_matrix_fixed ctor for vnl_matrix_fixed:
  inline operator vnl_matrix_fixed<T,N,N> () const { return as_matrix_fixed(); }

  inline vnl_diag_matrix_fixed& fill(T const &x) { diagonal_.fill(x); return *this; }

  //: Return pointer to the diagonal elements as a contiguous 1D C array;
  inline T*       data_block()       { return diagonal_.data_block(); }
  inline T const* data_block() const { return diagonal_.data_block(); }

  //: Set diagonal elements using vector, then return *this
  inline vnl_diag_matrix_fixed& set(vnl_vector_fixed<T,N> const& v)  { diagonal_=v; return *this; }

 private:
};

//:
// \relatesalso vnl_diag_matrix_fixed
template <class T, unsigned int N> VNL_TEMPLATE_EXPORT
std::ostream& operator<< (std::ostream&, vnl_diag_matrix_fixed<T,N> const&);

//: Convert a vnl_diag_matrix_fixed to a Matrix.
template <class T, unsigned int N>
inline vnl_matrix_fixed<T,N,N> vnl_diag_matrix_fixed<T,N>::as_matrix_fixed() const
{
  vnl_matrix_fixed<T,N,N> ret;
  for (unsigned i = 0; i < N; ++i)
  {
    unsigned j;
    for (j = 0; j < i; ++j)
      ret(i,j) = T(0);
    for (j = i+1; j < N; ++j)
      ret(i,j) = T(0);
    ret(i,i) = diagonal_[i];
  }
  return ret;
}

//: Invert a vnl_diag_matrix_fixed in-situ, then returns *this.
// Just replaces each element with its reciprocal.
template <class T, unsigned int N>
inline vnl_diag_matrix_fixed<T,N>& vnl_diag_matrix_fixed<T,N>::invert_in_place()
{
  T* d = data_block();
  T one = T(1);
  for (unsigned i = 0; i < N; ++i)
    d[i] = one / d[i];
  return *this;
}

//: Return determinant as product of diagonal values.
template <class T, unsigned int N>
inline T vnl_diag_matrix_fixed<T,N>::determinant() const
{
  T det = T(1);
  T const* d = data_block();
  for (unsigned i = 0; i < N; ++i)
    det *= d[i];
  return det;
}

//: Multiply two vnl_diag_matrices.  Just multiply the diag elements - n flops
// \relatesalso vnl_diag_matrix_fixed
template <class T, unsigned int N>
inline vnl_diag_matrix_fixed<T,N> operator* (vnl_diag_matrix_fixed<T,N> const& A, vnl_diag_matrix_fixed<T,N> const& B)
{
  vnl_diag_matrix_fixed<T,N> ret = A;
  for (unsigned i = 0; i < N; ++i)
    ret(i,i) *= B(i,i);
  return ret;
}

//: Multiply a vnl_matrix by a vnl_diag_matrix_fixed.  Just scales the columns - mn flops
// \relatesalso vnl_diag_matrix_fixed
// \relatesalso vnl_matrix
template <class T, unsigned int R, unsigned int C>
inline vnl_matrix_fixed<T,R,C> operator* (vnl_matrix_fixed<T,R,C> const& A, vnl_diag_matrix_fixed<T,C> const& D)
{
  vnl_matrix_fixed<T,R,C> ret;
  for (unsigned i = 0; i < R; ++i)
    for (unsigned j = 0; j < C; ++j)
      ret(i,j) = A(i,j) * D(j,j);
  return ret;
}

//: Multiply a vnl_diag_matrix_fixed by a vnl_matrix.  Just scales the rows - mn flops
// \relatesalso vnl_diag_matrix_fixed
// \relatesalso vnl_matrix
template <class T, unsigned int R, unsigned int C>
inline vnl_matrix_fixed<T,R,C> operator* (vnl_diag_matrix_fixed<T,R> const& D, vnl_matrix_fixed<T,R,C> const& A)
{
  vnl_matrix_fixed<T,R,C> ret;
  T const* d = D.data_block();
  for (unsigned i = 0; i < R; ++i)
    for (unsigned j = 0; j < C; ++j)
      ret(i,j) = A(i,j) * d[i];
  return ret;
}

//: Add two vnl_diag_matrices.  Just add the diag elements - n flops
// \relatesalso vnl_diag_matrix_fixed
template <class T, unsigned int N>
inline vnl_diag_matrix_fixed<T,N> operator+ (vnl_diag_matrix_fixed<T,N> const& A, vnl_diag_matrix_fixed<T,N> const& B)
{
  vnl_diag_matrix_fixed<T,N> ret = A;
  for (unsigned i = 0; i < N; ++i)
    ret(i,i) += B(i,i);
  return ret;
}

//: Add a vnl_diag_matrix_fixed to a vnl_matrix.  n adds, mn copies.
// \relatesalso vnl_diag_matrix_fixed
// \relatesalso vnl_matrix
template <class T, unsigned int N>
inline vnl_matrix_fixed<T,N,N> operator+ (vnl_matrix_fixed<T,N,N> const& A, vnl_diag_matrix_fixed<T,N> const& D)
{
  vnl_matrix_fixed<T,N,N> ret(A);
  T const* d = D.data_block();
  for (unsigned j = 0; j < N; ++j)
    ret(j,j) += d[j];
  return ret;
}

//: Add a vnl_matrix to a vnl_diag_matrix_fixed.  n adds, mn copies.
// \relatesalso vnl_diag_matrix_fixed
// \relatesalso vnl_matrix
template <class T, unsigned int N>
inline vnl_matrix_fixed<T,N,N> operator+ (vnl_diag_matrix_fixed<T,N> const& D, vnl_matrix_fixed<T,N,N> const& A)
{
  return A + D;
}

//: Subtract two vnl_diag_matrices.  Just subtract the diag elements - n flops
// \relatesalso vnl_diag_matrix_fixed
template <class T, unsigned int N>
inline vnl_diag_matrix_fixed<T,N> operator- (vnl_diag_matrix_fixed<T,N> const& A, vnl_diag_matrix_fixed<T,N> const& B)
{
  vnl_diag_matrix_fixed<T,N> ret(A);
  for (unsigned i = 0; i < N; ++i)
    ret(i,i) -= B(i,i);
  return ret;
}

//: Subtract a vnl_diag_matrix_fixed from a vnl_matrix.  n adds, mn copies.
// \relatesalso vnl_diag_matrix_fixed
// \relatesalso vnl_matrix
template <class T, unsigned int N>
inline vnl_matrix_fixed<T,N,N> operator- (vnl_matrix_fixed<T,N,N> const& A, vnl_diag_matrix_fixed<T,N> const& D)
{
  vnl_matrix_fixed<T,N,N> ret(A);
  T const* d = D.data_block();
  for (unsigned j = 0; j < N; ++j)
    ret(j,j) -= d[j];
  return ret;
}

//: Subtract a vnl_matrix from a vnl_diag_matrix_fixed.  n adds, mn copies.
// \relatesalso vnl_diag_matrix_fixed
// \relatesalso vnl_matrix
template <class T, unsigned int N>
inline vnl_matrix_fixed<T,N,N> operator- (vnl_diag_matrix_fixed<T,N> const& D, vnl_matrix_fixed<T,N,N> const& A)
{
  vnl_matrix_fixed<T,N,N> ret;
  T const* d = D.data_block();
  for (unsigned i = 0; i < N; ++i)
  {
    for (unsigned j = 0; j < i; ++j)
      ret(i,j) = -A(i,j);
    for (unsigned j = i+1; j < N; ++j)
      ret(i,j) = -A(i,j);
    ret(i,i) = d[i] - A(i,i);
  }
  return ret;
}

//: Multiply a vnl_diag_matrix_fixed by a vnl_vector_fixed.  n flops.
// \relatesalso vnl_diag_matrix_fixed
// \relatesalso vnl_vector_fixed
template <class T, unsigned int N>
inline vnl_vector_fixed<T,N> operator* (vnl_diag_matrix_fixed<T,N> const& D, vnl_vector_fixed<T,N> const& A)
{
  return element_product(D.diagonal(), A);
}

//: Multiply a vnl_vector_fixed by a vnl_diag_matrix_fixed.  n flops.
// \relatesalso vnl_diag_matrix_fixed
// \relatesalso vnl_vector_fixed
template <class T, unsigned int N>
inline vnl_vector_fixed<T,N> operator* (vnl_vector_fixed<T,N> const& A, vnl_diag_matrix_fixed<T,N> const& D)
{
  return element_product(D.diagonal(), A);
}

#endif // vnl_diag_matrix_fixed_h_
