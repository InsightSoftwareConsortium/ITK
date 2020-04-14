// This is core/vnl/vnl_diag_matrix.h
#ifndef vnl_diag_matrix_h_
#define vnl_diag_matrix_h_
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

#include <cassert>
#include <iosfwd>
#include <utility>

#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include "vnl_vector.h"
#include "vnl_matrix.h"
#include "vnl/vnl_export.h"

// forward declarations
template <class T> class vnl_diag_matrix;
template <class T> VNL_EXPORT vnl_vector<T> operator*(vnl_diag_matrix<T> const&, vnl_vector<T> const&);

//: stores a diagonal matrix as a single vector.
//  vnl_diag_matrix stores a diagonal matrix for time and space efficiency.
//  Specifically, only the diagonal elements are stored, and some matrix
//  operations (currently *, + and -) are overloaded to use more efficient
//  algorithms.

template <class T>
class VNL_EXPORT vnl_diag_matrix
{
  vnl_vector<T> diagonal_;

 public:
  vnl_diag_matrix() = default;
  vnl_diag_matrix(const vnl_diag_matrix<T> &)  = default;
  vnl_diag_matrix(vnl_diag_matrix<T> &&)  = default;
  vnl_diag_matrix& operator=(const vnl_diag_matrix<T> &)  = default;
  vnl_diag_matrix& operator=(vnl_diag_matrix<T> &&)  = default;
  ~vnl_diag_matrix() = default;

  //: Construct an empty diagonal matrix.
  vnl_diag_matrix(unsigned nn) : diagonal_(nn) {}

  //: Construct a diagonal matrix with diagonal elements equal to value.
  vnl_diag_matrix(unsigned nn, T const& value) : diagonal_(nn, value) {}

  //: Construct a diagonal matrix from a vnl_vector.
  //  The vector elements become the diagonal elements.
  vnl_diag_matrix(vnl_vector<T> that) : diagonal_(std::move(that)) {}

  // Operations----------------------------------------------------------------

  //: In-place arithmetic operation
  inline vnl_diag_matrix<T>& operator*=(T v) { diagonal_ *= v; return *this; }
  //: In-place arithmetic operation
  inline vnl_diag_matrix<T>& operator/=(T v) { diagonal_ /= v; return *this; }

  // Computations--------------------------------------------------------------

  vnl_diag_matrix& invert_in_place();
  T determinant() const;
  vnl_vector<T> solve(vnl_vector<T> const& b) const;
  void solve(vnl_vector<T> const& b, vnl_vector<T>* out) const;

  // Data Access---------------------------------------------------------------

  inline T operator () (unsigned i, unsigned j) const {
    return (i != j) ? T(0) : diagonal_[i];
  }

  inline T& operator () (unsigned i, unsigned j) {
    assert(i == j); (void)j;
    return diagonal_[i];
  }
  inline T& operator() (unsigned i) { return diagonal_[i]; }
  inline T const& operator() (unsigned i) const { return diagonal_[i]; }

  inline T& operator[] (unsigned i) { return diagonal_[i]; }
  inline T const& operator[] (unsigned i) const { return diagonal_[i]; }

  //: Return a vector (copy) with the content of the (main) diagonal
  inline vnl_vector<T> get_diagonal() const { return diagonal_; }

  //: Return diagonal elements as a vector
  inline vnl_vector<T> const& diagonal() const { return diagonal_; }

  //: Set all diagonal elements of matrix to specified value.
  inline vnl_diag_matrix& fill_diagonal (T const& v) { diagonal_.fill(v); return *this; }

  //: Sets the diagonal elements of this matrix to the specified list of values.
  inline vnl_diag_matrix& set_diagonal(vnl_vector<T> const& v) { diagonal_ = v; return *this; }

  // iterators

  typedef typename vnl_vector<T>::iterator iterator;
  inline iterator begin() { return diagonal_.begin(); }
  inline iterator end() { return diagonal_.end(); }
  typedef typename vnl_vector<T>::const_iterator const_iterator;
  inline const_iterator begin() const { return diagonal_.begin(); }
  inline const_iterator end() const { return diagonal_.end(); }

  //: Return the total number of elements stored by the matrix.
  // Since vnl_diag_matrix only stores values on the diagonal
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
  inline void put (unsigned r, unsigned c, T const& v) {
    assert(r == c);
    (void)c;
#if VNL_CONFIG_CHECK_BOUNDS
    if (r >= this->size())                  // If invalid size specified
      {
      vnl_error_matrix_row_index("get", r); // Raise exception
      }
#endif
    diagonal_[r] = v;
  }

  //: get element with boundary checks.
  inline T get (unsigned r, unsigned c) const {
    assert(r == c);
    (void)c;
#if VNL_CONFIG_CHECK_BOUNDS
    if (r >= this->size())                  // If invalid size specified
      {
      vnl_error_matrix_row_index("get", r); // Raise exception
      }
#endif
    return diagonal_[r];
  }

#if VXL_LEGACY_FUTURE_REMOVE
  VXL_DEPRECATED_MSG("Deprecated inconsistent return type.\nWARNING: .as_ref returns a vnl_matrix, not a vnl_matrix_ref, use .as_matrix() directly")
#endif
  vnl_matrix<T> as_ref() const { return as_matrix(); }

  // Need this until we add a vnl_diag_matrix ctor to vnl_matrix;
  vnl_matrix<T> as_matrix( ) const;
#if ! VXL_LEGACY_FUTURE_REMOVE
  VXL_DEPRECATED_MSG("Deprecated inconsistent name.\nUSE: .as_matrix() new consistent name.")
  vnl_matrix<T> asMatrix () const { return this->as_matrix(); }
#endif

  // This is as good as a vnl_diag_matrix ctor for vnl_matrix:
#if ! VXL_USE_HISTORICAL_IMPLICIT_CONVERSIONS
  explicit operator vnl_matrix<T> () const { return this->as_matrix(); }
#else
#if VXL_LEGACY_FUTURE_REMOVE
  VXL_DEPRECATED_MSG("Implicit cast conversion is dangerous.\nUSE: .as_vector() or .as_ref() member function for clarity.")
#endif
  operator vnl_matrix<T> () const { return this->as_matrix(); }
#endif

  inline void set_size(int n) { diagonal_.set_size(n); }

  inline void clear() { diagonal_.clear(); }
  inline vnl_diag_matrix& fill(T const &x) { diagonal_.fill(x); return *this; }

  //: Return pointer to the diagonal elements as a contiguous 1D C array;
  inline T*       data_block()       { return diagonal_.data_block(); }
  inline T const* data_block() const { return diagonal_.data_block(); }

  //: Set diagonal elements using vector
  inline vnl_diag_matrix& set(vnl_vector<T> const& v)  { diagonal_=v; return *this; }

 private:
};

//:
// \relatesalso vnl_diag_matrix
template <class T> VNL_EXPORT
std::ostream& operator<< (std::ostream&, vnl_diag_matrix<T> const&);

//: Convert a vnl_diag_matrix to a Matrix.
template <class T>
vnl_matrix<T> vnl_diag_matrix<T>::as_matrix() const
{
  unsigned len = diagonal_.size();
  vnl_matrix<T> ret(len, len);
  for (unsigned i = 0; i < len; ++i)
  {
    unsigned j;
    for (j = 0; j < i; ++j)
      ret(i,j) = T(0);
    for (j = i+1; j < len; ++j)
      ret(i,j) = T(0);
    ret(i,i) = diagonal_[i];
  }
  return ret;
}

//: Invert a vnl_diag_matrix in-situ.
// Just replaces each element with its reciprocal.
template <class T>
inline vnl_diag_matrix<T>& vnl_diag_matrix<T>::invert_in_place()
{
  unsigned len = diagonal_.size();
  T* d = data_block();
  T one = T(1);
  for (unsigned i = 0; i < len; ++i)
    d[i] = one / d[i];
  return *this;
}

//: Return determinant as product of diagonal values.
template <class T>
inline T vnl_diag_matrix<T>::determinant() const
{
  T det = T(1);
  T const* d = data_block();
  unsigned len = diagonal_.size();
  for (unsigned i = 0; i < len; ++i)
    det *= d[i];
  return det;
}

//: Multiply two vnl_diag_matrices.  Just multiply the diag elements - n flops
// \relatesalso vnl_diag_matrix
template <class T>
inline vnl_diag_matrix<T> operator* (vnl_diag_matrix<T> const& A, vnl_diag_matrix<T> const& B)
{
  assert(A.size() == B.size());
  vnl_diag_matrix<T> ret = A;
  for (unsigned i = 0; i < A.size(); ++i)
    ret(i,i) *= B(i,i);
  return ret;
}

//: Multiply a vnl_matrix by a vnl_diag_matrix.  Just scales the columns - mn flops
// \relatesalso vnl_diag_matrix
// \relatesalso vnl_matrix
template <class T>
inline vnl_matrix<T> operator* (vnl_matrix<T> const& A, vnl_diag_matrix<T> const& D)
{
  assert(A.columns() == D.size());
  vnl_matrix<T> ret(A.rows(), A.columns());
  for (unsigned i = 0; i < A.rows(); ++i)
    for (unsigned j = 0; j < A.columns(); ++j)
      ret(i,j) = A(i,j) * D(j,j);
  return ret;
}

//: Multiply a vnl_diag_matrix by a vnl_matrix.  Just scales the rows - mn flops
// \relatesalso vnl_diag_matrix
// \relatesalso vnl_matrix
template <class T>
inline vnl_matrix<T> operator* (vnl_diag_matrix<T> const& D, vnl_matrix<T> const& A)
{
  assert(A.rows() == D.size());
  vnl_matrix<T> ret(A.rows(), A.columns());
  T const* d = D.data_block();
  for (unsigned i = 0; i < A.rows(); ++i)
    for (unsigned j = 0; j < A.columns(); ++j)
      ret(i,j) = A(i,j) * d[i];
  return ret;
}

//: Add two vnl_diag_matrices.  Just add the diag elements - n flops
// \relatesalso vnl_diag_matrix
template <class T>
inline vnl_diag_matrix<T> operator+ (vnl_diag_matrix<T> const& A, vnl_diag_matrix<T> const& B)
{
  assert(A.size() == B.size());
  vnl_diag_matrix<T> ret = A;
  for (unsigned i = 0; i < A.size(); ++i)
    ret(i,i) += B(i,i);
  return ret;
}

//: Add a vnl_diag_matrix to a vnl_matrix.  n adds, mn copies.
// \relatesalso vnl_diag_matrix
// \relatesalso vnl_matrix
template <class T>
inline vnl_matrix<T> operator+ (vnl_matrix<T> const& A, vnl_diag_matrix<T> const& D)
{
  const unsigned n = D.size();
  assert(A.rows() == n); assert(A.columns() == n);
  vnl_matrix<T> ret(A);
  T const* d = D.data_block();
  for (unsigned j = 0; j < n; ++j)
    ret(j,j) += d[j];
  return ret;
}

//: Add a vnl_matrix to a vnl_diag_matrix.  n adds, mn copies.
// \relatesalso vnl_diag_matrix
// \relatesalso vnl_matrix
template <class T>
inline vnl_matrix<T> operator+ (vnl_diag_matrix<T> const& D, vnl_matrix<T> const& A)
{
  return A + D;
}

//: Subtract two vnl_diag_matrices.  Just subtract the diag elements - n flops
// \relatesalso vnl_diag_matrix
template <class T>
inline vnl_diag_matrix<T> operator- (vnl_diag_matrix<T> const& A, vnl_diag_matrix<T> const& B)
{
  assert(A.size() == B.size());
  vnl_diag_matrix<T> ret = A;
  for (unsigned i = 0; i < A.size(); ++i)
    ret(i,i) -= B(i,i);
  return ret;
}

//: Subtract a vnl_diag_matrix from a vnl_matrix.  n adds, mn copies.
// \relatesalso vnl_diag_matrix
// \relatesalso vnl_matrix
template <class T>
inline vnl_matrix<T> operator- (vnl_matrix<T> const& A, vnl_diag_matrix<T> const& D)
{
  const unsigned n = D.size();
  assert(A.rows() == n); assert(A.columns() == n);
  vnl_matrix<T> ret(A);
  T const* d = D.data_block();
  for (unsigned j = 0; j < n; ++j)
    ret(j,j) -= d[j];
  return ret;
}

//: Subtract a vnl_matrix from a vnl_diag_matrix.  n adds, mn copies.
// \relatesalso vnl_diag_matrix
// \relatesalso vnl_matrix
template <class T>
inline vnl_matrix<T> operator- (vnl_diag_matrix<T> const& D, vnl_matrix<T> const& A)
{
  const unsigned n = D.size();
  assert(A.rows() == n); assert(A.columns() == n);
  vnl_matrix<T> ret(n, n);
  T const* d = D.data_block();
  for (unsigned i = 0; i < n; ++i)
  {
    for (unsigned j = 0; j < i; ++j)
      ret(i,j) = -A(i,j);
    for (unsigned j = i+1; j < n; ++j)
      ret(i,j) = -A(i,j);
    ret(i,i) = d[i] - A(i,i);
  }
  return ret;
}

//: Multiply a vnl_diag_matrix by a vnl_vector.  n flops.
// \relatesalso vnl_diag_matrix
// \relatesalso vnl_vector
template <class T>
inline vnl_vector<T> operator* (vnl_diag_matrix<T> const& D, vnl_vector<T> const& A)
{
  assert(A.size() == D.size());
  return element_product(D.diagonal(), A);
}

//: Multiply a vnl_vector by a vnl_diag_matrix.  n flops.
// \relatesalso vnl_diag_matrix
// \relatesalso vnl_vector
template <class T>
inline vnl_vector<T> operator* (vnl_vector<T> const& A, vnl_diag_matrix<T> const& D)
{
  assert(A.size() == D.size());
  return element_product(D.diagonal(), A);
}

#endif // vnl_diag_matrix_h_
