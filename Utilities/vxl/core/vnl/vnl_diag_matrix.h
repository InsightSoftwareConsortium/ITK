// This is core/vnl/vnl_diag_matrix.h
#ifndef vnl_diag_matrix_h_
#define vnl_diag_matrix_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Contains class for diagonal matrices
// \author Andrew W. Fitzgibbon (Oxford RRG)
// \date   5/8/96
//
// \verbatim
//  Modifications
//   IMS (Manchester) 16/03/2001: Tidied up the documentation + added binary_io
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
//   Sep.2002 - Peter Vanroose - Added operator+, operator-, operator*
//   Mar.2004 - Peter Vanroose - removed deprecated resize()
// \endverbatim

#include <vcl_cassert.h>
#include <vcl_iosfwd.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

// forward declarations
template <class T> class vnl_diag_matrix;
template <class T> vnl_vector<T> operator*(vnl_diag_matrix<T> const&, vnl_vector<T> const&);

//: stores a diagonal matrix as a single vector.
//  vnl_diag_matrix stores a diagonal matrix for time and space efficiency.
//  Specifically, only the diagonal elements are stored, and some matrix
//  operations (currently *, + and -) are overloaded to use more efficient
//  algorithms.

export
template <class T>
class vnl_diag_matrix
{
  vnl_vector<T> diagonal_;

 public:
  vnl_diag_matrix() {}

  //: Construct an empty diagonal matrix.
  vnl_diag_matrix(unsigned nn) : diagonal_(nn) {}

  //: Construct a diagonal matrix with diagonal elements equal to value.
  vnl_diag_matrix(unsigned nn, T const& value) : diagonal_(nn, value) {}

  //: Construct a diagonal matrix from a vnl_vector.
  //  The vector elements become the diagonal elements.
  vnl_diag_matrix(vnl_vector<T> const& that): diagonal_(that) {}
 ~vnl_diag_matrix() {}

  inline vnl_diag_matrix& operator=(vnl_diag_matrix<T> const& that) {
    this->diagonal_ = that.diagonal_;
    return *this;
  }

  // Operations----------------------------------------------------------------

  //: In-place arithmetic operation
  inline vnl_diag_matrix<T>& operator*=(T v) { diagonal_ *= v; return *this; }
  //: In-place arithmetic operation
  inline vnl_diag_matrix<T>& operator/=(T v) { diagonal_ /= v; return *this; }

  // Computations--------------------------------------------------------------

  void invert_in_place();
  T determinant() const;
  vnl_vector<T> solve(vnl_vector<T> const& b);
  void solve(vnl_vector<T> const& b, vnl_vector<T>* out);

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

  //: set element with boundary checks.
  inline void put (unsigned r, unsigned c, T const& v) {
    assert(r == c); assert (r<size()); diagonal_[r] = v;
  }

  //: get element with boundary checks.
  inline T get (unsigned r, unsigned c) const {
    assert(r == c); assert (r<size()); return diagonal_[r];
  }

  //: Set all diagonal elements of matrix to specified value.
  inline void fill_diagonal (T const& v) { diagonal_.fill(v); }

  // iterators

  typedef typename vnl_vector<T>::iterator iterator;
  inline iterator begin() { return diagonal_.begin(); }
  inline iterator end() { return diagonal_.end(); }
  typedef typename vnl_vector<T>::const_iterator const_iterator;
  inline const_iterator begin() const { return diagonal_.begin(); }
  inline const_iterator end() const { return diagonal_.end(); }

  inline unsigned size() const { return diagonal_.size(); }
  inline unsigned rows() const { return diagonal_.size(); }
  inline unsigned cols() const { return diagonal_.size(); }
  inline unsigned columns() const { return diagonal_.size(); }

  // Need this until we add a vnl_diag_matrix ctor to vnl_matrix;
  inline vnl_matrix<T> asMatrix() const;

  inline vnl_matrix<T> as_ref() const { return asMatrix(); }

  // This is as good as a vnl_diag_matrix ctor for vnl_matrix:
  inline operator vnl_matrix<T> () const { return asMatrix(); }

  inline void set_size(int n) { diagonal_.set_size(n); }

  inline void clear() { diagonal_.clear(); }
  inline void fill(T const &x) { diagonal_.fill(x); }

  //: Return pointer to the diagonal elements as a contiguous 1D C array;
  inline T*       data_block()       { return diagonal_.data_block(); }
  inline T const* data_block() const { return diagonal_.data_block(); }

  //: Return diagonal elements as a vector
  inline vnl_vector<T> const& diagonal() const { return diagonal_; }

  //: Set diagonal elements using vector
  inline void set(vnl_vector<T> const& v)  { diagonal_=v; }

 private:
  #if VCL_NEED_FRIEND_FOR_TEMPLATE_OVERLOAD
  friend vnl_vector<T> operator*(vnl_diag_matrix<T> const&,vnl_vector<T> const&);
  #endif
};

//:
// \relates vnl_diag_matrix
template <class T>
vcl_ostream& operator<< (vcl_ostream&, vnl_diag_matrix<T> const&);

//: Convert a vnl_diag_matrix to a Matrix.
template <class T>
inline vnl_matrix<T> vnl_diag_matrix<T>::asMatrix() const
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
inline void vnl_diag_matrix<T>::invert_in_place()
{
  unsigned len = diagonal_.size();
  T* d = data_block();
  T one = T(1);
  for (unsigned i = 0; i < len; ++i)
    d[i] = one / d[i];
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
// \relates vnl_diag_matrix
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
// \relates vnl_diag_matrix
// \relates vnl_matrix
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
// \relates vnl_diag_matrix
// \relates vnl_matrix
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
// \relates vnl_diag_matrix
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
// \relates vnl_diag_matrix
// \relates vnl_matrix
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
// \relates vnl_diag_matrix
// \relates vnl_matrix
template <class T>
inline vnl_matrix<T> operator+ (vnl_diag_matrix<T> const& D, vnl_matrix<T> const& A)
{
  return A + D;
}

//: Subtract two vnl_diag_matrices.  Just subtract the diag elements - n flops
// \relates vnl_diag_matrix
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
// \relates vnl_diag_matrix
// \relates vnl_matrix
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
// \relates vnl_diag_matrix
// \relates vnl_matrix
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
// \relates vnl_diag_matrix
// \relates vnl_vector
template <class T>
inline vnl_vector<T> operator* (vnl_diag_matrix<T> const& D, vnl_vector<T> const& A)
{
  assert(A.size() == D.size());
  return element_product(D.diagonal(), A);
}

//: Multiply a vnl_vector by a vnl_diag_matrix.  n flops.
// \relates vnl_diag_matrix
// \relates vnl_vector
template <class T>
inline vnl_vector<T> operator* (vnl_vector<T> const& A, vnl_diag_matrix<T> const& D)
{
  assert(A.size() == D.size());
  return element_product(D.diagonal(), A);
}

#endif // vnl_diag_matrix_h_
