// This is core/vnl/vnl_sym_matrix.h
#ifndef vnl_sym_matrix_h_
#define vnl_sym_matrix_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Contains class for symmetric matrices
// \author Ian Scott (Manchester ISBE)
// \date   6 Dec 2001

#include <iosfwd>
#include <vcl_cassert.h>
#include <vcl_compiler.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_c_vector.h>
#include "vnl/vnl_export.h"

VCL_TEMPLATE_EXPORT template <class T> class vnl_sym_matrix;

//: stores a symmetric matrix as just the diagonal and lower triangular part
//  vnl_sym_matrix stores a symmetric matrix for time and space efficiency.
//  Specifically, only the diagonal and lower triangular elements are stored.

template <class T>
class VNL_TEMPLATE_EXPORT vnl_sym_matrix
{
 public:
  //: Construct an empty symmetric matrix.
  vnl_sym_matrix(): data_(0), index_(0), nn_(0) {}

  //: Construct a symmetric matrix of size nn by nn.
  explicit vnl_sym_matrix(unsigned nn):
  data_(vnl_c_vector<T>::allocate_T(nn * (nn + 1) / 2)),
  index_(vnl_c_vector<T>::allocate_Tptr(nn)),
  nn_(nn) { setup_index(); }

  //: Construct a symmetric matrix with elements equal to data
  // Value should be stored row-wise, and contain the
  // n*(n+1)/2 diagonal and lower triangular elements
  inline vnl_sym_matrix(T const * data, unsigned nn);

  //: Construct a symmetric matrix with all elements equal to value
  inline vnl_sym_matrix(unsigned nn, const T & value);

  //: Construct a symmetric matrix from a full matrix.
  // If NDEBUG is set, the symmetry of the matrix will be asserted.
  inline explicit vnl_sym_matrix(vnl_matrix<T> const& that);

  //: Copy constructor
  inline vnl_sym_matrix(vnl_sym_matrix<T> const& that);

  ~vnl_sym_matrix();

  vnl_sym_matrix<T>& operator=(vnl_sym_matrix<T> const& that);

  // Operations----------------------------------------------------------------

  //: In-place arithmetic operations
  inline vnl_sym_matrix<T>& operator*=(T v) { vnl_c_vector<T>::scale(data_, data_, size(), v); return *this; }
  //: In-place arithmetic operations
  inline vnl_sym_matrix<T>& operator/=(T v) { vnl_c_vector<T>::scale(data_, data_, size(), ((T)1)/v); return *this; }


  // Data Access---------------------------------------------------------------

  inline T operator () (unsigned i, unsigned j) const {
    return (i > j) ? index_[i][j] : index_[j][i];
  }

  inline T& operator () (unsigned i, unsigned j) {
    return (i > j) ? index_[i][j] : index_[j][i];
  }

  //: Access a half-row of data.
  // Only the first i+1 values from this pointer are valid.
  inline const T* operator [] (unsigned i) const {
    assert (i < nn_);
    return index_[i];
  }

  //: fast access, however i >= j
  inline T fast (unsigned i, unsigned j) const {
    assert (i >= j);
    return index_[i][j];
  }

  //: fast access, however i >= j
  inline T& fast (unsigned i, unsigned j) {
    assert (i >= j);
    return index_[i][j];
  }

  // iterators

  typedef T* iterator;
  inline iterator begin() { return data_; }
  inline iterator end() { return data_ + size(); }
  typedef const T* const_iterator;
  inline const_iterator begin() const { return data_; }
  inline const_iterator end() const { return data_ + size(); }

  //: Return the total number of elements stored by the matrix.
  // Since vnl_sym_matrix only stores the diagonal and lower
  // triangular elements and must be square, this returns
  // n*(n+1)/2, where n == rows() == cols().
  inline unsigned int size() const { return nn_ * (nn_ + 1) / 2; }

  //: Return the number of rows.
  inline unsigned int rows() const { return nn_; }

  //: Return the number of columns.
  // A synonym for columns().
  inline unsigned int cols() const { return nn_; }

  //: Return the number of columns.
  // A synonym for cols().
  inline unsigned int columns() const { return nn_; }

  //: set element
  inline void put (unsigned r, unsigned c, T const& v)
  {
#if VNL_CONFIG_CHECK_BOUNDS
    if (r >= this->nn_)                // If invalid size specified
      vnl_error_matrix_row_index("put", r); // Raise exception
    if (c >= this->nn_)                // If invalid size specified
      vnl_error_matrix_col_index("put", c); // Raise exception
#endif
    (r > c) ? index_[r][c] = v : index_[c][r] = v;
  }

  //: get element
  inline T get (unsigned r, unsigned c) const
  {
#if VNL_CONFIG_CHECK_BOUNDS
    if (r >= this->nn_)                // If invalid size specified
      vnl_error_matrix_row_index("get", r); // Raise exception
    if (c >= this->nn_)                // If invalid size specified
      vnl_error_matrix_col_index("get", c); // Raise exception
#endif
    return (r > c) ? index_[r][c] : index_[c][r];
  }

  // Need this until we add a vnl_sym_matrix ctor to vnl_matrix;
  inline vnl_matrix<T> as_matrix() const;

  //: Resize matrix to n by n.
  // You will loose any existing data.
  inline void set_size(int n);


  //: Return pointer to the lower triangular elements as a contiguous 1D C array;
  T*       data_block()       { return data_; }
  //: Return pointer to the lower triangular elements as a contiguous 1D C array;
  T const* data_block() const { return data_; }

  //: Set the first i values of row i
  // or the top i values of column i
  void set_half_row (const vnl_vector<T> &half_row, unsigned i);

  //: Replaces the symmetric submatrix of THIS matrix with the elements of matrix m.
  // Starting at top left corner. Complexity is $O(m^2)$.
  vnl_sym_matrix<T>& update (vnl_sym_matrix<T> const& m, unsigned diag_start=0);

  //: Swap contents of m with THIS
  void swap(vnl_sym_matrix &m);

 protected:
//: Set up the index array
  void setup_index();

  T* data_;
  T** index_;
  unsigned nn_;
};

//:
// \relatesalso vnl_sym_matrix
template <class T> VNL_TEMPLATE_EXPORT std::ostream& operator<< (std::ostream&, vnl_sym_matrix<T> const&);


template <class T>
inline vnl_sym_matrix<T>::vnl_sym_matrix(T const * data, unsigned nn):
  data_(vnl_c_vector<T>::allocate_T(nn * (nn + 1) / 2)),
  index_(vnl_c_vector<T>::allocate_Tptr(nn)),
  nn_(nn)
{
  setup_index();
  for (unsigned i = 0; i < nn_; ++i)
    for (unsigned j = 0; j <= i; ++j)
      fast(i,j) = *(data++);
}

template <class T>
inline vnl_sym_matrix<T>::vnl_sym_matrix(unsigned nn, const T & value):
  data_(vnl_c_vector<T>::allocate_T(nn * (nn + 1) / 2)),
  index_(vnl_c_vector<T>::allocate_Tptr(nn)),
  nn_(nn)
{
  setup_index();
  vnl_c_vector<T>::fill(data_, size(), value);
}


template <class T>
inline vnl_sym_matrix<T>::vnl_sym_matrix(vnl_matrix<T> const& that):
  data_(vnl_c_vector<T>::allocate_T(that.rows() * (that.rows() + 1) / 2)),
  index_(vnl_c_vector<T>::allocate_Tptr(that.rows())),
  nn_(that.rows())
{
  setup_index();
  assert (nn_ == that.cols());
  for (unsigned i = 0; i < nn_; ++i)
    for (unsigned j = 0; j <= i; ++j)
    {
      assert( that(i,j) == that(j,i) );
      fast(i,j) = that(i,j);
    }
}

template <class T>
inline vnl_sym_matrix<T>::vnl_sym_matrix(vnl_sym_matrix<T> const& that):
  data_(0), index_(0), nn_(0)
{
  set_size(that.rows());
  update(that);
}

//: Convert a vnl_sym_matrix to a vnl_matrix.
template <class T>
inline vnl_matrix<T> vnl_sym_matrix<T>::as_matrix() const
{
  vnl_matrix<T> ret(nn_, nn_);
  for (unsigned i = 0; i < nn_; ++i)
    for (unsigned j = 0; j <= i; ++j)
      ret(i,j) = ret(j,i) = fast(i,j);
  return ret;
}


template <class T>
inline void vnl_sym_matrix<T>::set_size(int n)
{
  if (n == (int)nn_) return;

  vnl_c_vector<T>::deallocate(data_, static_cast<std::size_t>(size()));
  vnl_c_vector<T>::deallocate(index_, static_cast<std::size_t>( nn_));

  nn_ = n;
  data_ = vnl_c_vector<T>::allocate_T(size());
  index_ = vnl_c_vector<T>::allocate_Tptr(n);

  setup_index();
}

template <class T> VNL_TEMPLATE_EXPORT
bool operator==(const vnl_sym_matrix<T> &a, const vnl_sym_matrix<T> &b);

template <class T> VNL_TEMPLATE_EXPORT
bool operator==(const vnl_sym_matrix<T> &a, const vnl_matrix<T> &b);

template <class T> VNL_TEMPLATE_EXPORT
bool operator==(const vnl_matrix<T> &a, const vnl_sym_matrix<T> &b);

//: Swap the contents of a and b.
// \relatesalso vnl_sym_matrix
template <class T> VNL_TEMPLATE_EXPORT
void swap(vnl_sym_matrix<T> &a, vnl_sym_matrix<T> &b)
{ a.swap(b); }

#endif // vnl_sym_matrix_h_
