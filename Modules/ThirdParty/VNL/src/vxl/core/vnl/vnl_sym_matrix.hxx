// This is core/vnl/vnl_sym_matrix.hxx
#ifndef vnl_sym_matrix_hxx_
#define vnl_sym_matrix_hxx_
//:
// \file

#include <iostream>
#include "vnl_sym_matrix.h"
#include <vcl_compiler.h>
#include <vnl/vnl_config.h> // for VNL_CONFIG_CHECK_BOUNDS

// ==========================================================================
//: Replaces the symmetric submatrix of THIS matrix, starting at top left corner, by the elements of matrix m.
// O(m*m).
template<class T>
vnl_sym_matrix<T>& vnl_sym_matrix<T>::update (vnl_sym_matrix<T> const& m,
                                              unsigned diagonal_start)
{
  unsigned int end_val = diagonal_start + m.nn_;
#if VNL_CONFIG_CHECK_BOUNDS  && (!defined NDEBUG)
  if (this->nn_ < end_val)
    vnl_error_matrix_dimension ("vnl_sym_matrix::update",
                                end_val, end_val, m.nn_, m.nn_);
#endif
  for (unsigned int i = diagonal_start; i < end_val; i++)
    for (unsigned int j = diagonal_start; j <= i; j++)
      this->fast(i,j) = m.fast(i-diagonal_start,j-diagonal_start);
  return *this;
}

// ==========================================================================
//: Swap contents of m with THIS
template <class T>
void vnl_sym_matrix<T>::swap(vnl_sym_matrix<T> &m)
{
  unsigned nn = nn_;
  T **index   = index_;
  T *data     = data_;
  nn_    =m.nn_;
  index_ =m.index_;
  data_  =m.data_;
  m.nn_    =nn;
  m.index_ =index;
  m.data_  =data;
}

// ==========================================================================

template <class T>
vnl_sym_matrix<T>& vnl_sym_matrix<T>::operator=(vnl_sym_matrix<T> const& that)
{
  if (&that == this) return *this;

  set_size(that.rows());
  update(that);
  return *this;
}
// ==========================================================================
template <class T>
vnl_sym_matrix<T>::~vnl_sym_matrix()
  {
    vnl_c_vector<T>::deallocate(data_, static_cast<std::size_t>( size() ) );
    vnl_c_vector<T>::deallocate(index_, static_cast<std::size_t> ( nn_ ) );
  }


template <class T>
void vnl_sym_matrix<T>::setup_index()
{
    T * data = data_;
    for (unsigned i=0; i< nn_; ++i) { index_[i] = data; data += i+1; }
}

// ==========================================================================
//: Set the first i values of row i
// or the top i values of column i
template <class T>
void vnl_sym_matrix<T>::set_half_row (const vnl_vector<T> &half_row, unsigned i)
{
#if VNL_CONFIG_CHECK_BOUNDS  && (!defined NDEBUG)
  if (half_row.size() != i+1)
    vnl_error_vector_dimension ("vnl_sym_matrix::set_half_row wrong size for half row",
                                half_row.size(), i+1);
  if ( i > nn_)
    vnl_error_vector_dimension ("vnl_sym_matrix::set_half_row wrong sizes",
                                i+1, rows());
#endif
  half_row.copy_out(index_[i]);
}

// ==========================================================================
//: print in lower triangular form
template <class T>
std::ostream& operator<< (std::ostream& s, const vnl_sym_matrix<T>& M)
{
  for (unsigned i=0; i<M.rows(); ++i)
  {
    for (unsigned j=0; j<=i; ++j)
      s << M.fast(i,j) << ' ';
    s  << '\n';
  }
  return s;
}

// ==========================================================================

template <class T>
bool operator==(const vnl_sym_matrix<T> &a, const vnl_sym_matrix<T> &b)
{
  if (a.rows() != b.rows()) return false;
  const T* a_data = a.data_block();
  const T* b_data = b.data_block();
  const unsigned mn = a.size();
  for (unsigned i = 0; i < mn; ++i)
    if (a_data[i] != b_data[i]) return false;
  return true;
}

// ==========================================================================

template <class T>
bool operator==(const vnl_sym_matrix<T> &a, const vnl_matrix<T> &b)
{
  if (a.rows() != b.rows() || a.cols() != b.cols()) return false;

  const unsigned n = a.rows();
  for (unsigned i=0; i< n; ++i)
  {
    for (unsigned j=0; j<i; ++j)
      if (a.fast(i,j) != b(i,j) || a.fast(i,j) != b(j,i)) return false;
    if (a.fast(i,i) != b(i,i)) return false;
  }
  return true;
}

// ==========================================================================

template <class T>
bool operator==(const vnl_matrix<T> &a, const vnl_sym_matrix<T> &b)
{
  return operator==(b,a);
}

// ==========================================================================

#undef VNL_SYM_MATRIX_INSTANTIATE
#define VNL_SYM_MATRIX_INSTANTIATE(T) \
template class VNL_EXPORT vnl_sym_matrix<T >; \
template VNL_EXPORT std::ostream& operator<< (std::ostream& s, vnl_sym_matrix<T > const &); \
template VNL_EXPORT bool operator==(const vnl_sym_matrix<T > &a, const vnl_sym_matrix<T > &b); \
template VNL_EXPORT bool operator==(const vnl_sym_matrix<T > &a, const vnl_matrix<T > &b); \
template VNL_EXPORT bool operator==(const vnl_matrix<T > &a, const vnl_sym_matrix<T > &b)

#endif // vnl_sym_matrix_hxx_
