// This is core/vnl/vnl_matrix_fixed_ref.hxx
#ifndef vnl_matrix_fixed_ref_hxx_
#define vnl_matrix_fixed_ref_hxx_

#include <cmath>
#include <iostream>
#include <cstdlib>
#include "vnl_matrix_fixed_ref.h"
//:
// \file

#include <vcl_compiler.h>
#include <vcl_cassert.h>

#include <vnl/vnl_error.h>
#include <vnl/vnl_math.h>

//------------------------------------------------------------

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> const&
vnl_matrix_fixed_ref<T,nrows,ncols>::fill (T value) const
{
  for (unsigned int i = 0; i < nrows; i++)
    for (unsigned int j = 0; j < ncols; j++)
      (*this)(i,j) = value;
  return *this;
}


template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> const&
vnl_matrix_fixed_ref<T,nrows,ncols>::fill_diagonal(T value) const
{
  for (unsigned int i = 0; i < nrows && i < ncols; i++)
    (*this)(i,i) = value;
  return *this;
}


template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> const&
vnl_matrix_fixed_ref<T,nrows,ncols>::set_diagonal(vnl_vector<T> const& diag) const
{
  assert(diag.size() >= nrows || diag.size() >= ncols);
  // The length of the diagonal of a non-square matrix is the minimum of
  // the matrix's width & height; that explains the "||" in the assert,
  // and the "&&" in the upper bound for the "for".
  for (unsigned int i = 0; i < nrows && i < ncols; ++i)
    (*this)(i,i) = diag[i];
  return *this;
}


template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed_ref_const<T,nrows,ncols>::print(std::ostream& os) const
{
  for (unsigned int i = 0; i < nrows; i++)
  {
    os << (*this)(i,0);
    for (unsigned int j = 1; j < ncols; j++)
      os << ' ' << (*this)(i,j);
    os << '\n';
  }
}


template <class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>
vnl_matrix_fixed_ref_const<T,nrows,ncols>::apply(T (*f)(T const&)) const
{
  vnl_matrix_fixed<T,nrows,ncols> ret;
  vnl_c_vector<T>::apply(this->begin(), rows()*cols(), f, ret.data_block());
  return ret;
}

template <class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>
vnl_matrix_fixed_ref_const<T,nrows,ncols>::apply(T (*f)(T)) const
{
  vnl_matrix_fixed<T,nrows,ncols> ret;
  vnl_c_vector<T>::apply(this->begin(), rows()*cols(), f, ret.data_block());
  return ret;
}

////--------------------------- Additions------------------------------------


template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,ncols,nrows>
vnl_matrix_fixed_ref_const<T,nrows,ncols>::transpose() const
{
  vnl_matrix_fixed<T,ncols,nrows> result;
  for (unsigned int i = 0; i < cols(); i++)
    for (unsigned int j = 0; j < rows(); j++)
      result(i,j) = (*this)(j,i);
  return result;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,ncols,nrows>
vnl_matrix_fixed_ref_const<T,nrows,ncols>::conjugate_transpose() const
{
  vnl_matrix_fixed<T,ncols,nrows> result(transpose());
  vnl_c_vector<T>::conjugate(result.begin(),  // src
                             result.begin(),  // dst
                             result.size());  // size of block
  return result;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> const&
vnl_matrix_fixed_ref<T,nrows,ncols>::update (vnl_matrix<T> const& m,
                                             unsigned top, unsigned left) const
{
  const unsigned int bottom = top + m.rows();
  const unsigned int right = left + m.cols();
#ifndef NDEBUG
  if (nrows < bottom || ncols < right)
    vnl_error_matrix_dimension ("update",
                                bottom, right, m.rows(), m.cols());
#endif
  for (unsigned int i = top; i < bottom; i++)
    for (unsigned int j = left; j < right; j++)
      (*this)(i,j) = m(i-top,j-left);
  return *this;
}


template<class T, unsigned nrows, unsigned ncols>
vnl_matrix<T>
vnl_matrix_fixed_ref_const<T,nrows,ncols>::extract (unsigned rowz, unsigned colz,
                                                    unsigned top, unsigned left) const
{
#ifndef NDEBUG
  unsigned int bottom = top + rowz;
  unsigned int right = left + colz;
  if ((nrows < bottom) || (ncols < right))
    vnl_error_matrix_dimension ("extract",
                                nrows, ncols, bottom, right);
#endif
  vnl_matrix<T> result(rowz, colz);
  for (unsigned int i = 0; i < rowz; i++)      // actual copy of all elements
    for (unsigned int j = 0; j < colz; j++)    // in submatrix
      result(i,j) = (*this)(top+i,left+j);
  return result;
}


template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> const&
vnl_matrix_fixed_ref<T,nrows,ncols>::copy_in(T const *p) const
{
  T* dp = this->data_block();
  unsigned int i = nrows * ncols;
  while (i--)
    *dp++ = *p++;
  return *this;
}

template<class T, unsigned nrows, unsigned ncols>
void vnl_matrix_fixed_ref_const<T,nrows,ncols>::copy_out(T *p) const
{
  T const* dp = this->data_block();
  unsigned int i = nrows*ncols;
  while (i--)
    *p++ = *dp++;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> const&
vnl_matrix_fixed_ref<T,nrows,ncols>::set_identity() const
{
  // Two simple loops are generally better than having a branch inside
  // the loop. Probably worth the O(n) extra writes.
  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
        (*this)(i,j) = T(0);
  for (unsigned int i = 0; i < nrows && i < ncols; ++i)
    (*this)(i,i) = T(1);
  return *this;
}

//: Make each row of the matrix have unit norm.
// All-zero rows are ignored.
template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> const&
vnl_matrix_fixed_ref<T,nrows,ncols>::normalize_rows() const
{
  typedef typename vnl_numeric_traits<T>::abs_t Abs_t;
  for (unsigned int i = 0; i < nrows; i++)
  {
    Abs_t norm(0); // double will not do for all types.
    for (unsigned int j = 0; j < ncols; j++)
      norm += vnl_math::squared_magnitude( (*this)(i,j) );

    if (norm != 0)
    {
      typedef typename vnl_numeric_traits<Abs_t>::real_t real_t;
      real_t scale = real_t(1)/std::sqrt((real_t)norm);
      for (unsigned int j = 0; j < ncols; j++)
      {
        // FIXME need correct rounding here
        // There is e.g. no *standard* operator*=(complex<float>, double), hence the T() cast.
        (*this)(i,j) *= (T)(scale);
      }
    }
  }
  return *this;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> const&
vnl_matrix_fixed_ref<T,nrows,ncols>::normalize_columns() const
{
  typedef typename vnl_numeric_traits<T>::abs_t Abs_t;
  for (unsigned int j = 0; j < ncols; j++) {  // For each column in the Matrix
    Abs_t norm(0); // double will not do for all types.
    for (unsigned int i = 0; i < nrows; i++)
      norm += vnl_math::squared_magnitude( (*this)(i,j) );

    if (norm != 0)
    {
      typedef typename vnl_numeric_traits<Abs_t>::real_t real_t;
      real_t scale = real_t(1)/std::sqrt((real_t)norm);
      for (unsigned int i = 0; i < nrows; i++)
      {
        // FIXME need correct rounding here
        // There is e.g. no *standard* operator*=(complex<float>, double), hence the T() cast.
        (*this)(i,j) *= (T)(scale);
      }
    }
  }
  return *this;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> const&
vnl_matrix_fixed_ref<T,nrows,ncols>::scale_row(unsigned row_index, T value) const
{
#ifndef NDEBUG
  if (row_index >= nrows)
    vnl_error_matrix_row_index("scale_row", row_index);
#endif
  for (unsigned int j = 0; j < ncols; j++)
    (*this)(row_index,j) *= value;
  return *this;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> const&
vnl_matrix_fixed_ref<T,nrows,ncols>::scale_column(unsigned column_index, T value) const
{
#ifndef NDEBUG
  if (column_index >= ncols)
    vnl_error_matrix_col_index("scale_column", column_index);
#endif
  for (unsigned int j = 0; j < nrows; j++)
    (*this)(j,column_index) *= value;
  return *this;
}

//: Returns a copy of n rows, starting from "row"
template<class T, unsigned nrows, unsigned ncols>
vnl_matrix<T>
vnl_matrix_fixed_ref_const<T,nrows,ncols>::get_n_rows (unsigned row, unsigned n) const
{
#ifndef NDEBUG
  if (row + n > nrows)
    vnl_error_matrix_row_index ("get_n_rows", row);
#endif

  // Extract data rowwise.
  return vnl_matrix<T>((*this)[row], n, ncols);
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix<T>
vnl_matrix_fixed_ref_const<T,nrows,ncols>::get_n_columns (unsigned column, unsigned n) const
{
#ifndef NDEBUG
  if (column + n > ncols)
    vnl_error_matrix_col_index ("get_n_columns", column);
#endif

  vnl_matrix<T> result(nrows, n);
  for (unsigned int c = 0; c < n; ++c)
    for (unsigned int r = 0; r < nrows; ++r)
      result(r, c) = (*this)(r,column + c);
  return result;
}

//: Return a vector with the content of the (main) diagonal
template<class T, unsigned nrows, unsigned ncols>
vnl_vector<T> vnl_matrix_fixed_ref_const<T,nrows,ncols>::get_diagonal() const
{
  vnl_vector<T> v(nrows < ncols ? nrows : ncols);
  for (unsigned int j = 0; j < nrows && j < ncols; ++j)
    v[j] = (*this)(j,j);
  return v;
}

//--------------------------------------------------------------------------------

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> const&
vnl_matrix_fixed_ref<T,nrows,ncols>::set_row(unsigned row_index, T const *v) const
{
  for (unsigned int j = 0; j < ncols; j++)
    (*this)(row_index,j) = v[j];
  return *this;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> const&
vnl_matrix_fixed_ref<T,nrows,ncols>::set_row(unsigned row_index, vnl_vector_fixed<T,ncols> const &v) const
{
  set_row(row_index,v.data_block());
  return *this;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> const&
vnl_matrix_fixed_ref<T,nrows,ncols>::set_row(unsigned row_index, vnl_vector<T> const &v) const
{
  set_row(row_index,v.data_block());
  return *this;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> const&
vnl_matrix_fixed_ref<T,nrows,ncols>::set_row(unsigned row_index, T v) const
{
  for (unsigned int j = 0; j < ncols; j++)
    (*this)(row_index,j) = v;
  return *this;
}

//--------------------------------------------------------------------------------

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> const&
vnl_matrix_fixed_ref<T,nrows,ncols>::set_column(unsigned column_index, T const *v) const
{
  for (unsigned int i = 0; i < nrows; i++)
    (*this)(i,column_index) = v[i];
  return *this;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> const&
vnl_matrix_fixed_ref<T,nrows,ncols>::set_column(unsigned column_index, vnl_vector_fixed<T,nrows> const &v) const
{
  set_column(column_index,v.data_block());
  return *this;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> const&
vnl_matrix_fixed_ref<T,nrows,ncols>::set_column(unsigned column_index, vnl_vector<T> const &v) const
{
  set_column(column_index,v.data_block());
  return *this;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> const&
vnl_matrix_fixed_ref<T,nrows,ncols>::set_column(unsigned column_index, T v) const
{
  for (unsigned int j = 0; j < nrows; j++)
    (*this)(j,column_index) = v;
  return *this;
}


template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> const&
vnl_matrix_fixed_ref<T,nrows,ncols>::set_columns(unsigned starting_column, vnl_matrix<T> const& m) const
{
#ifndef NDEBUG
  if (nrows != m.rows() ||
      ncols < m.cols() + starting_column)           // Size match?
    vnl_error_matrix_dimension ("set_columns",
                                nrows, ncols,
                                m.rows(), m.cols());
#endif

  for (unsigned int j = 0; j < m.cols(); ++j)
    for (unsigned int i = 0; i < nrows; i++)
      (*this)(i,starting_column + j) = m(i,j);
  return *this;
}


template <class T, unsigned nrows, unsigned ncols>
bool
vnl_matrix_fixed_ref_const<T,nrows,ncols>::is_identity() const
{
  T const zero(0);
  T const one(1);
  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
    {
      T xm = (*this)(i,j);
      if ( !((i == j) ? (xm == one) : (xm == zero)) )
        return false;
    }
  return true;
}

//: Return true if maximum absolute deviation of M from identity is <= tol.
template <class T, unsigned nrows, unsigned ncols>
bool
vnl_matrix_fixed_ref_const<T,nrows,ncols>::is_identity(double tol) const
{
  T one(1);
  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
    {
      T xm = (*this)(i,j);
      abs_t absdev = (i == j) ? vnl_math::abs(xm - one) : vnl_math::abs(xm);
      if (absdev > tol)
        return false;
    }
  return true;
}

template <class T, unsigned nrows, unsigned ncols>
bool
vnl_matrix_fixed_ref_const<T,nrows,ncols>::is_zero() const
{
  T const zero(0);
  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
      if ( !( (*this)(i, j) == zero) )
        return false;

  return true;
}

template <class T, unsigned nrows, unsigned ncols>
bool
vnl_matrix_fixed_ref_const<T,nrows,ncols>::is_zero(double tol) const
{
  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
      if (vnl_math::abs((*this)(i,j)) > tol)
        return false;

  return true;
}

template <class T, unsigned nrows, unsigned ncols>
bool
vnl_matrix_fixed_ref_const<T,nrows,ncols>::has_nans() const
{
  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
      if (vnl_math::isnan((*this)(i,j)))
        return true;

  return false;
}

template <class T, unsigned nrows, unsigned ncols>
bool
vnl_matrix_fixed_ref_const<T,nrows,ncols>::is_finite() const
{
  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
      if (!vnl_math::isfinite((*this)(i,j)))
        return false;

  return true;
}

//: Abort if any element of M is inf or nan
template <class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed_ref_const<T,nrows,ncols>::assert_finite_internal() const
{
  if (is_finite())
    return;

  std::cerr << "\n\n" << __FILE__ " : " << __LINE__ << ": matrix has non-finite elements\n";

  if (rows() <= 20 && cols() <= 20)
    std::cerr << __FILE__ ": here it is:\n" << *this << '\n';
  else
  {
    std::cerr << __FILE__ ": it is quite big (" << rows() << 'x' << cols() << ")\n"
             << __FILE__ ": in the following picture '-' means finite and '*' means non-finite:\n";

    for (unsigned int i=0; i<rows(); ++i)
    {
      for (unsigned int j=0; j<cols(); ++j)
        std::cerr << char(vnl_math::isfinite((*this)(i, j)) ? '-' : '*');
      std::cerr << '\n';
    }
  }
  std::cerr << __FILE__ ": calling abort()\n";
  std::abort();
}

//: Abort unless M has the given size.
template <class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed_ref_const<T,nrows,ncols>::assert_size_internal(unsigned rs,unsigned cs) const
{
  if (nrows!=rs || ncols!=cs)
  {
    std::cerr << __FILE__ ": size is " << nrows << 'x' << ncols
             << ". should be " << rs << 'x' << cs << std::endl;
    std::abort();
  }
}

template <class T, unsigned nrows, unsigned ncols>
bool
vnl_matrix_fixed_ref<T,nrows,ncols>::read_ascii(std::istream& s) const
{
  if (!s.good())
  {
    std::cerr << __FILE__ ": vnl_matrix_fixed_ref_const<T,nrows,ncols>::read_ascii: Called with bad stream\n";
    return false;
  }

  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
      s >> (*this)(i,j);

  return s.good() || s.eof();
}


template <class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> &
vnl_matrix_fixed_ref<T,nrows,ncols>::flipud() 
{
  for (unsigned int r1 = 0; 2*r1+1 < nrows; ++r1)
  {
    const unsigned int r2 = nrows - 1 - r1;
    for (unsigned int c = 0; c < ncols; ++c)
    {
      const T tmp = (*this)(r1, c);
      (*this)(r1, c) = (*this)(r2, c);
      (*this)(r2, c) = tmp;
    }
  }
  return *this;
}


template <class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> &
vnl_matrix_fixed_ref<T,nrows,ncols>::fliplr()
{
  for (unsigned int c1 = 0; 2*c1+1 < ncols; ++c1)
  {
    const unsigned int c2 = ncols - 1 - c1;
    for (unsigned int r = 0; r < nrows; ++r)
    {
      const T tmp = (*this)(r, c1);
      (*this)(r, c1) = (*this)(r, c2);
      (*this)(r, c2) = tmp;
    }
  }
  return *this;
}

template <class T, unsigned nrows, unsigned ncols>
typename vnl_matrix_fixed_ref_const<T,nrows,ncols>::abs_t
vnl_matrix_fixed_ref_const<T,nrows,ncols>::operator_one_norm() const
{
  abs_t m(0);
  for (unsigned int j=0; j<ncols; ++j)
  {
    abs_t t(0);
    for (unsigned int i=0; i<nrows; ++i)
      t += vnl_math::abs( (*this)(i,j) );
    if (t > m)
      m = t;
  }
  return m;
}

template <class T, unsigned nrows, unsigned ncols>
typename vnl_matrix_fixed_ref_const<T,nrows,ncols>::abs_t
vnl_matrix_fixed_ref_const<T,nrows,ncols>::operator_inf_norm() const
{
  abs_t m(0);
  for (unsigned int i=0; i<nrows; ++i)
  {
    abs_t t(0);
    for (unsigned int j=0; j<ncols; ++j)
      t += vnl_math::abs( (*this)(i,j) );
    if (t > m)
      m = t;
  }
  return m;
}

//: Transpose square matrix M in place.
template <class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed_ref<T,nrows,ncols> const&
vnl_matrix_fixed_ref<T,nrows,ncols>::inplace_transpose() const
{
  assert(nrows==ncols); // cannot inplace_transpose non-square fixed size matrix
  for (unsigned i = 0; i < nrows; ++i)
  for (unsigned j = i+1; j < ncols; ++j)
  {
    T t = (*this)(i,j);
    (*this)(i,j) = (*this)(j,i);
    (*this)(j,i) = t;
  }
  return *this;
}


#define VNL_MATRIX_FIXED_REF_INSTANTIATE(T,m,n) \
template class VNL_EXPORT vnl_matrix_fixed_ref_const<T, m, n >; \
template class VNL_EXPORT vnl_matrix_fixed_ref<T, m, n >

#endif // vnl_matrix_fixed_ref_hxx_
