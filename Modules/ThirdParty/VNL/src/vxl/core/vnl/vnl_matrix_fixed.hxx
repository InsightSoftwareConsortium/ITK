// This is core/vnl/vnl_matrix_fixed.hxx
#ifndef vnl_matrix_fixed_hxx_
#define vnl_matrix_fixed_hxx_
//:
// \file
#include <cmath>
#include <iostream>
#include <cstdlib>
#include "vnl_matrix_fixed.h"

#include <vcl_compiler.h>
#include <vcl_cassert.h>

#include <vnl/vnl_error.h>
#include <vnl/vnl_math.h>
#include <vnl/vnl_vector_fixed.h>

template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::add( const T* a, const T* b, T* r )
{
  unsigned int count = nrows*ncols;
  while ( count-- )
    *(r++) = *(a++) + *(b++);
}


template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::add( const T* a, T b, T* r )
{
  unsigned int count = nrows*ncols;
  while ( count-- )
    *(r++) = *(a++) + b;
}

template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::sub( const T* a, const T* b, T* r )
{
  unsigned int count = nrows*ncols;
  while ( count-- )
    *(r++) = *(a++) - *(b++);
}

template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::sub( const T* a, T b, T* r )
{
  unsigned int count = nrows*ncols;
  while ( count-- )
    *(r++) = *(a++) - b;
}

template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::sub( T a, const T* b, T* r )
{
  unsigned int count = nrows*ncols;
  while ( count-- )
    *(r++) = a - *(b++);
}

template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::mul( const T* a, const T* b, T* r )
{
  unsigned int count = nrows*ncols;
  while ( count-- )
    *(r++) = *(a++) * *(b++);
}

template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::mul( const T* a, T b, T* r )
{
  unsigned int count = nrows*ncols;
  while ( count-- )
    *(r++) = *(a++) * b;
}

template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::div( const T* a, const T* b, T* r )
{
  unsigned int count = nrows*ncols;
  while ( count-- )
    *(r++) = *(a++) / *(b++);
}

template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::div( const T* a, T b, T* r )
{
  unsigned int count = nrows*ncols;
  while ( count-- )
    *(r++) = *(a++) / b;
}

template<class T, unsigned nrows, unsigned ncols>
bool
vnl_matrix_fixed<T,nrows,ncols>::equal( const T* a, const T* b )
{
  unsigned int count = nrows*ncols;
  while ( count-- )
    if ( *(a++) != *(b++) )  return false;
  return true;
}

//------------------------------------------------------------


template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::fill (T value)
{
  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
      this->data_[i][j] = value;
  return *this;
}


template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::fill_diagonal (T value)
{
  for (unsigned int i = 0; i < nrows && i < ncols; ++i)
    this->data_[i][i] = value;
  return *this;
}


template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::set_diagonal(vnl_vector<T> const& diag)
{
  assert(diag.size() >= nrows || diag.size() >= ncols);
  // The length of the diagonal of a non-square matrix is the minimum of
  // the matrix's width & height; that explains the "||" in the assert,
  // and the "&&" in the upper bound for the "for".
  for (unsigned int i = 0; i < nrows && i < ncols; ++i)
    this->data_[i][i] = diag[i];
  return *this;
}


template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::print(std::ostream& os) const
{
  for (unsigned int i = 0; i < nrows; ++i)
  {
    os << this->data_[i][0];
    for (unsigned int j = 1; j < ncols; ++j)
      os << ' ' << this->data_[i][j];
    os << '\n';
  }
}


template <class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>
vnl_matrix_fixed<T,nrows,ncols>
::apply(T (*f)(T const&)) const
{
  vnl_matrix_fixed<T,nrows,ncols> ret;
  vnl_c_vector<T>::apply(this->data_[0], rows()*cols(), f, ret.data_block());
  return ret;
}

template <class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>
vnl_matrix_fixed<T,nrows,ncols>
::apply(T (*f)(T)) const
{
  vnl_matrix_fixed<T,nrows,ncols> ret;
  vnl_c_vector<T>::apply(this->data_[0], rows()*cols(), f, ret.data_block());
  return ret;
}

//: Make a vector by applying a function across rows.
template <class T, unsigned nrows, unsigned ncols>
vnl_vector_fixed<T,nrows>
vnl_matrix_fixed<T,nrows,ncols>
::apply_rowwise(T (*f)(vnl_vector_fixed<T,ncols> const&)) const
{
  vnl_vector_fixed<T,nrows> v;
  for (unsigned int i = 0; i < nrows; ++i)
    v.put(i,f(this->get_row(i)));
  return v;
}

//: Make a vector by applying a function across columns.
template <class T, unsigned nrows, unsigned ncols>
vnl_vector_fixed<T,ncols>
vnl_matrix_fixed<T,nrows,ncols>
::apply_columnwise(T (*f)(vnl_vector_fixed<T,nrows> const&)) const
{
  vnl_vector_fixed<T,ncols> v;
  for (unsigned int i = 0; i < ncols; ++i)
    v.put(i,f(this->get_column(i)));
  return v;
}

////--------------------------- Additions------------------------------------


template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,ncols,nrows>
vnl_matrix_fixed<T,nrows,ncols>::transpose() const
{
  vnl_matrix_fixed<T,ncols,nrows> result;
  for (unsigned int i = 0; i < cols(); ++i)
    for (unsigned int j = 0; j < rows(); ++j)
      result(i,j) = this->data_[j][i];
  return result;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,ncols,nrows>
vnl_matrix_fixed<T,nrows,ncols>::conjugate_transpose() const
{
  vnl_matrix_fixed<T,ncols,nrows> result(transpose());
  vnl_c_vector<T>::conjugate(result.begin(),  // src
                             result.begin(),  // dst
                             result.size());  // size of block
  return result;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::update (vnl_matrix<T> const& m,
                                         unsigned top, unsigned left)
{
  const unsigned int bottom = top + m.rows();
  const unsigned int right = left + m.cols();
#ifndef NDEBUG
  if (nrows < bottom || ncols < right)
    vnl_error_matrix_dimension ("update",
                                bottom, right, m.rows(), m.cols());
#endif
  for (unsigned int i = top; i < bottom; ++i)
    for (unsigned int j = left; j < right; ++j)
      this->data_[i][j] = m(i-top,j-left);
  return *this;
}


template<class T, unsigned nrows, unsigned ncols>
vnl_matrix<T>
vnl_matrix_fixed<T,nrows,ncols>::extract (unsigned rowz, unsigned colz,
                                          unsigned top, unsigned left) const
{
  vnl_matrix<T> result(rowz, colz);
  this->extract( result, top, left );
  return result;
}


template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::extract (vnl_matrix<T>& sub_matrix,
                                          unsigned top, unsigned left) const
{
  unsigned int rowz = sub_matrix.rows();
  unsigned int colz = sub_matrix.cols();
#ifndef NDEBUG
  unsigned int bottom = top + rowz;
  unsigned int right = left + colz;
  if ((nrows < bottom) || (ncols < right))
    vnl_error_matrix_dimension ("extract",
                                nrows, ncols, bottom, right);
#endif
  for (unsigned int i = 0; i < rowz; ++i)      // actual copy of all elements
    for (unsigned int j = 0; j < colz; ++j)    // in submatrix
      sub_matrix(i,j) = this->data_[top+i][left+j];
}


template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::copy_in(T const *p)
{
  T* dp = this->data_block();
  std::copy( p, p + nrows * ncols, dp );
  return *this;
}

template<class T, unsigned nrows, unsigned ncols>
void vnl_matrix_fixed<T,nrows,ncols>::copy_out(T *p) const
{
  T const* dp = this->data_block();
  std::copy( dp, dp + nrows * ncols, p );
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::set_identity()
{
  // Two simple loops are generally better than having a branch inside
  // the loop. Probably worth the O(n) extra writes.
  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
      this->data_[i][j] = T(0);
  for (unsigned int i = 0; i < nrows && i < ncols; ++i)
    this->data_[i][i] = T(1);
  return *this;
}

//: Make each row of the matrix have unit norm.
// All-zero rows are ignored.
template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::normalize_rows()
{
  for (unsigned int i = 0; i < nrows; ++i)
  {
    abs_t norm(0); // double will not do for all types.
    for (unsigned int j = 0; j < ncols; ++j)
      norm += vnl_math::squared_magnitude( this->data_[i][j] );

    if (norm != 0)
    {
      typedef typename vnl_numeric_traits<abs_t>::real_t real_t;
      real_t scale = real_t(1)/std::sqrt((real_t)norm);
      for (unsigned int j = 0; j < ncols; ++j)
      {
        // FIXME need correct rounding here
        // There is e.g. no *standard* operator*=(complex<float>, double), hence the T() cast.
        this->data_[i][j] *= T(scale);
      }
    }
  }
  return *this;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::normalize_columns()
{
  for (unsigned int j = 0; j < ncols; ++j) {  // For each column in the Matrix
    abs_t norm(0); // double will not do for all types.
    for (unsigned int i = 0; i < nrows; ++i)
      norm += vnl_math::squared_magnitude( this->data_[i][j] );

    if (norm != 0)
    {
      typedef typename vnl_numeric_traits<abs_t>::real_t real_t;
      real_t scale = real_t(1)/std::sqrt((real_t)norm);
      for (unsigned int i = 0; i < nrows; ++i)
      {
        // FIXME need correct rounding here
        // There is e.g. no *standard* operator*=(complex<float>, double), hence the T() cast.
        this->data_[i][j] *= T(scale);
      }
    }
  }
  return *this;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::scale_row(unsigned row_index, T value)
{
#ifndef NDEBUG
  if (row_index >= nrows)
    vnl_error_matrix_row_index("scale_row", row_index);
#endif
  for (unsigned int j = 0; j < ncols; ++j)
    this->data_[row_index][j] *= value;
  return *this;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::scale_column(unsigned column_index, T value)
{
#ifndef NDEBUG
  if (column_index >= ncols)
    vnl_error_matrix_col_index("scale_column", column_index);
#endif
  for (unsigned int j = 0; j < nrows; ++j)
    this->data_[j][column_index] *= value;
  return *this;
}

template <class T, unsigned int nrows, unsigned int ncols>
void
vnl_matrix_fixed<T,nrows,ncols>
::swap(vnl_matrix_fixed<T,nrows,ncols> &that)
{
  for (unsigned int r = 0; r < nrows; ++r)
  {
    for (unsigned int c = 0; c < ncols; ++c)
    {
    std::swap(this->data_[r][c], that.data_[r][c]);
    }
  }
}

//: Returns a copy of n rows, starting from "row"
template<class T, unsigned nrows, unsigned ncols>
vnl_matrix<T>
vnl_matrix_fixed<T,nrows,ncols>::get_n_rows (unsigned row, unsigned n) const
{
#ifndef NDEBUG
  if (row + n > nrows)
    vnl_error_matrix_row_index ("get_n_rows", row);
#endif

  // Extract data rowwise.
  return vnl_matrix<T>(data_[row], n, ncols);
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix<T>
vnl_matrix_fixed<T,nrows,ncols>::get_n_columns (unsigned column, unsigned n) const
{
#ifndef NDEBUG
  if (column + n > ncols)
    vnl_error_matrix_col_index ("get_n_columns", column);
#endif

  vnl_matrix<T> result(nrows, n);
  for (unsigned int c = 0; c < n; ++c)
    for (unsigned int r = 0; r < nrows; ++r)
      result(r, c) = this->data_[r][column + c];
  return result;
}

//: Create a vector out of row[row_index].
template<class T, unsigned nrows, unsigned ncols>
vnl_vector_fixed<T,ncols> vnl_matrix_fixed<T,nrows,ncols>::get_row(unsigned row_index) const
{
#ifdef ERROR_CHECKING
  if (row_index >= nrows)
    vnl_error_matrix_row_index ("get_row", row_index);
#endif

  vnl_vector_fixed<T,ncols> v;
  for (unsigned int j = 0; j < ncols; ++j)    // For each element in row
    v[j] = this->data_[row_index][j];
  return v;
}

//: Create a vector out of column[column_index].
template<class T, unsigned nrows, unsigned ncols>
vnl_vector_fixed<T,nrows> vnl_matrix_fixed<T,nrows,ncols>::get_column(unsigned column_index) const
{
#ifdef ERROR_CHECKING
  if (column_index >= ncols)
    vnl_error_matrix_col_index ("get_column", column_index);
#endif

  vnl_vector_fixed<T,nrows> v;
  for (unsigned int j = 0; j < nrows; ++j)
    v[j] = this->data_[j][column_index];
  return v;
}

//: Create a vector out of row[row_index].
template <class T, unsigned int nrows, unsigned int ncols>
vnl_matrix<T>
vnl_matrix_fixed<T,nrows,ncols>
::get_rows(vnl_vector<unsigned int> i) const
{
  vnl_matrix<T> m(i.size(), this->cols());
  for (unsigned int j = 0; j < i.size(); ++j)
    m.set_row(j, this->get_row(i.get(j)));
  return m;
}

//: Create a vector out of column[column_index].
template <class T, unsigned int nrows, unsigned int ncols>
vnl_matrix<T>
vnl_matrix_fixed<T,nrows,ncols>
::get_columns(vnl_vector<unsigned int> i) const
{
  vnl_matrix<T> m(this->rows(), i.size());
  for (unsigned int j = 0; j < i.size(); ++j)
    m.set_column(j, this->get_column(i.get(j)));
  return m;
}

//: Return a vector with the content of the (main) diagonal
template<class T, unsigned nrows, unsigned ncols>
vnl_vector<T> vnl_matrix_fixed<T,nrows,ncols>::get_diagonal() const
{
  vnl_vector<T> v(nrows < ncols ? nrows : ncols);
  for (unsigned int j = 0; j < nrows && j < ncols; ++j)
    v[j] = this->data_[j][j];
  return v;
}

//: Flatten row-major (C-style)
template <class T, unsigned nrows, unsigned ncols>
vnl_vector_fixed<T,nrows*ncols> vnl_matrix_fixed<T,nrows,ncols>::flatten_row_major() const
{
  vnl_vector_fixed<T,nrows*ncols> v;
  v.copy_in(this->data_block());
  return v;
}

//: Flatten column-major (Fortran-style)
template <class T, unsigned nrows, unsigned ncols>
vnl_vector_fixed<T,nrows*ncols> vnl_matrix_fixed<T,nrows,ncols>::flatten_column_major() const
{
  vnl_vector_fixed<T,nrows*ncols> v;
  for (unsigned int c = 0; c < ncols; ++c)
    for (unsigned int r = 0; r < nrows; ++r)
      v[c*nrows+r] = this->data_[r][c];
  return v;
}

//--------------------------------------------------------------------------------

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::set_row(unsigned row_index, T const *v)
{
  for (unsigned int j = 0; j < ncols; ++j)
    this->data_[row_index][j] = v[j];
  return *this;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::set_row(unsigned row_index, vnl_vector<T> const &v)
{
  if (v.size() >= ncols)
    set_row(row_index,v.data_block());
  else
    for (unsigned int j = 0; j < v.size(); ++j)
      this->data_[row_index][j] = v[j];
  return *this;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::set_row(unsigned row_index, vnl_vector_fixed<T,ncols> const &v)
{
  set_row(row_index,v.data_block());
  return *this;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::set_row(unsigned row_index, T v)
{
  for (unsigned int j = 0; j < ncols; ++j)
    this->data_[row_index][j] = v;
  return *this;
}

//--------------------------------------------------------------------------------

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::set_column(unsigned column_index, T const *v)
{
  for (unsigned int i = 0; i < nrows; ++i)
    this->data_[i][column_index] = v[i];
  return *this;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::set_column(unsigned column_index, vnl_vector<T> const &v)
{
  if (v.size() >= nrows)
    set_column(column_index,v.data_block());
  else
    for (unsigned int i = 0; i < v.size(); ++i)
      this->data_[i][column_index] = v[i];
  return *this;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::set_column(unsigned column_index, vnl_vector_fixed<T,nrows> const &v)
{
  set_column(column_index,v.data_block());
  return *this;
}

template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::set_column(unsigned column_index, T v)
{
  for (unsigned int j = 0; j < nrows; ++j)
    this->data_[j][column_index] = v;
  return *this;
}


template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::set_columns(unsigned starting_column, vnl_matrix<T> const& m)
{
  for (unsigned int j = 0; j < m.cols() && starting_column+j < ncols; ++j) // don't go too far right; possibly only use part of m
    for (unsigned int i = 0; i < nrows && i < m.rows(); ++i) // smallest of the two heights; possibly only use part of m
      this->data_[i][starting_column + j] = m(i,j);
  return *this;
}


template <class T, unsigned nrows, unsigned ncols>
bool
vnl_matrix_fixed<T,nrows,ncols>::is_identity() const
{
  T const zero(0);
  T const one(1);
  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
    {
      T xm = this->data_[i][j];
      if ( !((i == j) ? (xm == one) : (xm == zero)) )
        return false;
    }
  return true;
}

//: Return true if maximum absolute deviation of M from identity is <= tol.
template <class T, unsigned nrows, unsigned ncols>
bool
vnl_matrix_fixed<T,nrows,ncols>::is_identity(double tol) const
{
  T one(1);
  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
    {
      T xm = this->data_[i][j];
      abs_t absdev = (i == j) ? vnl_math::abs(xm - one) : vnl_math::abs(xm);
      if (absdev > tol)
        return false;
    }
  return true;
}

template <class T, unsigned nrows, unsigned ncols>
bool
vnl_matrix_fixed<T,nrows,ncols>::is_zero() const
{
  T const zero(0);
  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
      if ( !( this->data_[i][ j] == zero) )
        return false;

  return true;
}

template <class T, unsigned nrows, unsigned ncols>
bool vnl_matrix_fixed<T,nrows,ncols>
::is_equal(vnl_matrix_fixed<T,nrows,ncols> const& rhs, double tol) const
{
  if (this == &rhs)                                      // same object => equal.
    return true;

  if (this->rows() != rhs.rows() || this->cols() != rhs.cols())
    return false;                                        // different sizes => not equal.

  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
      if (vnl_math::abs(this->data_[i][j] - rhs.data_[i][j]) > tol)
        return false;                                    // difference greater than tol

  return true;
}

template <class T, unsigned nrows, unsigned ncols>
bool
vnl_matrix_fixed<T,nrows,ncols>::is_zero(double tol) const
{
  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
      if (vnl_math::abs(this->data_[i][j]) > tol)
        return false;

  return true;
}

template <class T, unsigned nrows, unsigned ncols>
bool
vnl_matrix_fixed<T,nrows,ncols>::has_nans() const
{
  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
      if (vnl_math::isnan(this->data_[i][j]))
        return true;

  return false;
}

template <class T, unsigned nrows, unsigned ncols>
bool
vnl_matrix_fixed<T,nrows,ncols>::is_finite() const
{
  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
      if (!vnl_math::isfinite(this->data_[i][j]))
        return false;

  return true;
}

//: Abort if any element of M is inf or nan
template <class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::assert_finite_internal() const
{
  if (is_finite())
    return;

  std::cerr << "\n\n" __FILE__ ": " << __LINE__ << ": matrix has non-finite elements\n";

  if (rows() <= 20 && cols() <= 20)
    std::cerr << __FILE__ ": here it is:\n" << *this << '\n';
  else
  {
    std::cerr << __FILE__ ": it is quite big (" << rows() << 'x' << cols() << ")\n"
             << __FILE__ ": in the following picture '-' means finite and '*' means non-finite:\n";

    for (unsigned int i=0; i<rows(); ++i)
    {
      for (unsigned int j=0; j<cols(); ++j)
        std::cerr << char(vnl_math::isfinite(this->data_[i][ j]) ? '-' : '*');
      std::cerr << '\n';
    }
  }
  std::cerr << __FILE__ ": calling abort()\n";
  std::abort();
}

//: Abort unless M has the given size.
template <class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::assert_size_internal(unsigned rs,unsigned cs) const
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
vnl_matrix_fixed<T,nrows,ncols>::read_ascii(std::istream& s)
{
  if (!s.good())
  {
    std::cerr << __FILE__ ": vnl_matrix_fixed<T,nrows,ncols>::read_ascii: Called with bad stream\n";
    return false;
  }

  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
      s >> this->data_[i][j];

  return s.good() || s.eof();
}


template <class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::flipud()
{
  for (unsigned int r1 = 0; 2*r1+1 < nrows; ++r1)
  {
    const unsigned int r2 = nrows - 1 - r1;
    for (unsigned int c = 0; c < ncols; ++c)
    {
    std::swap(this->data_[r1][c], this->data_[r2][c]);
    }
  }
  return *this;
}


template <class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::fliplr()
{
  for (unsigned int c1 = 0; 2*c1+1 < ncols; ++c1)
  {
    const unsigned int c2 = ncols - 1 - c1;
    for (unsigned int r = 0; r < nrows; ++r)
    {
    std::swap(this->data_[r][c1], this->data_[r][c2]);
    }
  }
  return *this;
}

template <class T, unsigned nrows, unsigned ncols>
typename vnl_matrix_fixed<T,nrows,ncols>::abs_t
vnl_matrix_fixed<T,nrows,ncols>::operator_one_norm() const
{
  abs_t m(0);
  for (unsigned int j=0; j<ncols; ++j)
  {
    abs_t t(0);
    for (unsigned int i=0; i<nrows; ++i)
      t += vnl_math::abs( this->data_[i][j] );
    if (t > m)
      m = t;
  }
  return m;
}

template <class T, unsigned nrows, unsigned ncols>
typename vnl_matrix_fixed<T,nrows,ncols>::abs_t
vnl_matrix_fixed<T,nrows,ncols>::operator_inf_norm() const
{
  abs_t m(0);
  for (unsigned int i=0; i<nrows; ++i)
  {
    abs_t t(0);
    for (unsigned int j=0; j<ncols; ++j)
      t += vnl_math::abs( this->data_[i][j] );
    if (t > m)
      m = t;
  }
  return m;
}

//: Transpose square matrix M in place.
template <class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>&
vnl_matrix_fixed<T,nrows,ncols>::inplace_transpose()
{
  assert(nrows==ncols); // cannot inplace_transpose non-square fixed size matrix
  for (unsigned i = 0; i < nrows; ++i)
  for (unsigned j = i+1; j < ncols; ++j)
  {
    T t = this->data_[i][j];
    this->data_[i][j] = this->data_[j][i];
    this->data_[j][i] = t;
  }
  return *this;
}

template <class T, unsigned m, unsigned n>
vnl_matrix_fixed<T,m,n>
outer_product(vnl_vector_fixed<T,m> const& a, vnl_vector_fixed<T,n> const& b)
{
  vnl_matrix_fixed<T,m,n> out; // = a.column() * b.row()
  for (unsigned int i = 0; i < m; ++i)
    for (unsigned int j = 0; j < n; ++j)
      out[i][j] = a[i] * b[j];
  return out;
}

#define VNL_OUTER_PRODUCT_FIXED_INSTANTIATE( T, M, N ) \
template VNL_EXPORT vnl_matrix_fixed<T,M,N > outer_product(vnl_vector_fixed<T,M > const&,\
                                                vnl_vector_fixed<T,N > const& )

#undef VNL_MATRIX_FIXED_INSTANTIATE
#define VNL_MATRIX_FIXED_INSTANTIATE(T, M, N) \
template class VNL_EXPORT vnl_matrix_fixed<T,M,N >; \
VNL_OUTER_PRODUCT_FIXED_INSTANTIATE( T, M, N )

#endif // vnl_matrix_fixed_hxx_
