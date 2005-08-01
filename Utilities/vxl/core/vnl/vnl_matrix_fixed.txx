// This is core/vnl/vnl_matrix_fixed.txx
#ifndef vnl_matrix_fixed_txx_
#define vnl_matrix_fixed_txx_
//:
// \file
#include "vnl_matrix_fixed.h"

#include <vcl_cmath.h>
#include <vcl_iostream.h>
#include <vcl_cstdlib.h> // for abort
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
void
vnl_matrix_fixed<T,nrows,ncols>::fill (T value)
{
  for (unsigned int i = 0; i < nrows; i++)
    for (unsigned int j = 0; j < ncols; j++)
      this->data_[i][j] = value;
}


template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::fill_diagonal (T value)
{
  for (unsigned int i = 0; i < nrows && i < ncols; i++)
    this->data_[i][i] = value;
}


template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::print(vcl_ostream& os) const
{
  for (unsigned int i = 0; i < nrows; i++)
  {
    os << this->data_[i][0];
    for (unsigned int j = 1; j < ncols; j++)
      os << ' ' << this->data_[i][j];
    os << '\n';
  }
}


template <class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>
vnl_matrix_fixed<T,nrows,ncols>::apply(T (*f)(T const&)) const
{
  vnl_matrix_fixed<T,nrows,ncols> ret;
  vnl_c_vector<T>::apply(this->data_[0], rows()*cols(), f, ret.data_block());
  return ret;
}

template <class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,nrows,ncols>
vnl_matrix_fixed<T,nrows,ncols>::apply(T (*f)(T)) const
{
  vnl_matrix_fixed<T,nrows,ncols> ret;
  vnl_c_vector<T>::apply(this->data_[0], rows()*cols(), f, ret.data_block());
  return ret;
}

////--------------------------- Additions------------------------------------


template<class T, unsigned nrows, unsigned ncols>
vnl_matrix_fixed<T,ncols,nrows>
vnl_matrix_fixed<T,nrows,ncols>::transpose() const
{
  vnl_matrix_fixed<T,ncols,nrows> result;
  for (unsigned int i = 0; i < cols(); i++)
    for (unsigned int j = 0; j < rows(); j++)
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
  for (unsigned int i = top; i < bottom; i++)
    for (unsigned int j = left; j < right; j++)
      this->data_[i][j] = m(i-top,j-left);
  return *this;
}


template<class T, unsigned nrows, unsigned ncols>
vnl_matrix<T>
vnl_matrix_fixed<T,nrows,ncols>::extract (unsigned rowz, unsigned colz,
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
      result(i,j) = this->data_[top+i][left+j];
  return result;
}


template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::copy_in(T const *p)
{
  T* dp = this->data_block();
  unsigned int i = nrows * ncols;
  while (i--)
    *dp++ = *p++;
}

template<class T, unsigned nrows, unsigned ncols>
void vnl_matrix_fixed<T,nrows,ncols>::copy_out(T *p) const
{
  T const* dp = this->data_block();
  unsigned int i = nrows*ncols;
  while (i--)
    *p++ = *dp++;
}

template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::set_identity()
{
#ifndef NDEBUG
  if (nrows != ncols) // Size?
    vnl_error_matrix_nonsquare ("set_identity");
#endif
  // Two simple loops are generally better than having a branch inside
  // the loop. Probably worth the O(n) extra writes.
  for (unsigned int i = 0; i < nrows; i++)
    for (unsigned int j = 0; j < ncols; j++)
      this->data_[i][j] = T(0);
  for (unsigned int i = 0; i < nrows; i++)
    this->data_[i][i] = T(1);
}

//: Make each row of the matrix have unit norm.
// All-zero rows are ignored.
template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::normalize_rows()
{
  for (unsigned int i = 0; i < nrows; i++)
  {
    abs_t norm(0); // double will not do for all types.
    for (unsigned int j = 0; j < ncols; j++)
      norm += vnl_math_squared_magnitude( this->data_[i][j] );

    if (norm != 0)
    {
      typedef typename vnl_numeric_traits<abs_t>::real_t real_t;
      real_t scale = real_t(1)/vcl_sqrt((real_t)norm);
      for (unsigned int j = 0; j < ncols; j++)
      {
        // FIXME need correct rounding here
        // There is e.g. no *standard* operator*=(complex<float>, double), hence the T() cast.
        this->data_[i][j] *= T(scale);
      }
    }
  }
}

template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::normalize_columns()
{
  for (unsigned int j = 0; j < ncols; j++) {  // For each column in the Matrix
    abs_t norm(0); // double will not do for all types.
    for (unsigned int i = 0; i < nrows; i++)
      norm += vnl_math_squared_magnitude( this->data_[i][j] );

    if (norm != 0)
    {
      typedef typename vnl_numeric_traits<abs_t>::real_t real_t;
      real_t scale = real_t(1)/vcl_sqrt((real_t)norm);
      for (unsigned int i = 0; i < nrows; i++)
      {
        // FIXME need correct rounding here
        // There is e.g. no *standard* operator*=(complex<float>, double), hence the T() cast.
        this->data_[i][j] *= T(scale);
      }
    }
  }
}

template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::scale_row(unsigned row_index, T value)
{
#ifndef NDEBUG
  if (row_index >= nrows)
    vnl_error_matrix_row_index("scale_row", row_index);
#endif
  for (unsigned int j = 0; j < ncols; j++)
    this->data_[row_index][j] *= value;
}

template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::scale_column(unsigned column_index, T value)
{
#ifndef NDEBUG
  if (column_index >= ncols)
    vnl_error_matrix_col_index("scale_column", column_index);
#endif
  for (unsigned int j = 0; j < nrows; j++)
    this->data_[j][column_index] *= value;
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
vnl_vector<T> vnl_matrix_fixed<T,nrows,ncols>::get_row(unsigned row_index) const
{
#ifdef ERROR_CHECKING
  if (row_index >= nrows)
    vnl_error_matrix_row_index ("get_row", row_index);
#endif

  vnl_vector<T> v(ncols);
  for (unsigned int j = 0; j < ncols; j++)    // For each element in row
    v[j] = this->data_[row_index][j];
  return v;
}

//: Create a vector out of column[column_index].
template<class T, unsigned nrows, unsigned ncols>
vnl_vector<T> vnl_matrix_fixed<T,nrows,ncols>::get_column(unsigned column_index) const
{
#ifdef ERROR_CHECKING
  if (column_index >= ncols)
    vnl_error_matrix_col_index ("get_column", column_index);
#endif

  vnl_vector<T> v(nrows);
  for (unsigned int j = 0; j < nrows; j++)
    v[j] = this->data_[j][column_index];
  return v;
}

//--------------------------------------------------------------------------------

template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::set_row(unsigned row_index, T const *v)
{
  for (unsigned int j = 0; j < ncols; j++)
    this->data_[row_index][j] = v[j];
}

template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::set_row(unsigned row_index, vnl_vector<T> const &v)
{
  set_row(row_index,v.data_block());
}

template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::set_row(unsigned row_index, T v)
{
  for (unsigned int j = 0; j < ncols; j++)
    this->data_[row_index][j] = v;
}

//--------------------------------------------------------------------------------

template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::set_column(unsigned column_index, T const *v)
{
  for (unsigned int i = 0; i < nrows; i++)
    this->data_[i][column_index] = v[i];
}

template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::set_column(unsigned column_index, vnl_vector<T> const &v)
{
  set_column(column_index,v.data_block());
}

template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::set_column(unsigned column_index, T v)
{
  for (unsigned int j = 0; j < nrows; j++)
    this->data_[j][column_index] = v;
}


template<class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::set_columns(unsigned starting_column, vnl_matrix<T> const& m)
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
      this->data_[i][starting_column + j] = m(i,j);
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
      abs_t absdev = (i == j) ? vnl_math_abs(xm - one) : vnl_math_abs(xm);
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
bool
vnl_matrix_fixed<T,nrows,ncols>::is_zero(double tol) const
{
  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
      if (vnl_math_abs(this->data_[i][j]) > tol)
        return false;

  return true;
}

template <class T, unsigned nrows, unsigned ncols>
bool
vnl_matrix_fixed<T,nrows,ncols>::has_nans() const
{
  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
      if (vnl_math_isnan(this->data_[i][j]))
        return true;

  return false;
}

template <class T, unsigned nrows, unsigned ncols>
bool
vnl_matrix_fixed<T,nrows,ncols>::is_finite() const
{
  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
      if (!vnl_math_isfinite(this->data_[i][j]))
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

  vcl_cerr << "\n\n" __FILE__ ": " << __LINE__ << ": matrix has non-finite elements\n";

  if (rows() <= 20 && cols() <= 20)
    vcl_cerr << __FILE__ ": here it is:\n" << *this << '\n';
  else
  {
    vcl_cerr << __FILE__ ": it is quite big (" << rows() << 'x' << cols() << ")\n"
             << __FILE__ ": in the following picture '-' means finite and '*' means non-finite:\n";

    for (unsigned int i=0; i<rows(); ++i)
    {
      for (unsigned int j=0; j<cols(); ++j)
        vcl_cerr << char(vnl_math_isfinite(this->data_[i][ j]) ? '-' : '*');
      vcl_cerr << '\n';
    }
  }
  vcl_cerr << __FILE__ ": calling abort()\n";
  vcl_abort();
}

//: Abort unless M has the given size.
template <class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::assert_size_internal(unsigned rs,unsigned cs) const
{
  if (nrows!=rs || ncols!=cs)
  {
    vcl_cerr << __FILE__ ": size is " << nrows << 'x' << ncols
             << ". should be " << rs << 'x' << cs << vcl_endl;
    vcl_abort();
  }
}

template <class T, unsigned nrows, unsigned ncols>
bool
vnl_matrix_fixed<T,nrows,ncols>::read_ascii(vcl_istream& s)
{
  if (!s.good())
  {
    vcl_cerr << __FILE__ ": vnl_matrix_fixed<T,nrows,ncols>::read_ascii: Called with bad stream\n";
    return false;
  }

  for (unsigned int i = 0; i < nrows; ++i)
    for (unsigned int j = 0; j < ncols; ++j)
      s >> this->data_[i][j];

  return s.good() || s.eof();
}


template <class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::flipud()
{
  for (unsigned int r1 = 0; 2*r1+1 < nrows; ++r1)
  {
    unsigned int r2 = nrows - 1 - r1;
    for (unsigned int c = 0; c < ncols; ++c)
    {
      T tmp = this->data_[r1][c];
      this->data_[r1][c] = this->data_[r2][c];
      this->data_[r2][c] = tmp;
    }
  }
}


template <class T, unsigned nrows, unsigned ncols>
void
vnl_matrix_fixed<T,nrows,ncols>::fliplr()
{
  for (unsigned int c1 = 0; 2*c1+1 < ncols; ++c1)
  {
    unsigned int c2 = ncols - 1 - c1;
    for (unsigned int r = 0; r < nrows; ++r)
    {
      T tmp = this->data_[r][c1];
      this->data_[r][c1] = this->data_[r][c2];
      this->data_[r][c2] = tmp;
    }
  }
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
      t += vnl_math_abs( this->data_[i][j] );
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
      t += vnl_math_abs( this->data_[i][j] );
    if (t > m)
      m = t;
  }
  return m;
}

//: Transpose square matrix M in place.
template <class T, unsigned nrows, unsigned ncols>
void vnl_matrix_fixed<T,nrows,ncols>::inplace_transpose()
{
  assert(nrows==ncols); // cannot inplace_transpose non-square fixed size matrix
  for (unsigned i = 0; i < nrows; ++i)
  for (unsigned j = i+1; j < ncols; ++j)
  {
    T t = this->data_[i][j];
    this->data_[i][j] = this->data_[j][i];
    this->data_[j][i] = t;
  }
}

// Workaround for argument deduction bug in VC6. See comment in .h
// file.  Note that the body of the function is outside the #ifdefs to
// maintain a single implementation of the function. The way to read
// this code is to just jump to the #else part and scan down, ignoring
// the #endif. Unless, of course, you are masochist and actually want
// to read the workaround.
//
#ifdef VCL_VC60

template<class VecA, class VecB, class RM>
RM
outer_product_fixed_calc_helper<VecA,VecB,RM>::calc( VecA const& a, VecB const& b )
{
  RM out; // RM should be a vnl_matrix_fixed of VecA::SIZE by VecB::SIZE
  for (unsigned int i = 0; i < VecA::SIZE; i++)
    for (unsigned int j = 0; j < VecB::SIZE; j++)
      out[i][j] = a[i] * b[j];
  return out;
};

#define VNL_OUTER_PRODUCT_FIXED_INSTANTIATE( T, M, N ) \
 template struct outer_product_fixed_calc_helper< vnl_vector_fixed<T,M >, \
                                                  vnl_vector_fixed<T,N >, \
                                                  vnl_matrix_fixed<T,M,N > >

#else // no need for workaround; declare the function sanely.

template <class T, unsigned m, unsigned n>
vnl_matrix_fixed<T,m,n>
outer_product(vnl_vector_fixed<T,m> const& a, vnl_vector_fixed<T,n> const& b)
{
  vnl_matrix_fixed<T,m,n> out; // = a.column() * b.row()
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      out[i][j] = a[i] * b[j];
  return out;
}

#define VNL_OUTER_PRODUCT_FIXED_INSTANTIATE( T, M, N ) \
 template vnl_matrix_fixed<T,M,N > outer_product(vnl_vector_fixed<T,M > const&,\
                                                 vnl_vector_fixed<T,N > const& )

#endif // VC60 outer_product workaround


#undef VNL_MATRIX_FIXED_INSTANTIATE
#define VNL_MATRIX_FIXED_INSTANTIATE(T, M, N) \
  template class vnl_matrix_fixed<T,M,N >; \
  VNL_OUTER_PRODUCT_FIXED_INSTANTIATE( T, M, N )

#endif // vnl_matrix_fixed_txx_
