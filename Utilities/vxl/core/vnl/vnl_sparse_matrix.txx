// This is core/vnl/vnl_sparse_matrix.txx
#ifndef vnl_sparse_matrix_txx_
#define vnl_sparse_matrix_txx_
//:
// \file

#include "vnl_sparse_matrix.h"
#include <vcl_cassert.h>
#include <vcl_algorithm.h>
#include <vcl_iostream.h>

// #define DEBUG_SPARSE 1

#ifdef DEBUG_SPARSE
# include <vnl/vnl_matrix.h>
#endif

// Implementation of vnl_sparse_matrix
//------------------------------------------------------------

//: Construct an empty matrix
template <class T>
vnl_sparse_matrix<T>::vnl_sparse_matrix()
  : rs_(0), cs_(0)
{
}

//------------------------------------------------------------
//: Construct an empty m*n matrix.  There are m rows and n columns.
template <class T>
vnl_sparse_matrix<T>::vnl_sparse_matrix(unsigned int m, unsigned int n)
  : elements(m), rs_(m), cs_(n)
{
}

//------------------------------------------------------------
//: Construct an m*n Matrix and copy rhs into it.
template <class T>
vnl_sparse_matrix<T>::vnl_sparse_matrix(const vnl_sparse_matrix<T>& rhs)
  : elements(rhs.elements), rs_(rhs.rs_), cs_(rhs.cs_)
{
}

//------------------------------------------------------------
//: Copy another vnl_sparse_matrix<T> into this.
template <class T>
vnl_sparse_matrix<T>& vnl_sparse_matrix<T>::operator=(const vnl_sparse_matrix<T>& rhs)
{
  if (this == &rhs)
    return *this;

  elements = rhs.elements;
  rs_ = rhs.rs_;
  cs_ = rhs.cs_;

  return *this;
}

//------------------------------------------------------------
//: Multiply this*rhs, another sparse matrix.
template <class T>
void vnl_sparse_matrix<T>::mult(vnl_sparse_matrix<T> const& rhs, vnl_sparse_matrix<T>& result) const
{
  assert(rhs.rows() == columns());
  unsigned int result_rows = rows();
  unsigned int result_cols = rhs.columns();

  // Clear result matrix.
  result.elements.clear();

  // Now give the result matrix enough rows.
  result.elements.resize(result_rows);
  result.rs_ = result_rows;
  result.cs_ = result_cols;

  // Now, iterate over non-zero rows of this.
  for (unsigned row_id=0; row_id<elements.size(); ++row_id) {
    // Get the row from this matrix (lhs).
    row const& this_row = elements[row_id];

    // Skip to next row if empty.
    if (this_row.empty())
      continue;

    // Get the new row in the result matrix.
    row& result_row = result.elements[row_id];

    // Iterate over the row.
    for (typename row::const_iterator col_iter = this_row.begin();
         col_iter != this_row.end();
         ++col_iter)
    {
      // Get the element from the row.
      vnl_sparse_matrix_pair<T> const & entry = *col_iter;
      unsigned const col_id = entry.first;

      // So we are at (row_id,col_id) = this_val in lhs matrix (this).
      // This must be multiplied by each entry in row col_id in
      // the rhs matrix, and the result added to result_row[col_id].

      // If that row in rhs is empty, there is nothing to do.
      row const & rhs_row = rhs.elements[col_id];
      if (rhs_row.empty())
        continue;

      // Else iterate over rhs's row.
      typename row::iterator result_col_iter = result_row.begin();
      for (typename row::const_iterator rhs_col_iter = rhs_row.begin();
           rhs_col_iter != rhs_row.end();
           ++rhs_col_iter)
      {
        const vnl_sparse_matrix_pair<T>& rhs_entry = *rhs_col_iter;
        unsigned int const dest_col = rhs_entry.first;

        // Calculate the product.
        T prod = entry.second * rhs_entry.second;

        // This must be added into result_row, at column dest_col.
        while ((result_col_iter != result_row.end()) &&
               ((*result_col_iter).first < dest_col))
          ++result_col_iter;

        if ((result_col_iter == result_row.end()) ||
            ((*result_col_iter).first != dest_col))
        {
          // Add new column to the row.
          result_col_iter = result_row.insert(result_col_iter, vnl_sparse_matrix_pair<T>(dest_col,prod));
        }
        else
        {
          // Else add product to existing contents.
          (*result_col_iter).second += prod;
        }
      }
    }
  }
}

//------------------------------------------------------------
//: Multiply this*p, a fortran order matrix.
//  The matrix p has n rows and m columns, and is in fortran order, ie. columns first.
template <class T>
void vnl_sparse_matrix<T>::mult(unsigned int prows, unsigned int pcols,
                                T const* p, T* q) const
{
  assert(prows == columns());

  // Clear q matrix.
  int size = prows*pcols;
  for (int temp=0; temp<size; temp++)
    q[temp] = T(0);

#ifdef DEBUG_SPARSE
  vnl_matrix<double> md(rows(),columns());
  for (int rr = 0; rr<rows(); rr++)
    for (int cc = 0; cc<columns(); cc++)
      md(rr,cc) = (*this)(rr,cc);

  vnl_matrix<double> pd(prows,pcols);
  for (int rr = 0; rr<prows; rr++)
    for (int cc = 0; cc<pcols; cc++)
      pd(rr,cc) = p[rr + cc*prows];

  vcl_cout << "Initial p:\n";
  for (int rr = 0; rr<prows; rr++) {
    for (int cc = 0; cc<pcols; cc++) {
      T pval = p[rr + cc*prows];
      vcl_cout << pval << ' ';
    }
    vcl_cout << '\n';
  }
#endif

  // Now, iterate over non-zero rows of this.
  for (unsigned row_id=0; row_id<elements.size(); ++row_id) {
    // Get the row from this matrix (lhs).
    row const & this_row = elements[row_id];

    // Skip to next row if empty.
    if (this_row.empty())
      continue;

    // Iterate over the row.
    for (typename row::const_iterator col_iter = this_row.begin();
         col_iter != this_row.end();
         ++col_iter)
    {
      // Get the element from the row.
      vnl_sparse_matrix_pair<T> const & entry = *col_iter;
      unsigned const col_id = entry.first;

      // So we are at (row_id,col_id) = this_val in lhs matrix
      // (this).  This must be multiplied by each entry in row
      // col_id in the p matrix, and the result added to
      // (row_id,p_col_id) in the q matrix.
      //

      // Iterate over p's row.
      for (unsigned int p_col_id = 0; p_col_id < pcols; p_col_id++) {
        // Get the correct position from p.
        T pval = p[col_id + p_col_id*prows];

        // Calculate the product.
        T prod = entry.second * pval;

        // Add the product into the correct position in q.
        q[row_id + p_col_id*prows] += prod;
      }
    }
  }

#ifdef DEBUG_SPARSE
  vcl_cout << "Final q:\n";
  for (int rr = 0; rr<prows; rr++) {
    for (int cc = 0; cc<pcols; cc++) {
      T pval = q[rr + cc*prows];
      vcl_cout << pval << ' ';
    }
    vcl_cout << '\n';
  }
  vcl_cout << "nonsparse: " << md*pd << '\n';
#endif
}


//------------------------------------------------------------
//: Multiply this*rhs, a vector.
template <class T>
void vnl_sparse_matrix<T>::mult(vnl_vector<T> const& rhs, vnl_vector<T>& result) const
{
  assert(rhs.size() == columns());

  result.set_size( rows() );
  result.fill(T(0));

  int rhs_row_id =0;
  typename vcl_vector<row>::const_iterator lhs_row_iter = elements.begin();
  for ( ; lhs_row_iter != elements.end(); ++lhs_row_iter, rhs_row_id++ ) {
    row const & lhs_row = *lhs_row_iter;
    if (lhs_row.empty()) continue;

    typename row::const_iterator lhs_col_iter = lhs_row.begin();
    for ( ; lhs_col_iter != lhs_row.end(); ++lhs_col_iter) {
      vnl_sparse_matrix_pair<T> const & entry = *lhs_col_iter;
      unsigned const lhs_col_id = entry.first;

      result[ rhs_row_id ] += rhs[ lhs_col_id ] * entry.second;
    }
  }
}

//------------------------------------------------------------
//: Multiply lhs*this, where lhs is a vector
template <class T>
void vnl_sparse_matrix<T>::pre_mult(const vnl_vector<T>& lhs, vnl_vector<T>& result) const
{
  assert(lhs.size() == rows());

  // Resize and clear result vector
  result.set_size( columns() );
  result.fill(T(0));

  // Now, iterate over lhs values and rows of rhs
  unsigned lhs_col_id = 0;
  for (typename vcl_vector<row>::const_iterator rhs_row_iter = elements.begin();
        rhs_row_iter != elements.end();
        ++rhs_row_iter, lhs_col_id++ )
    {
      // Get the row from rhs matrix.
      row const & rhs_row = *rhs_row_iter;

      // Skip to next row if empty.
      if (rhs_row.empty()) continue;

      // Iterate over values in rhs row
      for (typename row::const_iterator rhs_col_iter = rhs_row.begin();
           rhs_col_iter != rhs_row.end();
           ++rhs_col_iter)
        {
          // Get the element from the row.
          vnl_sparse_matrix_pair<T> const& entry = *rhs_col_iter;
          unsigned const rhs_col_id = entry.first;

          result[ rhs_col_id ] += lhs[ lhs_col_id ] * entry.second;
        }
    }
}

//------------------------------------------------------------
//: Add rhs to this.
template <class T>
void vnl_sparse_matrix<T>::add(const vnl_sparse_matrix<T>& rhs,
                               vnl_sparse_matrix<T>& result) const
{
  assert((rhs.rows() == rows()) && (rhs.columns() == columns()));

  // Clear result matrix.
  result.elements.clear();

  // Now give the result matrix enough rows.
  result.elements.resize(rows());
  result.rs_ = rows();
  result.cs_ = columns();

  // Now, iterate over non-zero rows of this.
  unsigned int row_id = 0;
  for (typename vcl_vector<row>::const_iterator row_iter = elements.begin();
       row_iter != elements.end();
       ++row_iter, ++row_id)
    {
      // Get the row from this matrix (lhs).
      row const & this_row = *row_iter;

      // Get the new row in the result matrix.
      row& result_row = result.elements[row_id];

      // Store this into result row.
      result_row = this_row;

      // If rhs row is empty, we are done.
      if (rhs.empty_row(row_id))
        continue;

      // Get the rhs row.
      row const& rhs_row = rhs.elements[row_id];

      // Iterate over the rhs row.
      for (typename row::const_iterator col_iter = rhs_row.begin();
           col_iter != rhs_row.end();
           ++col_iter)
        {
          // Get the element from the row.
          vnl_sparse_matrix_pair<T> const& entry = *col_iter;
          unsigned const col_id = entry.first;

          // So we are at (row_id,col_id) in rhs matrix.
          result(row_id,col_id) += entry.second;
        }
    }
}

//------------------------------------------------------------
//: Subtract rhs from this.
template <class T>
void vnl_sparse_matrix<T>::subtract(const vnl_sparse_matrix<T>& rhs,
                                    vnl_sparse_matrix<T>& result) const
{
  assert((rhs.rows() == rows()) && (rhs.columns() == columns()));

  // Clear result matrix.
  result.elements.clear();

  // Now give the result matrix enough rows.
  result.elements.resize(rows());
  result.rs_ = rows();
  result.cs_ = columns();

  // Now, iterate over non-zero rows of this.
  unsigned int row_id = 0;
  for (typename vcl_vector<row>::const_iterator row_iter = elements.begin();
       row_iter != elements.end();
       ++row_iter, ++row_id)
    {
      // Get the row from this matrix (lhs).
      row const& this_row = *row_iter;

      // Get the new row in the result matrix.
      row& result_row = result.elements[row_id];

      // Store this into result row.
      result_row = this_row;

      // If rhs row is empty, we are done.
      if (rhs.empty_row(row_id))
        continue;

      // Get the rhs row.
      row const& rhs_row = rhs.elements[row_id];

      // Iterate over the rhs row.
      for (typename row::const_iterator col_iter = rhs_row.begin();
           col_iter != rhs_row.end();
           ++col_iter)
        {
          // Get the element from the row.
          vnl_sparse_matrix_pair<T> const& entry = *col_iter;
          unsigned const col_id = entry.first;

          // So we are at (row_id,col_id) in rhs matrix.
          result(row_id,col_id) -= entry.second;
        }
    }
}

//------------------------------------------------------------
//: Get a reference to an entry in the matrix.
template <class T>
T& vnl_sparse_matrix<T>::operator()(unsigned int r, unsigned int c)
{
  assert((r < rows()) && (c < columns()));
  row& rw = elements[r];
  typename row::iterator ri;
  for (ri = rw.begin(); (ri != rw.end()) && ((*ri).first < c); ++ri);

  if ((ri == rw.end()) || ((*ri).first != c)) {
    // Add new column to the row.
    ri = rw.insert(ri, vnl_sparse_matrix_pair<T>(c,T(0)));
  }

  return (*ri).second;
}

template <class T>
void vnl_sparse_matrix<T>::diag_AtA(vnl_vector<T> & result) const
{
  result.set_size( columns() );
  result.fill(T(0));

  typename vcl_vector<row>::const_iterator row_iter = elements.begin();
  for ( ; row_iter != elements.end(); ++row_iter) {
    row const& this_row = *row_iter;
    typename row::const_iterator col_iter = this_row.begin();
    for ( ; col_iter != this_row.end(); ++col_iter) {
      vnl_sparse_matrix_pair<T> const& entry = *col_iter;
      unsigned const col_id = entry.first;
      result[col_id] += entry.second * entry.second;
    }
  }
}

//------------------------------------------------------------
//: Set row in the matrix.

template <class T>
void vnl_sparse_matrix<T>::set_row(unsigned int r,
                                   vcl_vector<int> const& cols,
                                   vcl_vector<T> const& vals)
{
  assert (r < rows());
  assert (cols.size() == vals.size());

  row& rw = elements[r];
  if (rw.size() != cols.size()) rw = row(cols.size());
  for (unsigned int i=0; i < cols.size(); ++i)
    rw[i] = vnl_sparse_matrix_pair<T>(cols[i], vals[i]);
  typedef typename vnl_sparse_matrix_pair<T>::less less;
  vcl_sort(rw.begin(), rw.end(), less());
}

template <class T>
vnl_sparse_matrix<T>& vnl_sparse_matrix<T>::vcat(vnl_sparse_matrix<T> const& A)
{
  if (rs_ == 0) {
    rs_ = A.rs_;
    cs_ = A.cs_;
    elements = A.elements;
  }
  else {
    assert(cs_ == A.cs_);
    rs_ += A.rs_;
    elements.insert(elements.end(), A.elements.begin(), A.elements.end());
  }
  return *this;
}


//------------------------------------------------------------
//: This is occasionally useful.  Sums a row of the matrix efficiently.
template <class T>
T vnl_sparse_matrix<T>::sum_row(unsigned int r)
{
  assert(r < rows());
  row & rw = elements[r];
  T sum = T(0);
  for (typename row::iterator ri = rw.begin(); ri != rw.end(); ++ri)
    sum += (*ri).second;

  return sum;
}

template <class T>
void vnl_sparse_matrix<T>::scale_row(unsigned int r, T scale)
{
  assert(r < rows());
  row& rw = elements[r];
  for (typename row::iterator ri = rw.begin(); ri != rw.end(); ++ri)
    (*ri).second *= scale;
}

//------------------------------------------------------------
//: Resizes the matrix so that it has r rows and c columns.
//
template <class T>
void vnl_sparse_matrix<T>::set_size( int r, int c)
{
  rs_ = r;
  cs_ = c;
  elements.resize(r);
  typename vnl_sparse_matrix_elements::iterator ie;
  for (ie = elements.begin(); ie != elements.end(); ++ie)
  {
    // just set matrix to 0
    ie->clear();
  }
  reset(); // reset iterator
}

//------------------------------------------------------------
//: Resets the internal iterator
template <class T>
void vnl_sparse_matrix<T>::reset()
{
  itr_isreset = true;
  itr_row = 0;
}

//------------------------------------------------------------
//: Moves the internal iterator to next non-zero entry in matrix.
// Returns true if there is another value, false otherwise. Use
// in combination with methods reset, getrow, getcolumn, and value.
//
template <class T>
bool vnl_sparse_matrix<T>::next()
{
  if ( itr_row >= rows() )
    return false;

  if ( itr_isreset ) {
    // itr_cur is not pointing to a entry
    itr_row = 0;
    itr_isreset = false;
  } else {
    // itr_cur is pointing to an entry.
    // Try to move to next entry in current row.
    itr_cur++;
    if ( itr_cur != elements[itr_row].end() )
      return true;  // found next entry in current row
    else
      itr_row++;
  }

  // search for next entry starting at row itr_row
  while ( itr_row < rows() ) {
    itr_cur = elements[itr_row].begin();
    if ( itr_cur != elements[itr_row].end() )
      return true;
    else
      itr_row++;
  }

  return itr_row < rows();
}

//------------------------------------------------------------
//: Returns the row of the entry pointed to by internal iterator.
//
template <class T>
int vnl_sparse_matrix<T>::getrow()
{
  return itr_row;
}

//------------------------------------------------------------
//: Returns the column of the entry pointed to by internal iterator.
//
template <class T>
int vnl_sparse_matrix<T>::getcolumn()
{
  return (*itr_cur).first;
}

//------------------------------------------------------------
//: Returns the value pointed to by the internal iterator.
//
template <class T>
T vnl_sparse_matrix<T>::value()
{
  return (*itr_cur).second;
}

#define VNL_SPARSE_MATRIX_INSTANTIATE(T) \
template class vnl_sparse_matrix<T >

#endif // vnl_sparse_matrix_txx_
