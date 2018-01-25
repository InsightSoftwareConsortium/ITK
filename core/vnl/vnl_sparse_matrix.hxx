// This is core/vnl/vnl_sparse_matrix.hxx
#ifndef vnl_sparse_matrix_hxx_
#define vnl_sparse_matrix_hxx_
//:
// \file

#include <algorithm>
#include <iostream>
#include "vnl_sparse_matrix.h"

#include <vcl_cassert.h>
#include <vcl_compiler.h>

#include <vnl/vnl_math.h>
#include <vnl/vnl_complex_traits.h>

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
  assert(this != &result); // make sure not to overwrite *this
  assert(&rhs != &result); // make sure not to overwrite rhs
  unsigned int result_rows = rows();
  unsigned int result_cols = rhs.columns();

  // Early return: empty result matrix
  if (result_rows <= 0 || result_cols <= 0) return;

  result.cs_ = result_cols;
  if (result.rows() != result_rows)
  {
    // Clear result matrix.
    result.elements.clear();
    // give the result matrix enough rows (but only if not yet correct).
    result.elements.resize(result_rows);
    result.rs_ = result_rows;
    for (unsigned row_id=0; row_id<result_rows; ++row_id)
      result.elements[row_id] = row();
  }

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
  int size = rows()*pcols;
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

  std::cout << "Initial p:\n";
  for (int rr = 0; rr<prows; rr++) {
    for (int cc = 0; cc<pcols; cc++) {
      T pval = p[rr + cc*prows];
      std::cout << pval << ' ';
    }
    std::cout << '\n';
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

        // Add the product into the correct position in q (fortran order)
        q[row_id + p_col_id*rows()] += prod;
      }
    }
  }

#ifdef DEBUG_SPARSE
  std::cout << "Final q:\n";
  for (int rr = 0; rr<prows; rr++) {
    for (int cc = 0; cc<pcols; cc++) {
      T pval = q[rr + cc*prows];
      std::cout << pval << ' ';
    }
    std::cout << '\n';
  }
  std::cout << "nonsparse: " << md*pd << '\n';
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
  typename std::vector<row>::const_iterator lhs_row_iter = elements.begin();
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
  for (typename std::vector<row>::const_iterator rhs_row_iter = elements.begin();
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
  for (typename std::vector<row>::const_iterator row_iter = elements.begin();
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
  for (typename std::vector<row>::const_iterator row_iter = elements.begin();
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
  for (ri = rw.begin(); (ri != rw.end()) && ((*ri).first < c); ++ri)
    /*nothing*/;

  if ((ri == rw.end()) || ((*ri).first != c)) {
    // Add new column to the row.
    ri = rw.insert(ri, vnl_sparse_matrix_pair<T>(c,T()));
  }

  return (*ri).second;
}

//------------------------------------------------------------
//: Get the value of an entry in the matrix.
template <class T>
T vnl_sparse_matrix<T>::operator()(unsigned int r, unsigned int c) const
{
  assert((r < rows()) && (c < columns()));
  row const& rw = elements[r];
  typename row::const_iterator ri = rw.begin();
  while (ri != rw.end() && (*ri).first < c)
    ++ri;
  if (ri == rw.end() || (*ri).first != c)
    return T(); // uninitialised value (default constructor) is returned
  else
    return (*ri).second;
}

//------------------------------------------------------------
//: Get an entry in the matrix.
//  This is the "const" version of operator().
template <class T>
T vnl_sparse_matrix<T>::get(unsigned int r, unsigned int c) const
{
  assert((r < rows()) && (c < columns()));
  row const& rw = elements[r];
  typename row::const_iterator ri = rw.begin();
  while (ri != rw.end() && (*ri).first < c)
    ++ri;
  if (ri == rw.end() || (*ri).first != c)
    return T(); // uninitialised value (default constructor) is returned
  else
    return (*ri).second;
}

//------------------------------------------------------------
//: Put (i.e., add or overwrite) an entry into the matrix.
template <class T>
void vnl_sparse_matrix<T>::put(unsigned int r, unsigned int c, T v)
{
  assert((r < rows()) && (c < columns()));
  row& rw = elements[r];
  typename row::iterator ri = rw.begin();
  while (ri != rw.end() && (*ri).first < c)
    ++ri;

  if (ri == rw.end() || (*ri).first != c) {
    // Add new column to the row.
    rw.insert(ri, vnl_sparse_matrix_pair<T>(c,v));
  }
  else
    (*ri).second = v;
}

template <class T>
void vnl_sparse_matrix<T>::diag_AtA(vnl_vector<T> & result) const
{
  result.set_size( columns() );
  result.fill(T(0));

  typename std::vector<row>::const_iterator row_iter = elements.begin();
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
vnl_sparse_matrix<T>&
vnl_sparse_matrix<T>::set_row(unsigned int r,
                              std::vector<int> const& colz,
                              std::vector<T> const& vals)
{
  assert (r < rows());
  assert (colz.size() == vals.size());

  row& rw = elements[r];
  if (rw.size() != colz.size()) rw = row(colz.size());
  for (unsigned int i=0; i < colz.size(); ++i)
    rw[i] = vnl_sparse_matrix_pair<T>(colz[i], vals[i]);
  typedef typename vnl_sparse_matrix_pair<T>::less less;
  std::sort(rw.begin(), rw.end(), less());
  return *this;
}

template <class T>
vnl_sparse_matrix<T>&
vnl_sparse_matrix<T>::vcat(vnl_sparse_matrix<T> const& A)
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
vnl_sparse_matrix<T>&
vnl_sparse_matrix<T>::scale_row(unsigned int r, T scale)
{
  assert(r < rows());
  row& rw = elements[r];
  for (typename row::iterator ri = rw.begin(); ri != rw.end(); ++ri)
    (*ri).second *= scale;
  return *this;
}

//------------------------------------------------------------
//: Resizes the matrix so that it has r rows and c columns, clearing the current contents.
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
//: Resizes the matrix so that it has r rows and c columns, leaving the current contents.
// This is more wasteful of resources than set_size, but it preserves the contents.
//
template <class T>
void vnl_sparse_matrix<T>::resize( int r, int c)
{
  unsigned int oldCs = cs_;

  rs_ = r;
  cs_ = c;
  elements.resize(r);

  // If the array has fewer columns now, we also need to cut them out
  if (oldCs > cs_) {
    for (unsigned int i = 0; i < elements.size(); ++i) {
      row& rw = elements[i];
      typename row::iterator iter;
      for (iter = rw.begin(); iter != rw.end() && (*iter).first<cs_ ; ++iter)
        /*nothing*/;
      if (iter != rw.end()) rw.erase(iter,rw.end());
    }
  }

  reset(); // reset iterator
}

//------------------------------------------------------------
//: Resets the internal iterator
template <class T>
void vnl_sparse_matrix<T>::reset() const
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
bool vnl_sparse_matrix<T>::next() const
{
  if ( itr_row >= rows() )
    return false;

  if ( itr_isreset ) {
    // itr_cur is not pointing to an entry
    itr_row = 0;
    itr_isreset = false;
  }
  else {
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
int vnl_sparse_matrix<T>::getrow() const
{
  return itr_row;
}

//------------------------------------------------------------
//: Returns the column of the entry pointed to by internal iterator.
//
template <class T>
int vnl_sparse_matrix<T>::getcolumn() const
{
  return (*itr_cur).first;
}

//------------------------------------------------------------
//: Returns the value pointed to by the internal iterator.
//
template <class T>
T vnl_sparse_matrix<T>::value() const
{
  return (*itr_cur).second;
}

//------------------------------------------------------------
//: Comparison
//
template <class T>
bool vnl_sparse_matrix<T>::operator==(vnl_sparse_matrix<T> const& rhs) const
{
  // first of all, sizes must match:
  if (rhs.rows() != rows() || rhs.columns() != columns()) {
#ifdef DEBUG_SPARSE
    std::cerr << "Sizes are different: " << rows() << 'x' << columns() << ' ' << rhs.rows() << 'x' << rhs.columns() << '\n';
#endif
    return false;
  }

  // Now, iterate over non-zero rows of this and of rhs.
  unsigned int row_id = 0;
  for (typename std::vector<row>::const_iterator row_iter = elements.begin();
       row_iter != elements.end();
       ++row_iter, ++row_id)
  {
    // Get the row from this matrix (lhs).
    row const& this_row = *row_iter;

    // Get the rhs row.
    row const& rhs_row = rhs.elements[row_id];

    // first of all, row sizes must match:
    if (rhs_row.size() != this_row.size())
      return false;

    // Iterate over the rhs row.
    for (typename row::const_iterator col_iter = rhs_row.begin();
         col_iter != rhs_row.end();
         ++col_iter)
    {
      // Get the element from the row.
      vnl_sparse_matrix_pair<T> const& entry = *col_iter;
      unsigned const col_id = entry.first;

      // So we are at (row_id,col_id) in rhs matrix.
      if (get(row_id,col_id) != entry.second)
        return false;
    }
  }
  // if we reach this point, all comparisons succeeded:
  return true;
}

//: Unary minus
template <class T>
vnl_sparse_matrix<T> vnl_sparse_matrix<T>::operator-() const
{
  // The matrix to be returned:
  vnl_sparse_matrix<T> result(rows(), columns());

  // Iterate over non-zero rows of this matrix.
  unsigned int row_id = 0;
  for (typename std::vector<row>::const_iterator row_iter = elements.begin();
       row_iter != elements.end();
       ++row_iter, ++row_id)
  {
    // Get the row.
    row const& this_row = *row_iter;

    // Iterate over the row.
    for (typename row::const_iterator col_iter = this_row.begin();
         col_iter != this_row.end();
         ++col_iter)
    {
      // Assign the corresponding result element.
      vnl_sparse_matrix_pair<T> const& entry = *col_iter;
      result(row_id, entry.first) = - entry.second;
    }
  }
  return result;
}

//: addition
template <class T>
vnl_sparse_matrix<T> vnl_sparse_matrix<T>::operator+(vnl_sparse_matrix<T> const& rhs) const
{
  vnl_sparse_matrix<T> result(rows(), columns());
  add(rhs, result);
  return result;
}

//: subtraction
template <class T>
vnl_sparse_matrix<T> vnl_sparse_matrix<T>::operator-(vnl_sparse_matrix<T> const& rhs) const
{
  vnl_sparse_matrix<T> result(rows(), columns());
  subtract(rhs, result);
  return result;
}

//: multiplication
template <class T>
vnl_sparse_matrix<T> vnl_sparse_matrix<T>::operator*(vnl_sparse_matrix<T> const& rhs) const
{
  vnl_sparse_matrix<T> result(rows(), rhs.columns());
  mult(rhs, result);
  return result;
}

//: in-place scalar multiplication
template <class T>
vnl_sparse_matrix<T>& vnl_sparse_matrix<T>::operator*=(T const& rhs)
{
  // Iterate over non-zero rows of this matrix.
  for (typename std::vector<row>::iterator row_iter = elements.begin();
       row_iter != elements.end();
       ++row_iter)
  {
    // Get the row.
    row& this_row = *row_iter;

    // Iterate over the row.
    for (typename row::iterator col_iter = this_row.begin();
         col_iter != this_row.end();
         ++col_iter)
    {
      // Change the corresponding element.
      col_iter->second *= rhs;
    }
  }
  return *this;
}

//: in-place scalar division
template <class T>
vnl_sparse_matrix<T>& vnl_sparse_matrix<T>::operator/=(T const& rhs)
{
  // Iterate over non-zero rows of this matrix.
  for (typename std::vector<row>::iterator row_iter = elements.begin();
       row_iter != elements.end();
       ++row_iter)
  {
    // Get the row.
    row& this_row = *row_iter;

    // Iterate over the row.
    for (typename row::iterator col_iter = this_row.begin();
         col_iter != this_row.end();
         ++col_iter)
    {
      // Change the corresponding element.
      col_iter->second /= rhs;
    }
  }
  return *this;
}

//: scalar multiplication
template <class T>
vnl_sparse_matrix<T> vnl_sparse_matrix<T>::operator*(T const& rhs) const
{
  vnl_sparse_matrix<T> result = *this;
  return result *= rhs;
}

//: scalar division
template <class T>
vnl_sparse_matrix<T> vnl_sparse_matrix<T>::operator/(T const& rhs) const
{
  vnl_sparse_matrix<T> result = *this;
  return result /= rhs;
}

//: in-place addition
template <class T>
vnl_sparse_matrix<T>& vnl_sparse_matrix<T>::operator+=(vnl_sparse_matrix<T> const& rhs)
{
  return *this = operator+(rhs);
}

//: in-place subtraction
template <class T>
vnl_sparse_matrix<T>& vnl_sparse_matrix<T>::operator-=(vnl_sparse_matrix<T> const& rhs)
{
  return *this = operator-(rhs);
}

//: in-place multiplication
template <class T>
vnl_sparse_matrix<T>& vnl_sparse_matrix<T>::operator*=(vnl_sparse_matrix<T> const& rhs)
{
  return *this = operator*(rhs);
}

//: Make each row of the matrix have unit norm.
// All-zero rows are ignored.
template<class T>
vnl_sparse_matrix<T>& vnl_sparse_matrix<T>::normalize_rows()
{
  typedef typename vnl_numeric_traits<T>::abs_t Abs_t;
  typedef typename vnl_numeric_traits<T>::real_t Real_t;
  typedef typename vnl_numeric_traits<Real_t>::abs_t abs_real_t;

  // Iterate through the matrix rows, and normalize one at a time:
  for (typename std::vector<row>::iterator row_iter = elements.begin();
       row_iter != elements.end();
       ++row_iter)
  {
    // Get the row.
    row& this_row = *row_iter;

    Abs_t norm(0); // double will not do for all types.

    // Iterate over the row
    for (typename row::iterator col_iter = this_row.begin();
         col_iter != this_row.end();
         ++col_iter)
    {
      vnl_sparse_matrix_pair<T>& entry = *col_iter;
      norm += vnl_math::squared_magnitude(entry.second);
    }
    if (norm != 0) {
      abs_real_t scale = abs_real_t(1)/(std::sqrt((abs_real_t)norm));
      // Iterate again over the row
      for (typename row::iterator col_iter = this_row.begin();
           col_iter != this_row.end();
           ++col_iter)
      {
        vnl_sparse_matrix_pair<T>& entry = *col_iter;
        entry.second = T(Real_t(entry.second) * scale);
      }
    }
  }
  return *this;
}

//: Fill this matrix with 1s on the main diagonal and 0s elsewhere.
template<class T>
vnl_sparse_matrix<T>& vnl_sparse_matrix<T>::set_identity()
{
  // Iterate through the matrix rows, and set one at a time:
  unsigned int rownum = 0;
  for (typename std::vector<row>::iterator row_iter = elements.begin();
       row_iter != elements.end() && rownum < cols();
       ++row_iter, ++rownum)
  {
    row& rw = *row_iter;
    rw.clear();
    rw[0] = vnl_sparse_matrix_pair<T>(rownum,T(1));
  }
  return *this;
}

//: returns a new sparse matrix, viz. the transpose of this
template<class T>
vnl_sparse_matrix<T> vnl_sparse_matrix<T>::transpose() const
{
  vnl_sparse_matrix<T> result(cols(), rows());
  unsigned int rownum = 0; // row number in this matrix
  // iterate through the rows of this matrix,
  // and add every element thus found to the new result matrix
  for (typename std::vector<row>::const_iterator row_iter = elements.begin();
       row_iter != elements.end();
       ++row_iter, ++rownum)
  {
    row const& this_row = *row_iter;
    for (typename row::const_iterator col_iter = this_row.begin();
         col_iter != this_row.end();
         ++col_iter)
    {
      vnl_sparse_matrix_pair<T> entry = *col_iter; // new copy of element
      row& rw = result.elements[entry.first];
      entry.first = rownum; // modify element: its column number is now rownum
      rw.insert(rw.end(), entry); // insert at the end of the row
    }
  }
  return result;
}

//: returns a new sparse matrix, viz. the conjugate (or Hermitian) transpose of this
template<class T>
vnl_sparse_matrix<T> vnl_sparse_matrix<T>::conjugate_transpose() const
{
  vnl_sparse_matrix<T> result(transpose());
  for (typename std::vector<row>::iterator row_iter = result.elements.begin();
       row_iter != result.elements.end();
       ++row_iter)
  {
    row& this_row = *row_iter;
    for (typename row::iterator col_iter = this_row.begin();
         col_iter != this_row.end();
         ++col_iter)
    {
      vnl_sparse_matrix_pair<T>& entry = *col_iter;
      entry.second = vnl_complex_traits<T>::conjugate(entry.second);
    }
  }
  return result;
}

#define VNL_SPARSE_MATRIX_INSTANTIATE(T) \
template class VNL_EXPORT vnl_sparse_matrix<T >

#endif // vnl_sparse_matrix_hxx_
