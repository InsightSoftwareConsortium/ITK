// This is core/vnl/vnl_crs_index.h
#ifndef vnl_crs_index_h_
#define vnl_crs_index_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Compressed Row Storage (CRS) indexing
// \author Matt Leotta (Brown)
// \date   April 13, 2005
//
// \verbatim
//  Modifications
// \endverbatim
//
#include <vector>
#include <utility>
#include <vcl_compiler.h>
#include "vnl/vnl_export.h"

//: Represents the configuration of a sparse matrix but not the data
//  This is essentially a sparse matrix of indices into a data vector
//  Compressed row storage is used for representation
//  This class is useful when working with several sparse matrices that
//  share a common sparse structure.
class VNL_EXPORT vnl_crs_index
{
 public:
  typedef std::pair<int,int> idx_pair;
  typedef std::vector<idx_pair> sparse_vector;

  //: Constructor - default
  vnl_crs_index() : num_cols_(0), col_idx_(), row_ptr_() {}

  //: Constructor - from a binary mask
  vnl_crs_index(const std::vector<std::vector<bool> >& mask);

  //: Destructor
  ~vnl_crs_index(){}

  //: number of rows in the sparse matrix
  int num_rows() const { return int(row_ptr_.size())-1; }

  //: number of columns in the sparse matrix
  int num_cols() const { return num_cols_; }

  //: number of non-zero elements
  int num_non_zero() const { return int(col_idx_.size()); }

  //: returns row \p i as a vector of index-column pairs
  sparse_vector sparse_row(int i) const;

  //: returns column \p j as a vector of index-row pairs
  // \note because of CRS this method is a bit less efficient than sparse_row
  sparse_vector sparse_col(int j) const;

  //: return the index at location (i,j)
  //  returns -1 if the entry is 0
  int operator() (int i, int j) const;

 private:
  //: The number of columns in the matrix
  unsigned int num_cols_;
  //: The column for each non-zero element
  std::vector<int> col_idx_;
  //: The index of the first non-zero element in each row
  std::vector<int> row_ptr_;
};

#endif // vnl_crs_index_h_
