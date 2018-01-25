// This is core/vnl/vnl_crs_index.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author Matt Leotta (Brown)
// \date   April 13, 2005

#include "vnl_crs_index.h"

//: Constructor - from a binary mask
vnl_crs_index::vnl_crs_index(const std::vector<std::vector<bool> >& mask)
 : num_cols_(mask[0].size()), col_idx_(), row_ptr_(mask.size()+1,0)
{
  int k=0;
  for (unsigned int i=0; i<mask.size(); ++i){
    const std::vector<bool>& col = mask[i];
    row_ptr_[i] = k;
    for (unsigned int j=0; j<num_cols_; ++j){
      if (col[j]){
        col_idx_.push_back(j);
        ++k;
      }
    }
  }
  row_ptr_[mask.size()] = k;
}


//: return the index at location (i,j)
//  returns -1 if the entry is 0
int
vnl_crs_index::operator() (int i, int j) const
{
  int low = row_ptr_[i];
  int high = row_ptr_[i+1]-1;

  // binary search for finding the element at column j
  while (low<=high){
    if (j<col_idx_[low] || j>col_idx_[high])
      return -1; // element is zero (no index)

    int mid = (low+high)>>1; //(low+high)/2;
    if (j<(int)col_idx_[mid])
        high = mid-1;
    else if (j>(int)col_idx_[mid])
        low=mid+1;
    else
      return mid;
  }

  return -1; // element is zero (no index)
}


//: returns row \p i as a vector of index-column pairs
vnl_crs_index::sparse_vector
vnl_crs_index::sparse_row(int i) const
{
  sparse_vector row;
  for (int j=row_ptr_[i]; j<row_ptr_[i+1]; ++j){
    row.push_back(idx_pair(j,col_idx_[j]));
  }
  return row;
}


//: returns column \p j as a vector of index-row pairs
// \note because of CRS this method is a bit less efficient than sparse_row
vnl_crs_index::sparse_vector
vnl_crs_index::sparse_col(int j) const
{
  sparse_vector col;
  for (int i=0; i<num_rows(); ++i){
    int idx = (*this)(i,j);
    if (idx >= 0)
      col.push_back(idx_pair(idx,i));
  }

  return col;
}
