// This is core/vnl/algo/vnl_matrix_update.h
#ifndef vnl_matrix_update_h_
#define vnl_matrix_update_h_
//:
// \file
// \brief Function to compute M=M+a*b'
// \author  Tim Cootes

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vcl_cassert.h>

//: Perform rank 1 update of M:   M+=(a*b')
//  Requires a.size()==M.rows(),  b.size()==M.columns()
//  \relatesalso vnl_matrix
template<class T>
inline void vnl_matrix_update(vnl_matrix<T>& M,
                              const vnl_vector<T>& a,
                              const vnl_vector<T>& b)
{
  unsigned nr=M.rows();
  unsigned nc=M.columns();
  assert(a.size()==nr);
  assert(b.size()==nc);
  T** rows=M.data_array();
  for (unsigned i=0;i<nr;++i)
  {
    // Update row i of M
    double ai = a[i];
    T* row= rows[i]-1;
    const T* b_data=b.data_block()-1;
    // Fast loop through elements in row
    for (unsigned j=nc;j;--j) row[j] += ai*b_data[j];
  }
}

#endif // vnl_matrix_update_h_

