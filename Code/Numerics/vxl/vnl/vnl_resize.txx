/*
  fsm@robots.ox.ac.uk
*/
#include "vnl_resize.h"
#include <vcl_new.h>  // vcl_destroy()

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_diag_matrix.h>

//--------------------------------------------------------------------------------

template <class T>
void vnl_resize(vnl_vector<T> &v, unsigned newsize)
{
  if (v.size() == newsize)
    return;

  // copy
  vnl_vector<T> old_v(v);
  
  // destruct
  vcl_destroy(&v);
  
  // construct
  new (&v) vnl_vector<T>(newsize);
  
  //
  for (unsigned i=0; i<v.size() && i<old_v.size(); ++i)
    v[i] = old_v[i];
}

template <class T>
void vnl_resize(vnl_matrix<T> &M, unsigned newrows, unsigned newcols)
{
  if (M.rows() == newrows && M.cols() == newcols)
    return;

  // copy
  vnl_matrix<T> old_M(M);
  
  // destruct
  vcl_destroy(&M);
  
  // construct
  new (&M) vnl_matrix<T>(newrows,newcols);

  //
  for (unsigned i=0; i<M.rows() && i<old_M.rows(); ++i)
    for (unsigned j=0; j<M.cols() && j<old_M.cols(); ++j)
      M(i,j) = old_M(i,j);
}

template <class T>
void vnl_resize(vnl_diag_matrix<T> &D, unsigned newsize)
{
  if (D.size() == newsize)
    return;

  // copy
  vnl_diag_matrix<T> old_D(D);
  
  // destruct
  vcl_destroy(&D);
  
  // construct
  new (&D) vnl_diag_matrix<T>(newsize);

  //
  for (unsigned i=0; i<D.size() && i<old_D.size(); ++i)
    D(i, i) = old_D(i, i);
}

//--------------------------------------------------------------------------------

#define VNL_RESIZE_INSTANTIATE(T) \
template void vnl_resize(vnl_vector<T > &, unsigned); \
template void vnl_resize(vnl_matrix<T > &, unsigned, unsigned); \
template void vnl_resize(vnl_diag_matrix<T > &, unsigned);
