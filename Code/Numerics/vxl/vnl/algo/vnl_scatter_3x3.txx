//-*- c++ -*-------------------------------------------------------------------
// vnl_scatter_3x3
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: 02 Oct 96
//-----------------------------------------------------------------------------

#include "vnl_scatter_3x3.h"
#include <vcl_iostream.h>
#include <vnl/algo/vnl_symmetric_eigensystem.h>

template <class T>
vnl_scatter_3x3<T>::vnl_scatter_3x3()
  : base(T(0))
  , symmetricp(true)
  , eigenvectors_currentp(false)
{
}

template <class T>
void vnl_scatter_3x3<T>::add_outer_product(const vnl_vector_fixed<T,3> & v)
  // awf/gcc-2.7.2: vnl_scatter_3x3<T>::vect doesn't work in the arglist
{
  vnl_scatter_3x3<T> & S = *this;
  for(int i = 0; i < 3; ++i) {
    S(i,i) +=  v[i]*v[i];
    for(int j = i+1; j < 3; ++j) {
      T value = v[i]*v[j];
      S(i,j) += value;
      S(j,i) = S(i,j);
    }
  }
}

template <class T>
void vnl_scatter_3x3<T>::add_outer_product(const vnl_vector_fixed<T,3> & u,
                                           const vnl_vector_fixed<T,3> & v)
{
  vnl_scatter_3x3<T> & S = *this;
  for(int i = 0; i < 3; ++i)
    for(int j = 0; j < 3; ++j)
      S(i,j) += v[i]*u[j];
  symmetricp = false;
}

template <class T>
void vnl_scatter_3x3<T>::force_symmetric()
{
  if (symmetricp)
    return;
  vnl_scatter_3x3<T> & S = *this;
  for(int i = 0; i < 3; ++i)
    for(int j = i+1; j < 3; ++j) {
      T vbar = (S(i,j) + S(j,i)) * 0.5;
      S(i,j) = S(j,i) = vbar;
    }
  symmetricp = true;
}

template <class T>
void vnl_scatter_3x3<T>::compute_eigensystem()
{
  vnl_scatter_3x3<T> &S = *this;
  if (symmetricp)
    vnl_symmetric_eigensystem_compute(S, V_, D);
  else
    vcl_cerr << "Unsymmetric scatter not handled now\n";

  eigenvectors_currentp = true;
}

//--------------------------------------------------------------------------------

#define VNL_SCATTER_3X3_INSTANTIATE(T) \
template class vnl_scatter_3x3<T >;
