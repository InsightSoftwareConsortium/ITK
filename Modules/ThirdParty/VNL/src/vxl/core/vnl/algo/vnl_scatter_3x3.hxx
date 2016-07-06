// This is core/vnl/algo/vnl_scatter_3x3.hxx
#ifndef vnl_scatter_3x3_hxx_
#define vnl_scatter_3x3_hxx_
//:
// \file
// \author Andrew W. Fitzgibbon, Oxford RRG
// Created: 02 Oct 96
//-----------------------------------------------------------------------------

#include <iostream>
#include "vnl_scatter_3x3.h"
#include <vcl_compiler.h>
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
{
  vnl_scatter_3x3<T> & S = *this;
  for (int i = 0; i < 3; ++i) {
    S(i,i) +=  v[i]*v[i];
    for (int j = i+1; j < 3; ++j) {
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
  for (int i = 0; i < 3; ++i)
    for (int j = 0; j < 3; ++j)
      S(i,j) += v[i]*u[j];
  symmetricp = false; // conservative assumption -- use add_outer_product(v) to maintain symmetry
}

template <class T>
void vnl_scatter_3x3<T>::sub_outer_product(const vnl_vector_fixed<T,3> & v)
{
  vnl_scatter_3x3<T> & S = *this;
  for (int i = 0; i < 3; ++i) {
    S(i,i) -=  v[i]*v[i];
    for (int j = i+1; j < 3; ++j) {
      T value = v[i]*v[j];
      S(i,j) -= value;
      S(j,i) = S(i,j);
    }
  }
}

template <class T>
void vnl_scatter_3x3<T>::sub_outer_product(const vnl_vector_fixed<T,3> & u,
                                           const vnl_vector_fixed<T,3> & v)
{
  vnl_scatter_3x3<T> & S = *this;
  for (int i = 0; i < 3; ++i)
    for (int j = 0; j < 3; ++j)
      S(i,j) -= v[i]*u[j];
  symmetricp = false; // conservative assumption -- use sub_outer_product(v) to maintain symmetry
}

template <class T>
void vnl_scatter_3x3<T>::force_symmetric()
{
  if (symmetricp)
    return;
  vnl_scatter_3x3<T> & S = *this;
  for (int i = 0; i < 3; ++i)
    for (int j = i+1; j < 3; ++j) {
      T vbar = (S(i,j) + S(j,i)) / 2;
      S(i,j) = S(j,i) = vbar;
    }
  symmetricp = true;
}

template <class T>
void vnl_scatter_3x3<T>::compute_eigensystem()
{
  vnl_scatter_3x3<T> &S = *this;
  vnl_matrix<T> M = S.as_matrix();
  if (symmetricp) {
    vnl_symmetric_eigensystem_compute(M, V_.as_ref().non_const(), D.as_ref().non_const());
  }
  else {
    std::cerr << "Asymmetric scatter not handled now\n";
  }

  eigenvectors_currentp = true;
}

//--------------------------------------------------------------------------------

#define VNL_SCATTER_3X3_INSTANTIATE(T) \
template class VNL_ALGO_EXPORT vnl_scatter_3x3<T >

#endif // vnl_scatter_3x3_hxx_
