// This is core/vnl/vnl_operators.h
#ifndef vnl_operators_h_
#define vnl_operators_h_
//:
// \file
// \brief Various operators for templated vnl classes
// \author Ian Scott

#include <vnl/vnl_vector.h>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_matrix_fixed.h>

//: Define a complete ordering on vnl_vector
// This is useful to create a set, or map of vectors.
//
// The ordering itself is implementation defined - so don't rely
// on the meaning of less here.
//
// \relatesalso vnl_vector

template<class T>
bool operator<(vnl_vector<T> const& lhs, vnl_vector<T> const& rhs)
{
  if (&lhs == &rhs)  return false;              // same object => equal.

  if (lhs.size() < rhs.size())  return true;    // Size different ?
  else if (lhs.size() > rhs.size()) return false;

  for (unsigned i = 0; i < lhs.size(); i++)         // For each index
  {
    if (lhs(i) < rhs(i)) return true; // Element different ?
    else if (lhs(i) > rhs(i)) return false;
  }
  return false;                                 // Else all same.
}

//: Define a complete ordering on vnl_matrix
// This is useful to create a set, or map of matrices.
//
// The ordering itself is implementation defined - so don't rely
// on the meaning of less here.
//
// \relatesalso vnl_matrix

template<class T>
bool operator<(vnl_matrix<T> const& lhs, vnl_matrix<T> const& rhs)
{
  if (&lhs == &rhs)  return false;              // same object => equal.

  if (lhs.rows() < rhs.rows())  return true;        // Size different ?
  else if (lhs.rows() > rhs.rows()) return false;
  else if (lhs.cols() < rhs.cols())  return true;
  else if (lhs.cols() > rhs.cols()) return false;

  for (unsigned i = 0; i < lhs.size(); i++)         // For each index
  {
    if (lhs.data_block()[i] < rhs.data_block()[i]) return true; // Element different ?
    else if (lhs.data_block()[i] > rhs.data_block()[i]) return false;
  }
  return false;                                 // Else all same.
}

//: Define a complete ordering on vnl_vector_fixed
// This is useful to create a set, or map of vectors.
//
// \relatesalso vnl_vector_fixed

template<class T, unsigned int n>
bool operator<(vnl_vector_fixed<T,n> const& lhs, vnl_vector_fixed<T,n> const& rhs)
{
  return lhs.as_ref() < rhs.as_ref();
}

//: Define a complete ordering on vnl_matrix_fixed
// This is useful to create a set, or map of matrices.
//
// \relatesalso vnl_matrix_fixed

template<class T, unsigned int n, unsigned int m>
bool operator<(vnl_matrix_fixed<T,n,m> const& lhs, vnl_matrix_fixed<T,n,m> const& rhs)
{
  return lhs.as_ref() < rhs.as_ref();
}

#endif // vnl_operators_h_
