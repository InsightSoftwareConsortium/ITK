#ifndef vnl_cross_h_
#define vnl_cross_h_

//:
// \file
// Implements cross product for vectors.
// \author Amitha Perera
// \verbatim
// Modifications
//   Oct.2002 - Amitha Perera - moved from vnl_vector.h
// \endverbatim

#include "vnl_vector.h"
#include "vnl_vector_fixed.h"
#include <vcl_cassert.h>

//: Compute the 2-D cross product
// \relates vnl_vector
template<class T>
T
vnl_cross_2d( const vnl_vector<T>& v1, const vnl_vector<T>& v2 )
{
  assert( v1.size() >= 2 && v2.size() >= 2 );
  return v1[0] * v2[1] - v1[1] * v2[0];
}

//: Compute the 2-D cross product
// \relates vnl_vector_fixed
template<class T>
T
vnl_cross_2d( const vnl_vector_fixed<T,2>& v1, const vnl_vector_fixed<T,2>& v2 )
{
  return v1[0] * v2[1] - v1[1] * v2[0];
}

//: Compute the 3-D cross product
// \relates vnl_vector
template<class T>
vnl_vector<T>
vnl_cross_3d( const vnl_vector<T>& v1, const vnl_vector<T>& v2 )
{
  assert( v1.size() == 3 && v2.size() == 3 );
  vnl_vector<T> result(3);
  result[0] = v1[1] * v2[2] - v1[2] * v2[1]; // work for both col/row
  result[1] = v1[2] * v2[0] - v1[0] * v2[2]; // representation
  result[2] = v1[0] * v2[1] - v1[1] * v2[0];
  return result;
}

//: Compute the 3-D cross product
// \relates vnl_vector_fixed
template<class T>
vnl_vector_fixed<T,3>
vnl_cross_3d( const vnl_vector_fixed<T,3>& v1, const vnl_vector_fixed<T,3>& v2 )
{
  vnl_vector_fixed<T,3> result(3);
  result[0] = v1[1] * v2[2] - v1[2] * v2[1]; // work for both col/row
  result[1] = v1[2] * v2[0] - v1[0] * v2[2]; // representation
  result[2] = v1[0] * v2[1] - v1[1] * v2[0];
  return result;
}

#endif // vnl_cross_h_
