// This is core/vnl/vnl_vector_fixed.hxx
#ifndef vnl_vector_fixed_hxx_
#define vnl_vector_fixed_hxx_
//:
// \file
#include <algorithm>
#include <iostream>
#include <cstdlib>
#include "vnl_vector_fixed.h"
#include "vnl_matrix_fixed.h"

#include <vcl_cassert.h>
#include <vcl_compiler.h>
#include <vnl/vnl_math.h>  // for vnl_math::isfinite

template<class T, unsigned int n>
vnl_vector_fixed<T,n>
vnl_vector_fixed<T,n>::apply( T (*f)(T) )
{
  vnl_vector_fixed<T,n> ret;
  for ( size_type i = 0; i < n; ++i )
    ret[i] = f( data_[i] );
  return ret;
}

template<class T, unsigned int n>
vnl_vector_fixed<T,n>
vnl_vector_fixed<T,n>::apply( T (*f)(const T&) )
{
  vnl_vector_fixed<T,n> ret;
  for ( size_type i = 0; i < n; ++i )
    ret[i] = f( data_[i] );
  return ret;
}

template<class T, unsigned int n>
vnl_vector_fixed<T,n>&
vnl_vector_fixed<T,n>::update( const vnl_vector<T>& v, unsigned int start )
{
  size_type stop = start + v.size();
  assert( stop <= n );
  for (size_type i = start; i < stop; i++)
    this->data_[i] = v[i-start];
  return *this;
}

template <class T, unsigned int n>
vnl_vector_fixed<T,n>&
vnl_vector_fixed<T,n>::flip()
{
  for ( unsigned int i=0; 2*i+1 < n; ++i )
    std::swap( data_[i], data_[n-1-i] );
  return *this;
}

template <class T, unsigned int n>
bool
vnl_vector_fixed<T,n>::is_finite() const
{
  for ( size_type i = 0; i < this->size(); ++i )
    if ( !vnl_math::isfinite( (*this)[i] ) )
      return false;

  return true;
}


template <class T, unsigned int n>
bool
vnl_vector_fixed<T,n>::is_zero() const
{
  T const zero(0);
  for ( size_type i = 0; i < this->size(); ++i )
    if ( !( (*this)[i] == zero) )
      return false;

  return true;
}


template <class T, unsigned int n>
bool
vnl_vector_fixed<T,n>::read_ascii(std::istream& s)
{
  for (size_type i = 0; i < this->size(); ++i)
    s >> (*this)(i);

  return s.good() || s.eof();
}

template <class T, unsigned int n>
void
vnl_vector_fixed<T,n>::assert_finite_internal() const
{
  if (this->is_finite())
    return;

  std::cerr << __FILE__ ": *** NAN FEVER **\n" << *this;
  std::abort();
}

template <class T, unsigned int n>
void
vnl_vector_fixed<T,n>::print(std::ostream& s) const
{
  if (this->size() > 0)
    s << (*this)[0];
  for (size_type i=1; i < this->size(); ++i)
    s << ' ' << (*this)[i];
}


// we don't need to explicitly instantiate all the operator+ and such
// since they appear in the .h file and are inline.

#define VNL_VECTOR_FIXED_INSTANTIATE(T,n) \
template class VNL_EXPORT vnl_vector_fixed<T,n >

#endif // vnl_vector_fixed_hxx_
