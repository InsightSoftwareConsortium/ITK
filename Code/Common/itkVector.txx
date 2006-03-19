/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVector.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkVector_txx
#define _itkVector_txx

#include "itkVector.h" 
#include "itkNumericTraits.h" 
#include <vnl/vnl_math.h>
#include "vnl/vnl_vector.h"
#include "itkObject.h"
#include "itkNumericTraitsVectorPixel.h"

namespace itk
{


/**
 * Constructor to initialize entire vector to one value.
 */
template<class T, unsigned int TVectorDimension>
Vector<T, TVectorDimension>
::Vector(const ValueType& r)
{
  for(typename BaseArray::Iterator i = BaseArray::Begin(); i != BaseArray::End(); ++i)
    {
    *i = r;
    }
}


template<class T, unsigned int TVectorDimension>
Vector<T, TVectorDimension>&
Vector<T, TVectorDimension>
::operator= (const ValueType r[TVectorDimension])
{
  BaseArray::operator=(r);
  return *this;
}


/**
 *
 */
template<class T, unsigned int TVectorDimension>
const typename Vector<T, TVectorDimension>::Self &
Vector<T, TVectorDimension>
::operator+=( const Self & vec )
{
  for( unsigned int i=0; i<TVectorDimension; i++) 
    {
    (*this)[i] += vec[i];
    }
  return *this;
}

 
/**
 *
 */
template<class T, unsigned int TVectorDimension>
const typename Vector<T, TVectorDimension>::Self &
Vector<T, TVectorDimension>
::operator-=( const Self & vec )
{
  for( unsigned int i=0; i<TVectorDimension; i++) 
    {
    (*this)[i] -= vec[i];
    }
  return *this;
}

 
/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int TVectorDimension>
Vector<T, TVectorDimension> 
Vector<T, TVectorDimension>
::operator-() const
{
  Self result;
  for( unsigned int i=0; i<TVectorDimension; i++) 
    {
    result[i] = -(*this)[i];
    }
  return result;
}



/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int TVectorDimension>
Vector<T, TVectorDimension> 
Vector<T, TVectorDimension>
::operator+( const Self & vec ) const
{
  Self result;
  for( unsigned int i=0; i<TVectorDimension; i++) 
    {
    result[i] = (*this)[i] + vec[i];
    }
  return result;
}



/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int TVectorDimension>
Vector<T, TVectorDimension> 
Vector<T, TVectorDimension>
::operator-( const Self & vec )  const
{
  Self result;
  for( unsigned int i=0; i<TVectorDimension; i++) 
    {
    result[i] = (*this)[i] - vec[i];
    }
  return result;
}


/**
 * Returns vector's Squared Euclidean Norm
 */
template<class T, unsigned int TVectorDimension>
typename Vector<T, TVectorDimension>::RealValueType
Vector<T, TVectorDimension>
::GetSquaredNorm( void ) const
{
  typename NumericTraits<RealValueType>::AccumulateType sum = NumericTraits<T>::Zero;
  for( unsigned int i=0; i<TVectorDimension; i++) 
    {
    const RealValueType value = (*this)[i];
    sum += value * value;
    }
  return sum;
}



/**
 * Returns vector's Euclidean Norm
 */
template<class T, unsigned int TVectorDimension>
typename Vector<T, TVectorDimension>::RealValueType
Vector<T, TVectorDimension>
::GetNorm( void ) const
{
  return RealValueType( vcl_sqrt(double(this->GetSquaredNorm()) )); 
}


/**
 * Divide vector's components by vector's norm
 */
template<class T, unsigned int TVectorDimension>
void
Vector<T, TVectorDimension>
::Normalize( void ) 
{
  const RealValueType norm = this->GetNorm();
  for( unsigned int i=0; i<TVectorDimension; i++) 
    {
    (*this)[i] /= norm;
    }
}

/**
 * Set a vnl_vector
 */
template<class T, unsigned int TVectorDimension>
void
Vector<T, TVectorDimension>
::Set_vnl_vector( const vnl_vector<T> & v)
{
  for(unsigned int i=0;i<v.size();i++) 
    {
    (*this)[i] = v(i);
    } 
}
 

/**
 * Return a vnl_vector_ref
 */
template<class T, unsigned int TVectorDimension>
vnl_vector_ref< T >
Vector<T, TVectorDimension>
::Get_vnl_vector( void ) 
{
  return vnl_vector_ref< T >( TVectorDimension, this->GetDataPointer() );
}
 

/**
 * Return a vnl_vector const
 */
template<class T, unsigned int TVectorDimension>
vnl_vector< T >
Vector<T, TVectorDimension>
::Get_vnl_vector( void ) const 
{
  // Return a vector_ref<>.  This will be automatically converted to a
  // vnl_vector<>.  We have to use a const_cast<> which would normally
  // be prohibited in a const method, but it is safe to do here
  // because the cast to vnl_vector<> will ultimately copy the data.
  return vnl_vector_ref<T>( TVectorDimension,
                            const_cast<T*>(this->GetDataPointer()));
}
 
/**
 * Set a vnl_vector
 */
template<class T, unsigned int TVectorDimension>
void
Vector<T, TVectorDimension>
::SetVnlVector( const vnl_vector<T> & v)
{
  for(unsigned int i=0;i<v.size();i++) 
    {
    (*this)[i] = v(i);
    } 
}
 

/**
 * Return a vnl_vector_ref
 */
template<class T, unsigned int TVectorDimension>
vnl_vector_ref< T >
Vector<T, TVectorDimension>
::GetVnlVector( void ) 
{
  return vnl_vector_ref< T >( TVectorDimension, this->GetDataPointer() );
}
 

/**
 * Return a vnl_vector const
 */
template<class T, unsigned int TVectorDimension>
vnl_vector< T >
Vector<T, TVectorDimension>
::GetVnlVector( void ) const 
{
  // Return a vector_ref<>.  This will be automatically converted to a
  // vnl_vector<>.  We have to use a const_cast<> which would normally
  // be prohibited in a const method, but it is safe to do here
  // because the cast to vnl_vector<> will ultimately copy the data.
  return vnl_vector_ref<T>( TVectorDimension,
                            const_cast<T*>(this->GetDataPointer()));
}

/**
 * Print content to an ostream
 */
template<class T, unsigned int TVectorDimension>
std::ostream &
operator<<(std::ostream& os,const Vector<T,TVectorDimension> & vct ) 
{
  os << "[";
  if ( TVectorDimension == 1)
    {
    os << vct[0];
    }
  else
    {
    for( unsigned int i=0; i+1<TVectorDimension; i++)
      {
      os <<  vct[i] << ", ";
      }
    os << vct[TVectorDimension-1];
    }
  os << "]";
  return os;
}


/**
 * Read content from an istream
 */
template<class T, unsigned int TVectorDimension>
std::istream &
operator>>(std::istream& is, Vector<T,TVectorDimension> & vct ) 
{
  for( unsigned int i=0; i<TVectorDimension; i++)
    {
    is >>  vct[i];
    }
  return is;
}


/**
 *
 */
template<class T, unsigned int TVectorDimension>
typename Vector<T, TVectorDimension>::ValueType
Vector<T, TVectorDimension>
::operator*( const Self & other ) const
{
  typename NumericTraits<T>::AccumulateType value = NumericTraits<T>::Zero;
  for( unsigned int i=0; i<TVectorDimension; i++) 
    {
    value += (*this)[i] * other[i];
    }
  return value;
}




} // end namespace itk


#endif
