/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCovariantVector.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkCovariantVector_txx
#define _itkCovariantVector_txx

#include "itkCovariantVector.h" 
#include <vnl/vnl_math.h>
#include "itkNumericTraits.h"

namespace itk
{


template<class T, unsigned int NVectorDimension>
CovariantVector<T, NVectorDimension>&
CovariantVector<T, NVectorDimension>
::operator= (const Self& r)
{
  BaseArray::operator=(r);
  return *this;
}


template<class T, unsigned int NVectorDimension>
CovariantVector<T, NVectorDimension>&
CovariantVector<T, NVectorDimension>
::operator= (const ValueType r[Dimension])
{
  BaseArray::operator=(r);
  return *this;
}


/**
 *
 */
template<class T, unsigned int NVectorDimension>
const CovariantVector<T, NVectorDimension> &
CovariantVector<T, NVectorDimension>
::operator*=( const ValueType & value )
{
  for( unsigned int i=0; i<NVectorDimension; i++) 
  {
    (*this)[i] *= value;
  }
  return *this;
}

  
/**
 *
 */
template<class T, unsigned int NVectorDimension>
const CovariantVector<T, NVectorDimension> &
CovariantVector<T, NVectorDimension>
::operator/=( const ValueType & value )
{
  for( unsigned int i=0; i<NVectorDimension; i++) 
  {
    (*this)[i] /= value;
  }
  return *this;
}


/**
 *
 */
template<class T, unsigned int NVectorDimension>
const typename CovariantVector<T, NVectorDimension>::Self &
CovariantVector<T, NVectorDimension>
::operator+=( const Self & vec )
{
  for( unsigned int i=0; i<NVectorDimension; i++) 
  {
    (*this)[i] += vec[i];
  }
  return *this;
}

 
/**
 *
 */
template<class T, unsigned int NVectorDimension>
const typename CovariantVector<T, NVectorDimension>::Self &
CovariantVector<T, NVectorDimension>
::operator-=( const Self & vec )
{
  for( unsigned int i=0; i<NVectorDimension; i++) 
  {
    (*this)[i] -= vec[i];
  }
  return *this;
}

 
/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int NVectorDimension>
CovariantVector<T, NVectorDimension> 
CovariantVector<T, NVectorDimension>
::operator-() const
{
  Self result;
  for( unsigned int i=0; i<NVectorDimension; i++) 
  {
    result[i] = -(*this)[i];
  }
  return result;
}



/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int NVectorDimension>
typename CovariantVector<T, NVectorDimension>::Self
CovariantVector<T, NVectorDimension>
::operator+( const Self & vec ) const
{
  Self result;
  for( unsigned int i=0; i<NVectorDimension; i++) 
  {
    result[i] = (*this)[i] + vec[i];
  }
  return result;
}



/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int NVectorDimension>
typename CovariantVector<T, NVectorDimension>::Self 
CovariantVector<T, NVectorDimension>
::operator-( const Self & vec )  const
{
  Self result;
  for( unsigned int i=0; i<NVectorDimension; i++) 
  {
    result[i] = (*this)[i] - vec[i];
  }
  return result;
}



/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int NVectorDimension>
CovariantVector<T, NVectorDimension> 
CovariantVector<T, NVectorDimension>
::operator*( const ValueType & value ) const
{
  Self result;
  for( unsigned int i=0; i<NVectorDimension; i++) 
  {
    result[i] = (*this)[i] * value;
  }
  return result;
}


/**
 *
 */
template<class T, unsigned int NVectorDimension>
typename CovariantVector<T, NVectorDimension>::ValueType
CovariantVector<T, NVectorDimension>
::operator*( const Self & other ) const
{
  typename NumericTraits<T>::AccumulateType value = NumericTraits<T>::Zero;
  for( unsigned int i=0; i<NVectorDimension; i++) 
  {
    value += (*this)[i] * other[i];
  }
  return static_cast< ValueType >( value );
}


/**
 *
 */
template<class T, unsigned int NVectorDimension>
typename CovariantVector<T, NVectorDimension>::ValueType
CovariantVector<T, NVectorDimension>
::operator*( const Vector<T,NVectorDimension> & other ) const
{
  typename NumericTraits<T>::AccumulateType value = NumericTraits<T>::Zero;
  for( unsigned int i=0; i<NVectorDimension; i++) 
  {
    value += (*this)[i] * other[i];
  }
  return value;
}



/**
 * Returns vector's Squared Euclidean Norm
 */
template<class T, unsigned int NVectorDimension>
T
CovariantVector<T, NVectorDimension>
::GetSquaredNorm( void ) const
{
  typename NumericTraits<T>::AccumulateType sum = NumericTraits<T>::Zero;
  for( unsigned int i=0; i<NVectorDimension; i++) 
  {
    const T value = (*this)[i];
    sum += value * value;
  }
  return sum;
}



/**
 * Returns vector's Euclidean Norm
 */
template<class T, unsigned int NVectorDimension>
T
CovariantVector<T, NVectorDimension>
::GetNorm( void ) const
{
  return sqrt( GetSquaredNorm() ); 
}



/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int NVectorDimension>
CovariantVector<T, NVectorDimension> 
CovariantVector<T, NVectorDimension>
::operator/( const ValueType & value ) const
{
  Self result;
  for( unsigned int i=0; i<NVectorDimension; i++) 
  {
    result[i] = (*this)[i] / value;
  }
  return result;
}


/**
 * Set a vnl_vector
 */
template<class T, unsigned int NVectorDimension >
void
CovariantVector<T, NVectorDimension >
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
template<class T, unsigned int NVectorDimension>
vnl_vector_ref< T >
CovariantVector<T, NVectorDimension>
::Get_vnl_vector( void ) 
{
  vnl_vector_ref< T > vector_ref( NVectorDimension, this->GetDataPointer() );
  return vector_ref;
}


/**
 * Return a vnl_vector const
 */
template<class T, unsigned int NVectorDimension>
vnl_vector< T >
CovariantVector<T, NVectorDimension>
::Get_vnl_vector( void ) const 
{
  vnl_vector< T > result(NVectorDimension);
  for(unsigned int i=0; i<NVectorDimension; i++)
  {
    result[i] = (*this)[i];
  }
  return result;
}
 


} // end namespace itk


#endif
