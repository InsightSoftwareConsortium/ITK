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


template<class T, unsigned int TCovariantVectorDimension>
CovariantVector<T, TCovariantVectorDimension>&
CovariantVector<T, TCovariantVectorDimension>
::operator= (const Self& r)
{
  BaseArray::operator=(r);
  return *this;
}


template<class T, unsigned int TCovariantVectorDimension>
CovariantVector<T, TCovariantVectorDimension>&
CovariantVector<T, TCovariantVectorDimension>
::operator= (const typename BaseArray::Reference& r)
{
  BaseArray::operator=(r);
  return *this;
}


template<class T, unsigned int TCovariantVectorDimension>
CovariantVector<T, TCovariantVectorDimension>&
CovariantVector<T, TCovariantVectorDimension>
::operator= (const typename BaseArray::ConstReference& r)
{
  BaseArray::operator=(r);
  return *this;
}


template<class T, unsigned int TCovariantVectorDimension>
CovariantVector<T, TCovariantVectorDimension>&
CovariantVector<T, TCovariantVectorDimension>
::operator= (const ValueType r[CovariantVectorDimension])
{
  BaseArray::operator=(r);
  return *this;
}


template<class T, unsigned int TCovariantVectorDimension>
typename CovariantVector<T, TCovariantVectorDimension>::ArrayCommaListCopier
CovariantVector<T, TCovariantVectorDimension>
::operator= (const ValueType& r)
{
  return BaseArray::operator=(r);
}


/**
 *
 */
template<class T, unsigned int TCovariantVectorDimension>
const CovariantVector<T, TCovariantVectorDimension> &
CovariantVector<T, TCovariantVectorDimension>
::operator*=( const ValueType & value )
{
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    (*this)[i] *= value;
  }
  return *this;
}

  
/**
 *
 */
template<class T, unsigned int TCovariantVectorDimension>
const CovariantVector<T, TCovariantVectorDimension> &
CovariantVector<T, TCovariantVectorDimension>
::operator/=( const ValueType & value )
{
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    (*this)[i] /= value;
  }
  return *this;
}


/**
 *
 */
template<class T, unsigned int TCovariantVectorDimension>
const CovariantVector<T, TCovariantVectorDimension>::Self &
CovariantVector<T, TCovariantVectorDimension>
::operator+=( const Self & vec )
{
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    (*this)[i] += vec[i];
  }
  return *this;
}

 
/**
 *
 */
template<class T, unsigned int TCovariantVectorDimension>
const CovariantVector<T, TCovariantVectorDimension>::Self &
CovariantVector<T, TCovariantVectorDimension>
::operator-=( const Self & vec )
{
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    (*this)[i] -= vec[i];
  }
  return *this;
}

 
/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int TCovariantVectorDimension>
CovariantVector<T, TCovariantVectorDimension> 
CovariantVector<T, TCovariantVectorDimension>
::operator-() const
{
  Self result;
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    result[i] = -(*this)[i];
  }
  return result;
}



/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int TCovariantVectorDimension>
CovariantVector<T, TCovariantVectorDimension>::Self
CovariantVector<T, TCovariantVectorDimension>
::operator+( const Self & vec ) const
{
  Self result;
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    result[i] = (*this)[i] + vec[i];
  }
  return result;
}



/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int TCovariantVectorDimension>
CovariantVector<T, TCovariantVectorDimension>::Self 
CovariantVector<T, TCovariantVectorDimension>
::operator-( const Self & vec )  const
{
  Self result;
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    result[i] = (*this)[i] - vec[i];
  }
  return result;
}



/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int TCovariantVectorDimension>
CovariantVector<T, TCovariantVectorDimension> 
CovariantVector<T, TCovariantVectorDimension>
::operator*( const ValueType & value ) const
{
  Self result;
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    result[i] = (*this)[i] * value;
  }
  return result;
}


/**
 *
 */
template<class T, unsigned int TCovariantVectorDimension>
typename CovariantVector<T, TCovariantVectorDimension>::ValueType
CovariantVector<T, TCovariantVectorDimension>
::operator*( const Self & other ) const
{
  NumericTraits<T>::AccumulateType value = NumericTraits<T>::Zero;
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    value += (*this)[i] * other[i];
  }
  return static_cast< ValueType >( value );
}


/**
 *
 */
template<class T, unsigned int TCovariantVectorDimension>
typename CovariantVector<T, TCovariantVectorDimension>::ValueType
CovariantVector<T, TCovariantVectorDimension>
::operator*( const Vector<T,TCovariantVectorDimension> & other ) const
{
  NumericTraits<T>::AccumulateType value = NumericTraits<T>::Zero;
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    value += (*this)[i] * other[i];
  }
  return value;
}



/**
 * Returns vector's Squared Euclidean Norm
 */
template<class T, unsigned int TCovariantVectorDimension>
T
CovariantVector<T, TCovariantVectorDimension>
::GetSquaredNorm( void ) const
{
  NumericTraits<T>::AccumulateType sum = NumericTraits<T>::Zero;
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    const T value = (*this)[i];
    sum += value * value;
  }
  return sum;
}



/**
 * Returns vector's Euclidean Norm
 */
template<class T, unsigned int TCovariantVectorDimension>
T
CovariantVector<T, TCovariantVectorDimension>
::GetNorm( void ) const
{
  return sqrt( GetSquaredNorm() ); 
}



/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int TCovariantVectorDimension>
CovariantVector<T, TCovariantVectorDimension> 
CovariantVector<T, TCovariantVectorDimension>
::operator/( const ValueType & value ) const
{
  Self result;
  for( unsigned int i=0; i<TCovariantVectorDimension; i++) 
  {
    result[i] = (*this)[i] / value;
  }
  return result;
}


/**
 * Set a vnl_vector
 */
template<class T, unsigned int TCovariantVectorDimension >
void
CovariantVector<T, TCovariantVectorDimension >
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
template<class T, unsigned int TCovariantVectorDimension>
vnl_vector_ref< T >
CovariantVector<T, TCovariantVectorDimension>
::Get_vnl_vector( void ) 
{
  vnl_vector_ref< T > vector_ref( TCovariantVectorDimension, this->GetDataPointer() );
  return vector_ref;
}


/**
 * Return a vnl_vector const
 */
template<class T, unsigned int TCovariantVectorDimension>
vnl_vector< T >
CovariantVector<T, TCovariantVectorDimension>
::Get_vnl_vector( void ) const 
{
  vnl_vector< T > result(TCovariantVectorDimension);
  for(unsigned int i=0; i<TCovariantVectorDimension; i++)
  {
    result[i] = (*this)[i];
  }
  return result;
}
 


} // end namespace itk


#endif
