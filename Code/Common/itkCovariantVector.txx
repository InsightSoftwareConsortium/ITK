/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCovariantVector.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
const CovariantVector<T, TCovariantVectorDimension> &
CovariantVector<T, TCovariantVectorDimension>
::operator+=( const CovariantVector<T, TCovariantVectorDimension> & vec )
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
const CovariantVector<T, TCovariantVectorDimension> &
CovariantVector<T, TCovariantVectorDimension>
::operator-=( const CovariantVector<T, TCovariantVectorDimension> & vec )
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
CovariantVector<T, TCovariantVectorDimension> 
CovariantVector<T, TCovariantVectorDimension>
::operator+( const CovariantVector<T, TCovariantVectorDimension> & vec ) const
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
CovariantVector<T, TCovariantVectorDimension> 
CovariantVector<T, TCovariantVectorDimension>
::operator-( const CovariantVector<T, TCovariantVectorDimension> & vec )  const
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
  T value = NumericTraits<T>::Zero;
  for( unsigned int i=0; i<TVectorDimension; i++) 
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
  T sum = 0;  // consider a trait for null here ?
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
template<class T, unsigned int TVectorDimension>
void
CovariantVector<T, TVectorDimension>
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


} // end namespace itk


#endif
