/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVector.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
::operator= (const Self& r)
{
  BaseArray::operator=(r);
  return *this;
}


template<class T, unsigned int TVectorDimension>
Vector<T, TVectorDimension>&
Vector<T, TVectorDimension>
::operator= (const ValueType r[Dimension])
{
  BaseArray::operator=(r);
  return *this;
}


/**
 *
 */
template<class T, unsigned int TVectorDimension>
const Vector<T, TVectorDimension> &
Vector<T, TVectorDimension>
::operator*=( const ValueType & value )
{
  for( unsigned int i=0; i<TVectorDimension; i++) 
  {
    (*this)[i] *= value;
  }
  return *this;
}

  
/**
 *
 */
template<class T, unsigned int TVectorDimension>
const Vector<T, TVectorDimension> &
Vector<T, TVectorDimension>
::operator/=( const ValueType & value )
{
  for( unsigned int i=0; i<TVectorDimension; i++) 
  {
    (*this)[i] /= value;
  }
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
 * Multiply components by a constant
 */
template<class T, unsigned int TVectorDimension>
Vector<T, TVectorDimension> 
Vector<T, TVectorDimension>
::operator*( const ValueType & value ) const
{
  Self result;
  for( unsigned int i=0; i<TVectorDimension; i++) 
  {
    result[i] = (*this)[i] * value;
  }
  return result;
}

 
/**
 * Returns vector's Squared Euclidean Norm
 */
template<class T, unsigned int TVectorDimension>
typename Vector<T, TVectorDimension>::ValueType
Vector<T, TVectorDimension>
::GetSquaredNorm( void ) const
{
  typename NumericTraits<T>::AccumulateType sum = NumericTraits<T>::Zero;
  for( unsigned int i=0; i<TVectorDimension; i++) 
  {
    const ValueType value = (*this)[i];
    sum += value * value;
  }
  return sum;
}



/**
 * Returns vector's Euclidean Norm
 */
template<class T, unsigned int TVectorDimension>
typename Vector<T, TVectorDimension>::ValueType
Vector<T, TVectorDimension>
::GetNorm( void ) const
{
  return ValueType(sqrt( double(this->GetSquaredNorm()) )); 
}


/**
 * Divide vector's components by vector's norm
 */
template<class T, unsigned int TVectorDimension>
void
Vector<T, TVectorDimension>
::Normalize( void ) 
{
  T norm = GetNorm();
  for( unsigned int i=0; i<TVectorDimension; i++) 
  {
    (*this)[i] /= norm;
  }
}




/**
 * Returns a temporary copy of a vector
 */
template<class T, unsigned int TVectorDimension>
Vector<T, TVectorDimension> 
Vector<T, TVectorDimension>
::operator/( const ValueType & value ) const
{
  Self result;
  for( unsigned int i=0; i<TVectorDimension; i++) 
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
  vnl_vector_ref< T > vector_ref( TVectorDimension, this->GetDataPointer() );
  return vector_ref;
}
 

/**
 * Return a vnl_vector const
 */
template<class T, unsigned int TVectorDimension>
vnl_vector< T >
Vector<T, TVectorDimension>
::Get_vnl_vector( void ) const 
{
  vnl_vector< T > result(TVectorDimension);
  for(unsigned int i=0; i<TVectorDimension; i++)
  {
    result[i] = (*this)[i];
  }
  return result;
}
 

/**
 * Print content to an ostream
 */
template<class T, unsigned int TVectorDimension>
std::ostream &
operator<<(std::ostream& os,const Vector<T,TVectorDimension> & vct ) 
{
  for( unsigned int i=0; i<TVectorDimension; i++)
  {
    os <<  vct[i] << "  ";
  }
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
