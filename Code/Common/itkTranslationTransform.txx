/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTranslationTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

NOTE: The code in this file is tested partly by itkTranslationTransformTest.cxx
and partly by itkImageMomentsTest.cxx.
=========================================================================*/
#ifndef _itkTranslationTransform_txx
#define _itkTranslationTransform_txx

#include "itkNumericTraits.h"
#include "itkTranslationTransform.h"


namespace itk
{

// Constructor with default arguments
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::
TranslationTransform()
{
  m_Offset.Fill( 0 );
}
 
// Constructor with explicit arguments
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::
TranslationTransform(const VectorType &offset)
{
  m_Offset = offset;
}

    
// Copy Constructor
template <class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>
::TranslationTransform( const TranslationTransform<ScalarType, NDimensions> & other )
{
  m_Offset    = other.m_Offset;
}

// Destructor
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::
~TranslationTransform()
{
 return;
}


// Assignment Operator
template <class ScalarType, unsigned int NDimensions>
const TranslationTransform<ScalarType, NDimensions> &
TranslationTransform<ScalarType, NDimensions>
::operator=( const Self & other )
{
  m_Offset   = other.m_Offset;
  return *this;
}


// Print self
template<class ScalarType, unsigned int NDimensions>
std::ostream &
TranslationTransform<ScalarType, NDimensions>::
PrintSelf(std::ostream &s) const
{
  s << m_Offset << std::endl;
  return s;
}


// Compose with another affine transformation
template<class ScalarType, unsigned int NDimensions>
void
TranslationTransform<ScalarType, NDimensions>::
Compose(const Self &other, bool )
{
  m_Offset += m_Offset;
  return;
}


// Compose with a translation
template<class ScalarType, unsigned int NDimensions>
void
TranslationTransform<ScalarType, NDimensions>::
Translate(const VectorType &offset, bool pre)
{
  m_Offset += offset;
  return;
}



// Transform a point
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::PointType
TranslationTransform<ScalarType, NDimensions>::
Transform(const PointType &point) const 
{
  return point + m_Offset;
}


// Transform a vector
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::VectorType
TranslationTransform<ScalarType, NDimensions>::
Transform(const VectorType &vect) const 
{
  return  vect;
}


// Transform a vnl_vector_fixed
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::VnlVectorType
TranslationTransform<ScalarType, NDimensions>::
Transform(const VnlVectorType &vect) const 
{
  return  vect;
}


// Transform a CovariantVector
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::CovariantVectorType
TranslationTransform<ScalarType, NDimensions>::
Transform(const CovariantVectorType &vect) const 
{
  return  vect;
}



// Back transform a point
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::PointType
TranslationTransform<ScalarType, NDimensions>::
BackTransform(const PointType &point) const {
  return point - m_Offset;
}

// Back transform a vector
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::VectorType
TranslationTransform<ScalarType, NDimensions>::
BackTransform(const VectorType &vect ) const 
{
    return  vect;
}

// Back transform a vnl_vector
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::VnlVectorType
TranslationTransform<ScalarType, NDimensions>::
BackTransform(const VnlVectorType &vect ) const 
{
    return  vect;
}


// Back Transform a CovariantVector
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>::CovariantVectorType
TranslationTransform<ScalarType, NDimensions>::
BackTransform(const CovariantVectorType &vect) const 
{
  return vect;
}



// Create and return an inverse transformation
template<class ScalarType, unsigned int NDimensions>
TranslationTransform<ScalarType, NDimensions>
TranslationTransform<ScalarType, NDimensions>::
Inverse()
{
  Self result;
  result.m_Offset   = - m_Offset;
  return result;
}


  
} // namespace

#endif
