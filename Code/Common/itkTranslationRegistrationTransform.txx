/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: 
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#ifndef _itkTranslationRegistrationTransform_txx
#define _itkTranslationRegistrationTransform_txx

#include <itkExceptionObject.h>
#include "itkTranslationRegistrationTransform.h"


namespace itk
{

/**
 * Constructor
 */
template <class TScalarType,unsigned int NDimensions>
TranslationRegistrationTransform<TScalarType,NDimensions>
::TranslationRegistrationTransform()
{ 
  m_Parameters = ParametersType::New();
  m_Parameters->Reserve(ParametersDimension);
}


/**
 * Constructor
 */
template <class TScalarType,unsigned int NDimensions>
TranslationRegistrationTransform<TScalarType,NDimensions>
::TranslationRegistrationTransform( const Self & other )
{
  m_TranslationTransform = other.m_TranslationTransform;
}


/**
 * Assignment Operator
 */
template <class TScalarType,unsigned int NDimensions>
const TranslationRegistrationTransform<TScalarType,NDimensions> &
TranslationRegistrationTransform<TScalarType,NDimensions>
::operator=( const Self & other )
{
  m_TranslationTransform = other.m_TranslationTransform;
  return *this;
}


/**
 * Transform a Point
 */
template <class TScalarType,unsigned int NDimensions>
TranslationRegistrationTransform<TScalarType,NDimensions>::PointType
TranslationRegistrationTransform<TScalarType,NDimensions>
::Transform( PointType & point )
{
  return m_TranslationTransform.Transform( point );
}


/**
 * Set the transformation parameters
 */
template <class TScalarType,unsigned int NDimensions>
void
TranslationRegistrationTransform<TScalarType,NDimensions>
::SetParameters(const ParametersPointer & parameters )
{
  if( parameters->Size() != m_Parameters->Size() )
  {
    throw ExceptionObject();
  }
  
  // Copy Parameters Vector
  ParametersType::ConstIterator it = parameters->Begin();
  ParametersType::Iterator      ot = m_Parameters->Begin();
  while( it != parameters->End() )
  {
    ot.Value() = it.Value();
    ++it;
    ++ot;
  }

  
  
  typename TranslationTransformType::VectorType constant;
  
  ParametersType::ConstIterator pit = m_Parameters->Begin();

  // Transfer the constant part
  for(unsigned int i=0; i<NDimensions; i++) 
  {
    constant[i] = pit.Value();
    ++pit;
  }

  m_TranslationTransform.SetOffset( constant );

}
 

} // end namespace itk

#endif