/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTranslationRegistrationTransform.txx
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
template <class TScalarType,unsigned int NDimensions, class TParameters>
TranslationRegistrationTransform<TScalarType,NDimensions,TParameters>
::TranslationRegistrationTransform()
{ 

}


/**
 * Constructor
 */
template <class TScalarType,unsigned int NDimensions, class TParameters>
TranslationRegistrationTransform<TScalarType,NDimensions,TParameters>
::TranslationRegistrationTransform( const Self & other )
{
  m_TranslationTransform = other.m_TranslationTransform;
}


/**
 * Assignment Operator
 */
template <class TScalarType,unsigned int NDimensions, class TParameters>
const TranslationRegistrationTransform<TScalarType,NDimensions,TParameters> &
TranslationRegistrationTransform<TScalarType,NDimensions,TParameters>
::operator=( const Self & other )
{
  m_TranslationTransformation = other.m_TranslationTransformation;
  return *this;
}


/**
 * Transform a Point
 */
template <class TScalarType,unsigned int NDimensions, class TParameters>
TranslationRegistrationTransform<TScalarType,NDimensions,TParameters>::PointType
TranslationRegistrationTransform<TScalarType,NDimensions,TParameters>
::Transform( const PointType & point ) const
{
  return m_TranslationTransform.Transform( point );
}



/**
 * Set the transformation parameters
 */
template <class TScalarType,unsigned int NDimensions, class TParameters>
void
TranslationRegistrationTransform<TScalarType,NDimensions,TParameters>
::SetParameters(const ParametersType & parameters )
{

  m_Parameters = parameters;
  
  typename TranslationTransformType::VectorType  constant;
  
  // Transfer the constant part
  unsgined int par = 0;
  for(unsigned int i=0; i<NDimensions; i++) 
  {
    constant[i] = m_Parameters[par];
    ++par;
  }

  m_TranslationTransform.SetOffset( constant );

}


// Compute the Jacobian of the transformation
// It follows the same order of Parameters vector 
template<class ScalarType, unsigned int NDimensions, class TParameters>
const TranslationRegistrationTransform<ScalarType, NDimensions,TParameters>::JacobianType &
TranslationRegistrationTransform<ScalarType, NDimensions,TParameters>::
GetJacobian( const PointType & p ) const
{
  
  // The Jacobian of the Translation transform is an identity matrix
  m_Jacobian.SetIdentity();
  return m_Jacobian;

}



} // end namespace itk

#endif

