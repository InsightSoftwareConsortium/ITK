/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAffineRegistrationTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkAffineRegistrationTransform_txx
#define _itkAffineRegistrationTransform_txx

#include <itkExceptionObject.h>

namespace itk
{

/**
 * Constructor
 */
template <unsigned int NDimensions>
AffineRegistrationTransform<NDimensions>
::AffineRegistrationTransform()
{
  m_Parameters->Reserve( NDimensions * ( NDimensions + 1 ) );
}


/**
 * Constructor
 */
template <unsigned int NDimensions>
AffineRegistrationTransform<NDimensions>
::AffineRegistrationTransform( const Self & other )
{
  m_AffineTransform = other.m_AffineTransform;
}


/**
 * Assignment Operator
 */
template <unsigned int NDimensions>
const AffineRegistrationTransform<NDimensions> &
AffineRegistrationTransform<NDimensions>
::operator=( const Self & other )
{
  m_AffineTransform = other.m_AffineTransform;
  return *this;
}


/**
 * Transform a Point
 */
template <unsigned int NDimensions>
AffineRegistrationTransform<NDimensions>::PointType
AffineRegistrationTransform<NDimensions>
::Transform(const PointType & point )
{
  return m_AffineTransform.TransformPoint ( point );
}




/**
 * Transform a Vector
 */
template <unsigned int NDimensions>
AffineRegistrationTransform<NDimensions>::VectorType
AffineRegistrationTransform<NDimensions>
::Transform(const VectorType & vector )
{
  return m_AffineTransform.TransformVector( vector );
}


/**
 * Inverse Transform a Point
 */
template <unsigned int NDimensions>
AffineRegistrationTransform<NDimensions>::PointType
AffineRegistrationTransform<NDimensions>
::InverseTransform(const PointType & point )
{
  return m_AffineTransform.BackTransformPoint( point );
}



/**
 * Inverse Transform a Vector
 */
template <unsigned int NDimensions>
AffineRegistrationTransform<NDimensions>::VectorType
AffineRegistrationTransform<NDimensions>
::InverseTransform(const VectorType & vector )
{
  return m_AffineTransform.BackTransformVector( vector );
}




/**
 * Set the transformation parameters
 */
template <unsigned int NDimensions>
void
AffineRegistrationTransform<NDimensions>
::SetParameters(const ParametersType * parameters )
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

  
  typename AffineTransformType::MatrixType linear;
  typename AffineTransformType::VectorType constant;
  
  ParametersType::ConstIterator pit = m_Parameters->Begin();

  // Transfer the linear part
  for(unsigned int row=0; row<NDimensions; row++) 
  {
    for(unsigned int col=0; col<NDimensions; col++) 
    {
      linear(row,col) = pit.Value();
      ++pit;
    }
  }

  // Transfer the constant part
  for(unsigned int i=0; i<NDimensions; i++) 
  {
    constant[i] = pit.Value();
    ++pit;
  }

  m_AffineTransform.SetMatrix( linear);
  m_AffineTransform.SetOffset( constant );

}

} // end namespace itk

#endif
