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
#include "itkAffineRegistrationTransform.h"


namespace itk
{

/**
 * Constructor
 */
template <class TScalarType,unsigned int NDimensions>
AffineRegistrationTransform<TScalarType,NDimensions>
::AffineRegistrationTransform()
{ 
  m_Parameters = ParametersType::New();
  m_Parameters->Reserve(ParametersDimension);
}


/**
 * Constructor
 */
template <class TScalarType,unsigned int NDimensions>
AffineRegistrationTransform<TScalarType,NDimensions>
::AffineRegistrationTransform( const Self & other )
{
  m_AffineTransform = other.m_AffineTransform;
}


/**
 * Assignment Operator
 */
template <class TScalarType,unsigned int NDimensions>
const AffineRegistrationTransform<TScalarType,NDimensions> &
AffineRegistrationTransform<TScalarType,NDimensions>
::operator=( const Self & other )
{
  m_AffineTransformation = other.m_AffineTransformation;
  return *this;
}


/**
 * Transform a Point
 */
template <class TScalarType,unsigned int NDimensions>
AffineRegistrationTransform<TScalarType,NDimensions>::PointType
AffineRegistrationTransform<TScalarType,NDimensions>
::Transform( PointType & point )
{
  return m_AffineTransform.Transform( point );
}



/**
 * Set the transformation parameters
 */
template <class TScalarType,unsigned int NDimensions>
void
AffineRegistrationTransform<TScalarType,NDimensions>
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

  
  typename AffineTransformType::MatrixType linear;
  typename AffineTransformType::VectorType constant;
  
  ParametersType::ConstIterator pit = m_Parameters->Begin();

  // Transfer the linear part
  for(unsigned int row=0; row<NDimensions; row++) 
  {
    for(unsigned int col=0; col<NDimensions; col++) 
    {
      linear[row][col] = pit.Value();
      ++pit;
    }
  }

  // Transfer the constant part
  for(unsigned int i=0; i<NDimensions; i++) 
  {
    constant[i] = pit.Value();
    ++pit;
  }

  m_AffineTransform.SetMatrix( linear );
  m_AffineTransform.SetOffset( constant );

}


// Compute the Jacobian of the transformation
// It follows the same order of Parameters vector 
template<class ScalarType, int NDimensions>
const AffineRegistrationTransform<ScalarType, NDimensions>::JacobianType &
AffineRegistrationTransform<ScalarType, NDimensions>::
GetJacobian( const PointType & p ) const
{
  
  // The Jacobian of the affine transform is composed of
  // subblocks of diagonal matrices, each one of them having
  // a constant value in the diagonal.

  m_Jacobian.Fill( 0.0 );

  unsigned int blockOffset = 0;
  
  for(unsigned int block=0; block < SpaceDimension; block++) 
  {
    ScalarType diagonalValue = p[block];

    for(unsigned int dim=0; dim < SpaceDimension; dim ) 
    {
       m_Jacobian[ blockOffset + dim ][ dim ] = diagonalValue;
    }

    blockOffset += SpaceDimension;

  }

  for(unsigned int dim=0; dim < SpaceDimension; dim ) 
  {
     m_Jacobian[ blockOffset + dim ][ dim ] = 1.0;
  }

  return m_Jacobian;

}



} // end namespace itk

#endif

