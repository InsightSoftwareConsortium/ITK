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
template <class TScalarType,unsigned int NDimensions, class TParameters>
AffineRegistrationTransform<TScalarType,NDimensions,TParameters>
::AffineRegistrationTransform()
{ 

}


/**
 * Constructor
 */
template <class TScalarType,unsigned int NDimensions, class TParameters>
AffineRegistrationTransform<TScalarType,NDimensions,TParameters>
::AffineRegistrationTransform( const Self & other )
{
  m_AffineTransform = other.m_AffineTransform;
}


/**
 * Assignment Operator
 */
template <class TScalarType,unsigned int NDimensions, class TParameters>
const AffineRegistrationTransform<TScalarType,NDimensions,TParameters> &
AffineRegistrationTransform<TScalarType,NDimensions,TParameters>
::operator=( const Self & other )
{
  m_AffineTransformation = other.m_AffineTransformation;
  return *this;
}


/**
 * Transform a Point
 */
template <class TScalarType,unsigned int NDimensions, class TParameters>
AffineRegistrationTransform<TScalarType,NDimensions,TParameters>::PointType
AffineRegistrationTransform<TScalarType,NDimensions,TParameters>
::Transform( const PointType & point ) const
{
  return m_AffineTransform.Transform( point );
}



/**
 * Set the transformation parameters
 */
template <class TScalarType,unsigned int NDimensions, class TParameters>
void
AffineRegistrationTransform<TScalarType,NDimensions,TParameters>
::SetParameters(const ParametersType & parameters )
{

  m_Parameters = parameters;
  
  typename AffineTransformType::MatrixType  linear;
  typename AffineTransformType::VectorType  constant;
  
  // Transfer the linear part
  unsigned int par = 0;

  for(unsigned int row=0; row<NDimensions; row++) 
  {
    for(unsigned int col=0; col<NDimensions; col++) 
    {
      linear[row][col] = m_Parameters[par];
      ++par;
    }
  }

  // Transfer the constant part
  for(unsigned int i=0; i<NDimensions; i++) 
  {
    constant[i] = m_Parameters[par];
    ++par;
  }

  m_AffineTransform.SetMatrix( linear );
  m_AffineTransform.SetOffset( constant );

}


// Compute the Jacobian of the transformation
// It follows the same order of Parameters vector 
template<class ScalarType, unsigned int NDimensions, class TParameters>
const AffineRegistrationTransform<ScalarType, NDimensions,TParameters>::JacobianType &
AffineRegistrationTransform<ScalarType, NDimensions,TParameters>::
GetJacobian( const PointType & p ) const
{
  
  // The Jacobian of the affine transform is composed of
  // subblocks of diagonal matrices, each one of them having
  // a constant value in the diagonal.

  m_Jacobian.Fill( 0.0 );

  unsigned int blockOffset = 0;
  
  for(unsigned int block=0; block < SpaceDimension; block++) 
  {
    for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
    {
       m_Jacobian[ block ][ blockOffset + dim ] = p[dim];
    }

    blockOffset += SpaceDimension;

  }

  for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
  {
     m_Jacobian[ dim ][ blockOffset + dim ] = 1.0;
  }

  return m_Jacobian;

}



} // end namespace itk

#endif

