/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRigid3DRegistrationTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkRigid3DRegistrationTransform_txx
#define _itkRigid3DRegistrationTransform_txx

#include <itkExceptionObject.h>
#include "itkRigid3DRegistrationTransform.h"


namespace itk
{

/**
 * Constructor
 */
template <class TScalarType,class TParameters>
Rigid3DRegistrationTransform<TScalarType,TParameters>
::Rigid3DRegistrationTransform()
{ 
  m_TranslationScale = 1.0;
}


/**
 * Constructor
 */
template <class TScalarType,class TParameters>
Rigid3DRegistrationTransform<TScalarType,TParameters>
::Rigid3DRegistrationTransform( const Self & other )
{
  m_RigidTransform   = other.m_RigidTransform;
  m_TranslationScale = other.m_TranslationScale;
}


/**
 * Assignment Operator
 */
template <class TScalarType,class TParameters>
const Rigid3DRegistrationTransform<TScalarType,TParameters> &
Rigid3DRegistrationTransform<TScalarType,TParameters>
::operator=( const Self & other )
{
  m_RigidTransformation = other.m_RigidTransformation;
  m_TranslationScale = other.m_TranslationScale;
  return *this;
}


/**
 * Transform a Point
 */
template <class TScalarType,class TParameters>
Rigid3DRegistrationTransform<TScalarType,TParameters>::PointType
Rigid3DRegistrationTransform<TScalarType,TParameters>
::Transform( const PointType & point ) const
{
  return m_RigidTransform.Transform( point );
}



/**
 * Set the transformation parameters
 */
template <class TScalarType,class TParameters>
void
Rigid3DRegistrationTransform<TScalarType,TParameters>
::SetParameters(const ParametersType & parameters )
{

  m_Parameters = parameters;
  
  typename RigidTransformType::VectorType  axis;
  typename RigidTransformType::VectorType  translation;

  unsigned int counter = 0;
  for( unsigned int e=0; e<SpaceDimension; e++)
  {
    axis[e] = m_Parameters[counter++];
  }

  const double angle = 2.0 * acos( m_Parameters[counter++] );
  
  for( unsigned int i=0; i<SpaceDimension; i++)
  {
    translation[i] = m_Parameters[counter++] * m_TranslationScale;
  }

  m_RigidTransform.Rotate3D( axis, angle );
  m_RigidTransform.SetOffset( translation );

  std::cout << "SetParameters = " << std::endl;
  std::cout << m_RigidTransform << std::endl;
}


// Compute the Jacobian of the transformation
// It follows the same order of Parameters vector 
template<class ScalarType, class TParameters>
const Rigid3DRegistrationTransform<ScalarType, TParameters>::JacobianType &
Rigid3DRegistrationTransform<ScalarType, TParameters>::
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

