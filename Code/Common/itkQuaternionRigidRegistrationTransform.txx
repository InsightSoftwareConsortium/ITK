/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuaternionRigidRegistrationTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkQuaternionRigidRegistrationTransform_txx
#define _itkQuaternionRigidRegistrationTransform_txx

#include <itkExceptionObject.h>
#include "itkQuaternionRigidRegistrationTransform.h"
#include "itkMatrix.h"


namespace itk
{

/**
 * Constructor
 */
template <class TScalarType,class TParameters>
QuaternionRigidRegistrationTransform<TScalarType,TParameters>
::QuaternionRigidRegistrationTransform()
{ 
  m_TranslationScale = 1.0;
}


/**
 * Constructor
 */
template <class TScalarType,class TParameters>
QuaternionRigidRegistrationTransform<TScalarType,TParameters>
::QuaternionRigidRegistrationTransform( const Self & other )
{
  m_RigidTransform   = other.m_RigidTransform;
  m_TranslationScale = other.m_TranslationScale;
}


/**
 * Assignment Operator
 */
template <class TScalarType,class TParameters>
const QuaternionRigidRegistrationTransform<TScalarType,TParameters> &
QuaternionRigidRegistrationTransform<TScalarType,TParameters>
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
QuaternionRigidRegistrationTransform<TScalarType,TParameters>::OutputPointType
QuaternionRigidRegistrationTransform<TScalarType,TParameters>
::Transform( const InputPointType & point ) const
{
  return m_RigidTransform.Transform( point );
}



/**
 * Set the transformation parameters.
 * The first 4 parameters are the components of the quaternion.
 * The last 3 parameters are the translations.
 *
 * Note that the parameters for the quaternion are normalize
 * before being used to update the internal transformation method.
 *
 */
template <class TScalarType,class TParameters>
void
QuaternionRigidRegistrationTransform<TScalarType,TParameters>
::SetParameters(const ParametersType & parameters )
{

  m_Parameters = parameters;
  unsigned int counter = 0;

  // get the quaternion
  typename RigidTransformType::VnlQuaternionType  quaternion;
  for( unsigned int i=0; i < 4; i++)
  {
    quaternion[i] = m_Parameters[counter++];
  }

  // normalize the quaternion
  m_Magnitude = quaternion.magnitude();
  if( m_Magnitude ) quaternion.normalize();


  // get the translation
  typename RigidTransformType::VectorType  translation;
  for( unsigned int i=0; i<SpaceDimension; i++)
  {
    translation[i] = m_Parameters[counter++] * m_TranslationScale;
  }

  m_RigidTransform.SetRotation( quaternion );
  m_RigidTransform.SetOffset( translation );

//  std::cout << "SetParameters = " << std::endl;
//  std::cout << m_RigidTransform << std::endl;
}


/**
 * Compute the Jacobian of the transformation.
 * The output is a 3 x 7 matrix representing d(x_i)/d(p_j),
 * where x_i is the i-th spatial coordindate and p_j
 * is the j-th parameters.
 *
 */
template<class ScalarType, class TParameters>
const QuaternionRigidRegistrationTransform<ScalarType, TParameters>::JacobianType &
QuaternionRigidRegistrationTransform<ScalarType, TParameters>::
GetJacobian( const InputPointType & p ) const
{

  // compute derivatives with respect to rotation
  typename RigidTransformType::VnlQuaternionType Q =
    m_RigidTransform.GetRotation();

  m_Jacobian.Fill(0.0);

  // compute Jacobian with respect to quaternion parameters
  m_Jacobian[0][0] = 2.0 * (  Q.x() * p[0] + Q.y() * p[1] + Q.z() * p[2]);
  m_Jacobian[0][1] = 2.0 * (- Q.y() * p[0] + Q.x() * p[1] - Q.r() * p[2]);
  m_Jacobian[0][2] = 2.0 * (- Q.z() * p[0] + Q.r() * p[1] + Q.x() * p[2]);
  m_Jacobian[0][3] = 2.0 * (  Q.r() * p[0] + Q.z() * p[1] - Q.y() * p[2]);

  m_Jacobian[1][0] = - m_Jacobian[0][1];
  m_Jacobian[1][1] =   m_Jacobian[0][0];
  m_Jacobian[1][2] = - m_Jacobian[0][3];
  m_Jacobian[1][3] =   m_Jacobian[0][2];

  m_Jacobian[2][0] = - m_Jacobian[0][2];
  m_Jacobian[2][1] =   m_Jacobian[0][3];
  m_Jacobian[2][2] =   m_Jacobian[0][0];
  m_Jacobian[2][3] = - m_Jacobian[0][1];

  // scale quaternion parameters with m_Magnitude
  for( unsigned int i = 0; i < 4; i++ )
    {
    m_Jacobian.GetVnlMatrix().scale_column( i, m_Magnitude );
    }
  
  // compute derivatives for the translation part
  unsigned int blockOffset = 4;
  for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
  {
     m_Jacobian[ dim ][ blockOffset + dim ] = m_TranslationScale;
  }
  
  return m_Jacobian;

}



} // end namespace itk

#endif

