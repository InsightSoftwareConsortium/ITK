/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRigid3DPerspectiveRegistrationTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkRigid3DPerspectiveRegistrationTransform_txx
#define _itkRigid3DPerspectiveRegistrationTransform_txx

#include <itkExceptionObject.h>
#include "itkRigid3DPerspectiveRegistrationTransform.h"
#include "itkMatrix.h"


namespace itk
{

/**
 * Constructor
 */
template <class TScalarType,class TParameters>
Rigid3DPerspectiveRegistrationTransform<TScalarType,TParameters>
::Rigid3DPerspectiveRegistrationTransform()
{ 
  m_TranslationScale = 1.0;
}


/**
 * Constructor
 */
template <class TScalarType,class TParameters>
Rigid3DPerspectiveRegistrationTransform<TScalarType,TParameters>
::Rigid3DPerspectiveRegistrationTransform( const Self & other )
{
  m_RigidTransform   = other.m_RigidTransform;
  m_TranslationScale = other.m_TranslationScale;
}


/**
 * Assignment Operator
 */
template <class TScalarType,class TParameters>
const Rigid3DPerspectiveRegistrationTransform<TScalarType,TParameters> &
Rigid3DPerspectiveRegistrationTransform<TScalarType,TParameters>
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
Rigid3DPerspectiveRegistrationTransform<TScalarType,TParameters>::OutputPointType
Rigid3DPerspectiveRegistrationTransform<TScalarType,TParameters>
::Transform( const InputPointType & point ) const
{
  return m_Transform.Transform( point );
}



/**
 * Set the transformation parameters
 * 
 * \todo  Verify if the parameters should be sent as 
 * a list of 4 components or as an axis and angle.
 * Eventually consider to use screws for the total 
 * transformation.
 *
 */
template <class TScalarType,class TParameters>
void
Rigid3DPerspectiveRegistrationTransform<TScalarType,TParameters>
::SetParameters(const ParametersType & parameters )
{

  m_Parameters = parameters;
  
  typename TransformType::VectorType  axis;
  typename TransformType::VectorType  translation;

  // get the axis of rotation
  unsigned int counter = 0;
  for( unsigned int e=0; e<SpaceDimension; e++)
  {
    axis[e] = m_Parameters[counter++];
  }

  // normalize the axis
  m_AxisMagnitude = axis.GetNorm();

  if( m_AxisMagnitude ) 
   {
   axis /= m_AxisMagnitude;
   }

  // get the angle
  const double angle = m_Parameters[counter++];
  m_SinHalfAngle = sin( angle / 2.0 );
  m_CosHalfAngle = cos( angle / 2.0 );

  // compute derivatives of the quaternion components 
  // with respect to the actual parameters
  double cosFactor = 0.5 * m_CosHalfAngle;
  double sinFactor = m_SinHalfAngle * m_AxisMagnitude * m_TranslationScale;
  m_dQ_dParameters.Fill(0.0);

  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    m_dQ_dParameters[i][i] = sinFactor;
    m_dQ_dParameters[i][3] = cosFactor * axis[i];
    }
  m_dQ_dParameters[3][3] = -0.5 * m_SinHalfAngle;

  
  // get the translation
  for( unsigned int i=0; i<SpaceDimension; i++)
  {
    translation[i] = m_Parameters[counter++] * m_TranslationScale;
  }

  TransformType::VnlQuaternionType rotation( axis.Get_vnl_vector(), angle );
  
  m_Transform.SetRotation( rotation );
  m_Transform.SetOffset( translation );

//  std::cout << "SetParameters = " << std::endl;
//  std::cout << m_RigidTransform << std::endl;
}


// Compute the Jacobian of the transformation
// It follows the same order of Parameters vector 
template<class ScalarType, class TParameters>
const Rigid3DPerspectiveRegistrationTransform<ScalarType, TParameters>::JacobianType &
Rigid3DPerspectiveRegistrationTransform<ScalarType, TParameters>::
GetJacobian( const InputPointType & p ) const
{

  // reset Jacobian
  m_Jacobian.Fill( 0.0 );

  // compute derivatives with respect to rotation
  typename TransformType::VnlQuaternionType Q =
    m_RigidTransform.GetRotation();

  // compute Jacobian with respect to quaternion parameters
  vnl_matrix_fixed<double,3,4> dX_dQ;

  dX_dQ[0][0] = 2.0 * (  Q.x() * p[0] + Q.y() * p[1] + Q.z() * p[2]);
  dX_dQ[0][1] = 2.0 * (- Q.y() * p[0] + Q.x() * p[1] - Q.r() * p[2]);
  dX_dQ[0][2] = 2.0 * (- Q.z() * p[0] + Q.r() * p[1] + Q.x() * p[2]);
  dX_dQ[0][3] = 2.0 * (  Q.r() * p[0] + Q.z() * p[1] - Q.y() * p[2]);

  dX_dQ[1][0] = - dX_dQ[0][1];
  dX_dQ[1][1] =   dX_dQ[0][0];
  dX_dQ[1][2] = - dX_dQ[0][3];
  dX_dQ[1][3] =   dX_dQ[0][2];

  dX_dQ[2][0] = - dX_dQ[0][2];
  dX_dQ[2][1] =   dX_dQ[0][3];
  dX_dQ[2][2] =   dX_dQ[0][0];
  dX_dQ[2][3] = - dX_dQ[0][1];

  // compute Jacobian with respect to actual parameters
  vnl_matrix_fixed<double,3,4> dX_dParameters = 
		dX_dQ * m_dQ_dParameters.GetVnlMatrix();

  m_Jacobian.GetVnlMatrix().update( dX_dParameters, 0, 0 );

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

