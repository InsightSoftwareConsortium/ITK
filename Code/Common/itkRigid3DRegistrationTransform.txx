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
#include "itkMatrix.h"


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
  m_Transform   = other.m_Transform;
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
  m_Transformation = other.m_Transformation;
  m_TranslationScale = other.m_TranslationScale;
  return *this;
}

/**
 * Set Center of the Rotation
 */
template <class TScalarType,class TParameters>
void
Rigid3DRegistrationTransform<TScalarType,TParameters>
::SetCenterOfRotation(const double* center)
{
  m_Transform.SetCenterOfRotation(center);
}
  
  
/**
 * Get Center of the Rotation
 */
template <class TScalarType,class TParameters>
const double* 
Rigid3DRegistrationTransform<TScalarType,TParameters>
::GetCenterOfRotation(void) 
{
  return m_Transform.GetCenterOfRotation();
}

/**
 * Transform a Point
 */
template <class TScalarType,class TParameters>
Rigid3DRegistrationTransform<TScalarType,TParameters>::OutputPointType
Rigid3DRegistrationTransform<TScalarType,TParameters>
::Transform( const InputPointType & point ) const
{
  return m_Transform.TransformPoint( point );
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
Rigid3DRegistrationTransform<TScalarType,TParameters>
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

  m_Transform.Rotate( axis, angle );
  m_Transform.SetOffset( translation );

}


/**
 * Set the transformation parameters
 * as Euler's angle (alpha,beta,gamma)
 * and a translation (x,y,z)
 * Note: To keep the same parameters space dimension
 * (ie 7) the 4th parameter is ignored.
 * the 3 first parameters are angles and the 3 last 
 * ones are translation.
 *
 */
template <class TScalarType,class TParameters>
void
Rigid3DRegistrationTransform<TScalarType,TParameters>
::SetEulerParameters(const ParametersType & parameters )
{
  m_Parameters = parameters;
  
  typename TransformType::VectorType  translation;

  // get euler's angles
  unsigned int counter = 0;
  double alpha= m_Parameters[counter++];
  double beta = m_Parameters[counter++];
  double gamma= m_Parameters[counter++];


  // fourth parameter ignored to keep the number of 
  // parameters seven.
  counter++;
  
  // get the translation
  for( unsigned int i=0; i<SpaceDimension; i++)
  {
    translation[i] = m_Parameters[counter++] * m_TranslationScale;
  }
  m_Transform.SetEulerAngles( alpha,beta,gamma );
  m_Transform.SetOffset( translation );

}

/**
 * Return the rotation matrix of the tranformation
 */
template<class ScalarType, class TParameters>
const Rigid3DRegistrationTransform<ScalarType, TParameters>::RotationMatrixType &
Rigid3DRegistrationTransform<ScalarType, TParameters>::
GetRotationMatrix( void ) const
{
  return m_Transform.GetRotationMatrix();
}


// Compute the Jacobian of the transformation
// It follows the same order of Parameters vector 
template<class ScalarType, class TParameters>
const Rigid3DRegistrationTransform<ScalarType, TParameters>::JacobianType &
Rigid3DRegistrationTransform<ScalarType, TParameters>::
GetJacobian( const InputPointType & p ) const
{

  // reset Jacobian
  m_Jacobian.Fill( 0.0 );

  // compute derivatives with respect to rotation
  typename TransformType::VnlQuaternionType Q =
    m_Transform.GetRotation();

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

