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

//  axis.Set_vnl_vector( axis.Get_vnl_vector().normalize() );

//  const double angle = 2.0 * acos( m_Parameters[counter++] );
  const double angle = m_Parameters[counter++];
  
  for( unsigned int i=0; i<SpaceDimension; i++)
  {
    translation[i] = m_Parameters[counter++] * m_TranslationScale;
  }

  m_RigidTransform.Rotate( axis, angle );
  m_RigidTransform.SetOffset( translation );

//  std::cout << "SetParameters = " << std::endl;
//  std::cout << m_RigidTransform << std::endl;
}


// Compute the Jacobian of the transformation
// It follows the same order of Parameters vector 
template<class ScalarType, class TParameters>
const Rigid3DRegistrationTransform<ScalarType, TParameters>::JacobianType &
Rigid3DRegistrationTransform<ScalarType, TParameters>::
GetJacobian( const PointType & p ) const
{

  // compute derivatives with respect to rotation
  typename RigidTransformType::VnlQuaternionType Q =
    m_RigidTransform.GetRotation();

  vnl_matrix_fixed<double,3,4> dXdQ;

  // compute dX / dQ
  dXdQ(0,0) =   Q.x() * p[0] + Q.y() * p[1] + Q.z() * p[2];
  dXdQ(0,1) = - Q.y() * p[0] + Q.x() * p[1] - Q.r() * p[2];
  dXdQ(0,2) = - Q.z() * p[0] + Q.r() * p[1] + Q.x() * p[2];
  dXdQ(0,3) =   Q.r() * p[0] + Q.z() * p[1] - Q.y() * p[2];

  dXdQ(1,0) =   Q.y() * p[0] - Q.x() * p[1] - Q.r() * p[2];
  dXdQ(1,1) =   Q.x() * p[0] + Q.y() * p[1] + Q.z() * p[2];
  dXdQ(1,2) = - Q.r() * p[0] - Q.z() * p[1] + Q.y() * p[2];
  dXdQ(1,3) = - Q.z() * p[0] + Q.r() * p[1] - Q.x() * p[2];

  dXdQ(2,0) =   Q.z() * p[0] - Q.r() * p[1] - Q.x() * p[2];
  dXdQ(2,1) =   Q.r() * p[0] + Q.z() * p[1] - Q.y() * p[2];
  dXdQ(2,2) =   Q.x() * p[0] + Q.y() * p[1] + Q.z() * p[2];
  dXdQ(2,3) =   Q.y() * p[0] - Q.x() * p[1] + Q.r() * p[2];

  dXdQ *= 2.0;

  // compute dQ / dParameters
  vnl_matrix_fixed<double,4,4> dQdP;
  dQdP.fill( 0.0 );

  dQdP(0,0) = sin( m_Parameters[3] / 2 );
  dQdP(1,1) = dQdP(0,0);
  dQdP(2,2) = dQdP(0,0);

  dQdP(0,3) = 0.5 * cos( m_Parameters[3] / 2 );
  dQdP(1,3) = dQdP(0,3);
  dQdP(2,3) = dQdP(0,3);
  dQdP(3,3) = - 0.5 * dQdP(0,0);

  // compute dX / dParameters
  vnl_matrix_fixed<double,3,4> dXdP = dXdQ * dQdP;

  m_Jacobian.Fill(0.0);
  m_Jacobian.GetVnlMatrix().update( dXdP, 0, 0 );


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

