/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuaternionRigidTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkQuaternionRigidTransform_txx
#define _itkQuaternionRigidTransform_txx

#include "itkQuaternionRigidTransform.h"


namespace itk
{

// Constructor with default arguments
template <class TScalarType>
QuaternionRigidTransform<TScalarType>
::QuaternionRigidTransform():Superclass(SpaceDimension,ParametersDimension) 
{
  m_Rotation = VnlQuaternionType(0,0,0,1); // axis * sin(t/2), cos(t/2)
  m_RotationMatrix = m_Rotation.rotation_matrix_transpose();
  m_Parameters.Fill(0);
  m_Parameters[3] = 1.0;
  m_Center.Fill( 0.0 );
  m_Translation.Fill( 0.0 );
}


// Print self
template<class TScalarType>
void
QuaternionRigidTransform<TScalarType>::
PrintSelf(std::ostream &os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Rotation:    " << m_Rotation    << std::endl;
  os << indent << "Center:      " << m_Center      << std::endl;
  os << indent << "Translation: " << m_Translation << std::endl;
  os << indent << "Parameters:  " << m_Parameters  << std::endl;
}


// Set rotation
template<class TScalarType>
void
QuaternionRigidTransform<TScalarType>::
SetRotation(const VnlQuaternionType &rotation )
{
  m_Rotation        = rotation;
  VnlQuaternionType conjugateRotation = m_Rotation.conjugate();
  // this is done to compensate for the transposed representation
  // between VNL and ITK
  m_RotationMatrix  = conjugateRotation.rotation_matrix_transpose();

  m_RotationMatrixMTime.Modified();

  //VnlQuaternionType inverseRotation = conjugateRotation.inverse();
  //m_InverseMatrix = inverseRotation.rotation_matrix_transpose();
  return;
}


// Set the parameters in order to fit an Identity transform
template<class TScalarType>
void
QuaternionRigidTransform<TScalarType>::
SetIdentity( void ) 
{ 
  this->Superclass::SetIdentity();
  m_Rotation = VnlQuaternionType(0,0,0,1);
  m_Center.Fill( 0.0 );
  m_Translation.Fill( 0.0 );
  this->ComputeOffset();
  this->Modified();  
}



 
template<class TScalarType>
void
QuaternionRigidTransform<TScalarType>::
SetCenter( const InputPointType & center )
{
  m_Center = center;
  this->ComputeOffset();
}


// Set Parameters
template <class TScalarType>
void
QuaternionRigidTransform<TScalarType>
::SetParameters( const ParametersType & parameters )
{
  VnlQuaternionType  quaternion;
  OutputVectorType   translation; 

  m_Parameters = parameters;

  // Transfer the quaternion part
  unsigned int par = 0;
  double norm = 0;

  for(unsigned int j=0; j < 4; j++) 
    {
    quaternion[j] = parameters[par];
    norm += vnl_math_sqr( quaternion[j] );
    ++par;
    }

  // Transfer the constant part
  for(unsigned int i=0; i < SpaceDimension; i++) 
    {
    translation[i] = parameters[par];
    ++par;
    }

  this->SetRotation( quaternion );
  this->SetTranslation( translation );

  this->ComputeOffset();

}



// Set Parameters
template <class TScalarType>
const 
typename QuaternionRigidTransform<TScalarType>::ParametersType & 
QuaternionRigidTransform<TScalarType>
::GetParameters() const
{
  VnlQuaternionType  quaternion  = this->GetRotation();
  OutputVectorType   translation = this->GetTranslation(); 

  // Transfer the quaternion part
  unsigned int par = 0;

  for(unsigned int j=0; j < 4; j++) 
    {
    m_Parameters[par] = quaternion[j];
    ++par;
    }

  // Transfer the constant part
  for(unsigned int i=0; i < SpaceDimension; i++) 
    {
    m_Parameters[par] = translation[i];
    ++par;
    }

  return m_Parameters;

}


// Get parameters
template<class TScalarType>
const typename QuaternionRigidTransform<TScalarType>::JacobianType &
QuaternionRigidTransform<TScalarType>::
GetJacobian( const InputPointType & p ) const
{

  // compute derivatives with respect to rotation
  m_Jacobian.Fill(0.0);

  const TScalarType x = p[0] - m_Center[0];
  const TScalarType y = p[1] - m_Center[1];
  const TScalarType z = p[2] - m_Center[2];

  // compute Jacobian with respect to quaternion parameters
  m_Jacobian[0][0] =   2.0 * (  m_Rotation.x() * x + m_Rotation.y() * y 
                              + m_Rotation.z() * z );
  m_Jacobian[0][1] =   2.0 * (- m_Rotation.y() * x + m_Rotation.x() * y 
                              + m_Rotation.r() * z );
  m_Jacobian[0][2] =   2.0 * (- m_Rotation.z() * x - m_Rotation.r() * y 
                              + m_Rotation.x() * z );
  m_Jacobian[0][3] = - 2.0 * (- m_Rotation.r() * x + m_Rotation.z() * y 
                              - m_Rotation.y() * z );

  m_Jacobian[1][0] = - m_Jacobian[0][1];
  m_Jacobian[1][1] =   m_Jacobian[0][0];
  m_Jacobian[1][2] =   m_Jacobian[0][3];
  m_Jacobian[1][3] = - m_Jacobian[0][2];

  m_Jacobian[2][0] = - m_Jacobian[0][2];
  m_Jacobian[2][1] = - m_Jacobian[0][3];
  m_Jacobian[2][2] =   m_Jacobian[0][0];
  m_Jacobian[2][3] =   m_Jacobian[0][1];


  // compute derivatives for the translation part
  unsigned int blockOffset = 4;  
  for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
    {
    m_Jacobian[ dim ][ blockOffset + dim ] = 1.0;
    }

  return m_Jacobian;

}
 

template<class TScalarType>
void
QuaternionRigidTransform<TScalarType>::
SetTranslation( const OutputVectorType & translation )
{
  m_Translation = translation;
  this->ComputeOffset();
}



// Compute Offset
template<class TScalarType>
void
QuaternionRigidTransform<TScalarType>::
ComputeOffset( void ) 
{

  const MatrixType & matrix = this->GetRotationMatrix();
  
  OffsetType offset;
  for(unsigned int i=0; i<SpaceDimension; i++)
    {
    offset[i] = m_Translation[i] + m_Center[i];
    for(unsigned int j=0; j<SpaceDimension; j++)
      {
      offset[i] -= matrix[i][j] * m_Center[j];
      }
    }

  this->SetOffset( offset );

}

//
template<class TScalarType>
typename QuaternionRigidTransform< TScalarType >::MatrixType
QuaternionRigidTransform<TScalarType>::
GetInverseMatrix() const
{
  // If the transform has been modified we recompute the inverse
  if(m_InverseMatrixMTime != m_RotationMatrixMTime)
    {
    VnlQuaternionType conjugateRotation = m_Rotation.conjugate();
    VnlQuaternionType inverseRotation = conjugateRotation.inverse();
    m_InverseMatrix = inverseRotation.rotation_matrix_transpose();
    m_InverseMatrixMTime = m_RotationMatrixMTime;
    }
  return m_InverseMatrix; 
}

 
} // namespace

#endif
