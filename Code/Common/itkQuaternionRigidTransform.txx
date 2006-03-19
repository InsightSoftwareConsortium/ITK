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
::QuaternionRigidTransform() :
  Superclass(SpaceDimension, ParametersDimension) 
{
  m_Rotation = VnlQuaternionType(0,0,0,1); // axis * vcl_sin(t/2), vcl_cos(t/2)
}

// Constructor with default arguments
template<class TScalarType>
QuaternionRigidTransform<TScalarType>::
QuaternionRigidTransform( unsigned int outputSpaceDimension, 
                          unsigned int parametersDimension ) :
  Superclass(outputSpaceDimension, parametersDimension)
{
  m_Rotation = VnlQuaternionType(0,0,0,1); // axis * vcl_sin(t/2), vcl_cos(t/2)
}



// Constructor with explicit arguments
template<class TScalarType>
QuaternionRigidTransform<TScalarType>::
QuaternionRigidTransform( const MatrixType & matrix,
                          const OutputVectorType & offset ) :
  Superclass(matrix, offset)
{
  this->ComputeMatrixParameters();
}


// Print self
template<class TScalarType>
void
QuaternionRigidTransform<TScalarType>::
PrintSelf(std::ostream &os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Rotation:    " << m_Rotation    << std::endl;
}


// Set rotation
template<class TScalarType>
void
QuaternionRigidTransform<TScalarType>::
SetRotation(const VnlQuaternionType &rotation )
{
  m_Rotation        = rotation;

  this->ComputeMatrix();

  return;
}


// Set the parameters in order to fit an Identity transform
template<class TScalarType>
void
QuaternionRigidTransform<TScalarType>::
SetIdentity( void ) 
{ 
  m_Rotation = VnlQuaternionType(0,0,0,1);
  this->Superclass::SetIdentity();
}


// Set Parameters
template <class TScalarType>
void
QuaternionRigidTransform<TScalarType>
::SetParameters( const ParametersType & parameters )
{
  OutputVectorType   translation; 

  // Transfer the quaternion part
  unsigned int par = 0;

  for(unsigned int j=0; j < 4; j++) 
    {
    m_Rotation[j] = parameters[par];
    ++par;
    }
  this->ComputeMatrix();

  // Transfer the constant part
  for(unsigned int i=0; i < SpaceDimension; i++) 
    {
    translation[i] = parameters[par];
    ++par;
    }
  this->SetVarTranslation( translation );
  this->ComputeOffset();

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();

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
    this->m_Parameters[par] = quaternion[j];
    ++par;
    }

  // Transfer the constant part
  for(unsigned int i=0; i < SpaceDimension; i++) 
    {
    this->m_Parameters[par] = translation[i];
    ++par;
    }

  return this->m_Parameters;

}


// Get parameters
template<class TScalarType>
const typename QuaternionRigidTransform<TScalarType>::JacobianType &
QuaternionRigidTransform<TScalarType>::
GetJacobian( const InputPointType & p ) const
{

  // compute derivatives with respect to rotation
  this->m_Jacobian.Fill(0.0);

  const TScalarType x = p[0] - this->GetCenter()[0];
  const TScalarType y = p[1] - this->GetCenter()[1];
  const TScalarType z = p[2] - this->GetCenter()[2];

  // compute Jacobian with respect to quaternion parameters
  this->m_Jacobian[0][0] =   2.0 * (  m_Rotation.x() * x + m_Rotation.y() * y 
                              + m_Rotation.z() * z );
  this->m_Jacobian[0][1] =   2.0 * (- m_Rotation.y() * x + m_Rotation.x() * y 
                              + m_Rotation.r() * z );
  this->m_Jacobian[0][2] =   2.0 * (- m_Rotation.z() * x - m_Rotation.r() * y 
                              + m_Rotation.x() * z );
  this->m_Jacobian[0][3] = - 2.0 * (- m_Rotation.r() * x + m_Rotation.z() * y 
                              - m_Rotation.y() * z );

  this->m_Jacobian[1][0] = - this->m_Jacobian[0][1];
  this->m_Jacobian[1][1] =   this->m_Jacobian[0][0];
  this->m_Jacobian[1][2] =   this->m_Jacobian[0][3];
  this->m_Jacobian[1][3] = - this->m_Jacobian[0][2];

  this->m_Jacobian[2][0] = - this->m_Jacobian[0][2];
  this->m_Jacobian[2][1] = - this->m_Jacobian[0][3];
  this->m_Jacobian[2][2] =   this->m_Jacobian[0][0];
  this->m_Jacobian[2][3] =   this->m_Jacobian[0][1];


  // compute derivatives for the translation part
  unsigned int blockOffset = 4;  
  for(unsigned int dim=0; dim < SpaceDimension; dim++ ) 
    {
    this->m_Jacobian[ dim ][ blockOffset + dim ] = 1.0;
    }

  return this->m_Jacobian;

}
 
template<class TScalarType>
const typename QuaternionRigidTransform< TScalarType >::InverseMatrixType &
QuaternionRigidTransform<TScalarType>::
GetInverseMatrix() const
{
  // If the transform has been modified we recompute the inverse
  if(this->InverseMatrixIsOld())
    {
    InverseMatrixType newMatrix;
    VnlQuaternionType conjugateRotation = m_Rotation.conjugate();
    VnlQuaternionType inverseRotation = conjugateRotation.inverse();
    newMatrix = inverseRotation.rotation_matrix_transpose();
    this->SetVarInverseMatrix(newMatrix);
    }
  return this->GetVarInverseMatrix();
}

template<class TScalarType>
void
QuaternionRigidTransform<TScalarType>::
ComputeMatrix()
{
  VnlQuaternionType conjugateRotation = m_Rotation.conjugate();
  // this is done to compensate for the transposed representation
  // between VNL and ITK
  MatrixType newMatrix;
  newMatrix = conjugateRotation.rotation_matrix_transpose();
  this->SetVarMatrix(newMatrix);
}

template<class TScalarType>
void
QuaternionRigidTransform<TScalarType>::
ComputeMatrixParameters()
{
  VnlQuaternionType quat(this->GetMatrix().GetVnlMatrix());
  m_Rotation = quat;
}
 
} // namespace

#endif
