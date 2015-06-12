/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkQuaternionRigidTransform_hxx
#define itkQuaternionRigidTransform_hxx

#include "itkQuaternionRigidTransform.h"

namespace itk
{
// Constructor with default arguments
template<typename TParametersValueType>
QuaternionRigidTransform<TParametersValueType>
::QuaternionRigidTransform() :
  Superclass(ParametersDimension)
{
  m_Rotation = VnlQuaternionType(0, 0, 0, 1); // axis * std::sin(t/2),
                                              // std::cos(t/2)
}

// Constructor with default arguments
template<typename TParametersValueType>
QuaternionRigidTransform<TParametersValueType>::QuaternionRigidTransform(unsigned int parametersDimension) :
  Superclass(parametersDimension)
{
  m_Rotation = VnlQuaternionType(0, 0, 0, 1); // axis * std::sin(t/2),
                                              // std::cos(t/2)
}

// Constructor with explicit arguments
template<typename TParametersValueType>
QuaternionRigidTransform<TParametersValueType>::QuaternionRigidTransform(const MatrixType & matrix,
                                                                const OutputVectorType & offset) :
  Superclass(matrix, offset)
{
  this->ComputeMatrixParameters();
}

// Print self
template<typename TParametersValueType>
void
QuaternionRigidTransform<TParametersValueType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Rotation:    " << m_Rotation    << std::endl;
}

// Set rotation
template<typename TParametersValueType>
void
QuaternionRigidTransform<TParametersValueType>::SetRotation(const VnlQuaternionType & rotation)
{
  m_Rotation        = rotation;

  this->ComputeMatrix();
}

// Set the parameters in order to fit an Identity transform
template<typename TParametersValueType>
void
QuaternionRigidTransform<TParametersValueType>::SetIdentity(void)
{
  m_Rotation = VnlQuaternionType(0, 0, 0, 1);
  this->Superclass::SetIdentity();
}

// Set Parameters
template<typename TParametersValueType>
void
QuaternionRigidTransform<TParametersValueType>
::SetParameters(const ParametersType & parameters)
{
  OutputVectorType translation;

  // Save parameters. Needed for proper operation of TransformUpdateParameters.
  if( &parameters != &(this->m_Parameters) )
    {
    this->m_Parameters = parameters;
    }

  // Transfer the quaternion part
  unsigned int par = 0;
  for( unsigned int j = 0; j < 4; j++ )
    {
    m_Rotation[j] = parameters[par];
    ++par;
    }
  this->ComputeMatrix();
  // Transfer the constant part
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    translation[i] = parameters[par];
    ++par;
    }
  this->SetVarTranslation(translation);
  this->ComputeOffset();

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();
}

// Set Parameters
template<typename TParametersValueType>
const
typename QuaternionRigidTransform<TParametersValueType>::ParametersType
& QuaternionRigidTransform<TParametersValueType>
::GetParameters() const
  {
  VnlQuaternionType quaternion  = this->GetRotation();
  OutputVectorType  translation = this->GetTranslation();

  // Transfer the quaternion part
  unsigned int par = 0;
  for( unsigned int j = 0; j < 4; j++ )
    {
    this->m_Parameters[par] = quaternion[j];
    ++par;
    }
  // Transfer the constant part
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    this->m_Parameters[par] = translation[i];
    ++par;
    }

  return this->m_Parameters;
  }

template<typename TParametersValueType>
void
QuaternionRigidTransform<TParametersValueType>
::ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const
{
  // compute derivatives with respect to rotation
  jacobian.SetSize( 3, this->GetNumberOfLocalParameters() );
  jacobian.Fill(0.0);

  const TParametersValueType x = p[0] - this->GetCenter()[0];
  const TParametersValueType y = p[1] - this->GetCenter()[1];
  const TParametersValueType z = p[2] - this->GetCenter()[2];

  // compute Jacobian with respect to quaternion parameters
  jacobian[0][0] =   2.0 * (  m_Rotation.x() * x + m_Rotation.y() * y
                              + m_Rotation.z() * z );
  jacobian[0][1] =   2.0 * ( -m_Rotation.y() * x + m_Rotation.x() * y
                             + m_Rotation.r() * z );
  jacobian[0][2] =   2.0 * ( -m_Rotation.z() * x - m_Rotation.r() * y
                             + m_Rotation.x() * z );
  jacobian[0][3] = -2.0 * ( -m_Rotation.r() * x + m_Rotation.z() * y
                            - m_Rotation.y() * z );

  jacobian[1][0] = -jacobian[0][1];
  jacobian[1][1] =   jacobian[0][0];
  jacobian[1][2] =   jacobian[0][3];
  jacobian[1][3] = -jacobian[0][2];

  jacobian[2][0] = -jacobian[0][2];
  jacobian[2][1] = -jacobian[0][3];
  jacobian[2][2] =   jacobian[0][0];
  jacobian[2][3] =   jacobian[0][1];

  // compute derivatives for the translation part
  unsigned int blockOffset = 4;
  for( unsigned int dim = 0; dim < SpaceDimension; dim++ )
    {
    jacobian[dim][blockOffset + dim] = 1.0;
    }
}

template<typename TParametersValueType>
const typename QuaternionRigidTransform<TParametersValueType>::InverseMatrixType
& QuaternionRigidTransform<TParametersValueType>::GetInverseMatrix() const
  {
  // If the transform has been modified we recompute the inverse
  if( this->InverseMatrixIsOld() )
    {
    InverseMatrixType newMatrix;
    VnlQuaternionType conjugateRotation = m_Rotation.conjugate();
    VnlQuaternionType inverseRotation = conjugateRotation.inverse();
    newMatrix = inverseRotation.rotation_matrix_transpose();
    this->SetVarInverseMatrix(newMatrix);
    }
  return this->GetVarInverseMatrix();
  }

template<typename TParametersValueType>
void
QuaternionRigidTransform<TParametersValueType>::ComputeMatrix()
{
  VnlQuaternionType conjugateRotation = m_Rotation.conjugate();
  // this is done to compensate for the transposed representation
  // between VNL and ITK
  MatrixType newMatrix;

  newMatrix = conjugateRotation.rotation_matrix_transpose();
  this->SetVarMatrix(newMatrix);
}

template<typename TParametersValueType>
void
QuaternionRigidTransform<TParametersValueType>::ComputeMatrixParameters()
{
  VnlQuaternionType quat( this->GetMatrix().GetVnlMatrix() );

  m_Rotation = quat.conjugate();
}

} // namespace

#endif
