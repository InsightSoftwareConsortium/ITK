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
#ifndef __itkEuler3DTransform_hxx
#define __itkEuler3DTransform_hxx

#include "itkEuler3DTransform.h"

namespace itk
{
// Constructor with default arguments
template <class TScalar>
Euler3DTransform<TScalar>
::Euler3DTransform() :
  Superclass(ParametersDimension)
{
  m_ComputeZYX = false;
  m_AngleX = m_AngleY = m_AngleZ = NumericTraits<ScalarType>::Zero;
}

// Constructor with default arguments
template <class TScalar>
Euler3DTransform<TScalar>
::Euler3DTransform(const MatrixType & matrix, const OutputPointType & offset)
{
  m_ComputeZYX = false;
  this->SetMatrix(matrix);

  OffsetType off;
  off[0] = offset[0];
  off[1] = offset[1];
  off[2] = offset[2];
  this->SetOffset(off);
}

// Constructor with arguments
template <class TScalar>
Euler3DTransform<TScalar>
::Euler3DTransform(unsigned int parametersDimension) :
  Superclass(parametersDimension)
{
  m_ComputeZYX = false;
  m_AngleX = m_AngleY = m_AngleZ = NumericTraits<ScalarType>::Zero;
}

// Set Angles
template <class TScalar>
void
Euler3DTransform<TScalar>
::SetVarRotation(ScalarType angleX, ScalarType angleY, ScalarType angleZ)
{
  this->m_AngleX = angleX;
  this->m_AngleY = angleY;
  this->m_AngleZ = angleZ;
}

// Set Parameters
template <class TScalar>
void
Euler3DTransform<TScalar>
::SetParameters(const ParametersType & parameters)
{
  itkDebugMacro(<< "Setting parameters " << parameters);

  // Save parameters. Needed for proper operation of TransformUpdateParameters.
  if( &parameters != &(this->m_Parameters) )
    {
    this->m_Parameters = parameters;
    }

  // Set angles with parameters
  m_AngleX = parameters[0];
  m_AngleY = parameters[1];
  m_AngleZ = parameters[2];
  this->ComputeMatrix();

  // Transfer the translation part
  OutputVectorType newTranslation;
  newTranslation[0] = parameters[3];
  newTranslation[1] = parameters[4];
  newTranslation[2] = parameters[5];
  this->SetVarTranslation(newTranslation);
  this->ComputeOffset();

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();

  itkDebugMacro(<< "After setting parameters ");
}

// Get Parameters
template <class TScalar>
const typename Euler3DTransform<TScalar>::ParametersType
& Euler3DTransform<TScalar>
::GetParameters(void) const
  {
  this->m_Parameters[0] = m_AngleX;
  this->m_Parameters[1] = m_AngleY;
  this->m_Parameters[2] = m_AngleZ;
  this->m_Parameters[3] = this->GetTranslation()[0];
  this->m_Parameters[4] = this->GetTranslation()[1];
  this->m_Parameters[5] = this->GetTranslation()[2];

  return this->m_Parameters;
  }

// Set Rotational Part
template <class TScalar>
void
Euler3DTransform<TScalar>
::SetRotation(ScalarType angleX, ScalarType angleY, ScalarType angleZ)
{
  m_AngleX = angleX;
  m_AngleY = angleY;
  m_AngleZ = angleZ;
  this->ComputeMatrix();
  this->ComputeOffset();
}

// Compose
template <class TScalar>
void
Euler3DTransform<TScalar>
::SetIdentity(void)
{
  Superclass::SetIdentity();
  m_AngleX = 0;
  m_AngleY = 0;
  m_AngleZ = 0;
}

// Compute angles from the rotation matrix
template <class TScalar>
void
Euler3DTransform<TScalar>
::ComputeMatrixParameters(void)
{
  if( m_ComputeZYX )
    {
    m_AngleY = -vcl_asin(this->GetMatrix()[2][0]);
    double C = vcl_cos(m_AngleY);
    if( vcl_fabs(C) > 0.00005 )
      {
      double x = this->GetMatrix()[2][2] / C;
      double y = this->GetMatrix()[2][1] / C;
      m_AngleX = vcl_atan2(y, x);
      x = this->GetMatrix()[0][0] / C;
      y = this->GetMatrix()[1][0] / C;
      m_AngleZ = vcl_atan2(y, x);
      }
    else
      {
      m_AngleX = NumericTraits<ScalarType>::Zero;
      double x = this->GetMatrix()[1][1];
      double y = -this->GetMatrix()[0][1];
      m_AngleZ = vcl_atan2(y, x);
      }
    }
  else
    {
    m_AngleX = vcl_asin(this->GetMatrix()[2][1]);
    double A = vcl_cos(m_AngleX);
    if( vcl_fabs(A) > 0.00005 )
      {
      double x = this->GetMatrix()[2][2] / A;
      double y = -this->GetMatrix()[2][0] / A;
      m_AngleY = vcl_atan2(y, x);

      x = this->GetMatrix()[1][1] / A;
      y = -this->GetMatrix()[0][1] / A;
      m_AngleZ = vcl_atan2(y, x);
      }
    else
      {
      m_AngleZ = NumericTraits<ScalarType>::Zero;
      double x = this->GetMatrix()[0][0];
      double y = this->GetMatrix()[1][0];
      m_AngleY = vcl_atan2(y, x);
      }
    }
  this->ComputeMatrix();
}

// Compute the matrix
template <class TScalar>
void
Euler3DTransform<TScalar>
::ComputeMatrix(void)
{
  // need to check if angles are in the right order
  const ScalarType cx = vcl_cos(m_AngleX);
  const ScalarType sx = vcl_sin(m_AngleX);
  const ScalarType cy = vcl_cos(m_AngleY);
  const ScalarType sy = vcl_sin(m_AngleY);
  const ScalarType cz = vcl_cos(m_AngleZ);
  const ScalarType sz = vcl_sin(m_AngleZ);
  const ScalarType one = NumericTraits<ScalarType>::One;
  const ScalarType zero = NumericTraits<ScalarType>::Zero;

  Matrix<TScalar, 3, 3> RotationX;
  RotationX[0][0] = one;  RotationX[0][1] = zero; RotationX[0][2] = zero;
  RotationX[1][0] = zero; RotationX[1][1] = cx;   RotationX[1][2] = -sx;
  RotationX[2][0] = zero; RotationX[2][1] = sx;   RotationX[2][2] = cx;

  Matrix<TScalar, 3, 3> RotationY;
  RotationY[0][0] = cy;   RotationY[0][1] = zero; RotationY[0][2] = sy;
  RotationY[1][0] = zero; RotationY[1][1] = one;  RotationY[1][2] = zero;
  RotationY[2][0] = -sy;  RotationY[2][1] = zero; RotationY[2][2] = cy;

  Matrix<TScalar, 3, 3> RotationZ;
  RotationZ[0][0] = cz;   RotationZ[0][1] = -sz;  RotationZ[0][2] = zero;
  RotationZ[1][0] = sz;   RotationZ[1][1] = cz;   RotationZ[1][2] = zero;
  RotationZ[2][0] = zero; RotationZ[2][1] = zero; RotationZ[2][2] = one;

  /** Aply the rotation first around Y then X then Z */
  if( m_ComputeZYX )
    {
    this->SetVarMatrix(RotationZ * RotationY * RotationX);
    }
  else
    {
    // Like VTK transformation order
    this->SetVarMatrix(RotationZ * RotationX * RotationY);
    }
}

template <class TScalar>
void
Euler3DTransform<TScalar>
::ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const
{
  // need to check if angles are in the right order
  const double cx = vcl_cos(m_AngleX);
  const double sx = vcl_sin(m_AngleX);
  const double cy = vcl_cos(m_AngleY);
  const double sy = vcl_sin(m_AngleY);
  const double cz = vcl_cos(m_AngleZ);
  const double sz = vcl_sin(m_AngleZ);

  jacobian.SetSize( 3, this->GetNumberOfLocalParameters() );
  jacobian.Fill(0.0);

  const double px = p[0] - this->GetCenter()[0];
  const double py = p[1] - this->GetCenter()[1];
  const double pz = p[2] - this->GetCenter()[2];

  if( m_ComputeZYX )
    {
    jacobian[0][0] = ( cz * sy * cx + sz * sx ) * py + ( -cz * sy * sx + sz * cx ) * pz;
    jacobian[1][0] = ( sz * sy * cx - cz * sx ) * py + ( -sz * sy * sx - cz * cx ) * pz;
    jacobian[2][0] = ( cy * cx ) * py + ( -cy * sx ) * pz;

    jacobian[0][1] = ( -cz * sy ) * px + ( cz * cy * sx ) * py + ( cz * cy * cx ) * pz;
    jacobian[1][1] = ( -sz * sy ) * px + ( sz * cy * sx ) * py + ( sz * cy * cx ) * pz;
    jacobian[2][1] = ( -cy ) * px + ( -sy * sx ) * py + ( -sy * cx ) * pz;

    jacobian[0][2] = ( -sz * cy ) * px + ( -sz * sy * sx - cz * cx ) * py
      + ( -sz * sy * cx + cz * sx ) * pz;
    jacobian[1][2] = ( cz * cy ) * px + ( cz * sy * sx - sz * cx ) * py + ( cz * sy * cx + sz * sx ) * pz;
    jacobian[2][2] = 0;
    }
  else
    {
    jacobian[0][0] = ( -sz * cx * sy ) * px + ( sz * sx ) * py + ( sz * cx * cy ) * pz;
    jacobian[1][0] = ( cz * cx * sy ) * px + ( -cz * sx ) * py + ( -cz * cx * cy ) * pz;
    jacobian[2][0] = ( sx * sy ) * px + ( cx ) * py + ( -sx * cy ) * pz;

    jacobian[0][1] = ( -cz * sy - sz * sx * cy ) * px + ( cz * cy - sz * sx * sy ) * pz;
    jacobian[1][1] = ( -sz * sy + cz * sx * cy ) * px + ( sz * cy + cz * sx * sy ) * pz;
    jacobian[2][1] = ( -cx * cy ) * px + ( -cx * sy ) * pz;

    jacobian[0][2] = ( -sz * cy - cz * sx * sy ) * px + ( -cz * cx ) * py
      + ( -sz * sy + cz * sx * cy ) * pz;
    jacobian[1][2] = ( cz * cy - sz * sx * sy ) * px + ( -sz * cx ) * py
      + ( cz * sy + sz * sx * cy ) * pz;
    jacobian[2][2] = 0;
    }

  // compute derivatives for the translation part
  unsigned int blockOffset = 3;
  for( unsigned int dim = 0; dim < SpaceDimension; dim++ )
    {
    jacobian[dim][blockOffset + dim] = 1.0;
    }
}

// Print self
template <class TScalar>
void
Euler3DTransform<TScalar>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Euler's angles: AngleX=" << m_AngleX
     << " AngleY=" << m_AngleY
     << " AngleZ=" << m_AngleZ
     << std::endl;
  os << indent << "m_ComputeZYX = " << m_ComputeZYX << std::endl;
}

} // namespace

#endif
