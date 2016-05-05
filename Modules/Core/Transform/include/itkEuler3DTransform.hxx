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
#ifndef itkEuler3DTransform_hxx
#define itkEuler3DTransform_hxx

#include "itkEuler3DTransform.h"

namespace itk
{
// Constructor with default arguments
template<typename TParametersValueType>
Euler3DTransform<TParametersValueType>
::Euler3DTransform() :
  Superclass(ParametersDimension)
{
  m_ComputeZYX = false;
  m_AngleX = m_AngleY = m_AngleZ = NumericTraits<ScalarType>::ZeroValue();
  this->m_FixedParameters.SetSize(SpaceDimension+1);
  this->m_FixedParameters.Fill(0.0);
}

// Constructor with default arguments
template<typename TParametersValueType>
Euler3DTransform<TParametersValueType>
::Euler3DTransform(const MatrixType & matrix, const OutputPointType & offset)
{
  m_ComputeZYX = false;
  this->SetMatrix(matrix);

  OffsetType off;
  off[0] = offset[0];
  off[1] = offset[1];
  off[2] = offset[2];
  this->SetOffset(off);
  this->m_FixedParameters.SetSize(SpaceDimension+1);
  this->m_FixedParameters.Fill(0.0);
}

// Constructor with arguments
template<typename TParametersValueType>
Euler3DTransform<TParametersValueType>
::Euler3DTransform(unsigned int parametersDimension) :
  Superclass(parametersDimension)
{
  m_ComputeZYX = false;
  m_AngleX = m_AngleY = m_AngleZ = NumericTraits<ScalarType>::ZeroValue();
  this->m_FixedParameters.SetSize(SpaceDimension+1);
  this->m_FixedParameters.Fill(0.0);
}

// Set Angles
template<typename TParametersValueType>
void
Euler3DTransform<TParametersValueType>
::SetVarRotation(ScalarType angleX, ScalarType angleY, ScalarType angleZ)
{
  this->m_AngleX = angleX;
  this->m_AngleY = angleY;
  this->m_AngleZ = angleZ;
}

// Set Parameters
template<typename TParametersValueType>
void
Euler3DTransform<TParametersValueType>
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
template<typename TParametersValueType>
const typename Euler3DTransform<TParametersValueType>::ParametersType
& Euler3DTransform<TParametersValueType>
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

template<typename TParametersValueType>
const typename Euler3DTransform<TParametersValueType>::FixedParametersType &
Euler3DTransform<TParametersValueType>
::GetFixedParameters() const
{
  // Call the superclass GetFixedParameters so that it fills the
  // array, we ignore the returned data and add the additional
  // information to the updated array.
  Superclass::GetFixedParameters();
  this->m_FixedParameters[3] = this-> m_ComputeZYX;
  return this->m_FixedParameters;
}

template<typename TParametersValueType>
void
Euler3DTransform<TParametersValueType>
::SetFixedParameters(const FixedParametersType & parameters)
{
  InputPointType c;
  for( unsigned int i = 0; i < InputSpaceDimension; i++ )
    {
    c[i] = this->m_FixedParameters[i] = parameters[i];
    }
  this->SetCenter(c);
  //conditional is here for backwards compatibility: the
  //m_ComputeZYX flag was not serialized so it may or may
  //not be included as part of the fixed parameters
  if( parameters.Size() == 4 )
    {
    this->m_FixedParameters[3] = parameters[3];
    this->SetComputeZYX(this->m_FixedParameters[3]);
    }
}

// Set Rotational Part
template<typename TParametersValueType>
void
Euler3DTransform<TParametersValueType>
::SetRotation(ScalarType angleX, ScalarType angleY, ScalarType angleZ)
{
  m_AngleX = angleX;
  m_AngleY = angleY;
  m_AngleZ = angleZ;
  this->ComputeMatrix();
  this->ComputeOffset();
}

// Compose
template<typename TParametersValueType>
void
Euler3DTransform<TParametersValueType>
::SetIdentity(void)
{
  Superclass::SetIdentity();
  m_AngleX = 0;
  m_AngleY = 0;
  m_AngleZ = 0;
}

// Compute angles from the rotation matrix
template<typename TParametersValueType>
void
Euler3DTransform<TParametersValueType>
::ComputeMatrixParameters(void)
{
  if( m_ComputeZYX )
    {
    m_AngleY = -std::asin(this->GetMatrix()[2][0]);
    double C = std::cos(m_AngleY);
    if( std::fabs(C) > 0.00005 )
      {
      double x = this->GetMatrix()[2][2] / C;
      double y = this->GetMatrix()[2][1] / C;
      m_AngleX = std::atan2(y, x);
      x = this->GetMatrix()[0][0] / C;
      y = this->GetMatrix()[1][0] / C;
      m_AngleZ = std::atan2(y, x);
      }
    else
      {
      m_AngleX = NumericTraits<ScalarType>::ZeroValue();
      double x = this->GetMatrix()[1][1];
      double y = -this->GetMatrix()[0][1];
      m_AngleZ = std::atan2(y, x);
      }
    }
  else
    {
    m_AngleX = std::asin(this->GetMatrix()[2][1]);
    double A = std::cos(m_AngleX);
    if( std::fabs(A) > 0.00005 )
      {
      double x = this->GetMatrix()[2][2] / A;
      double y = -this->GetMatrix()[2][0] / A;
      m_AngleY = std::atan2(y, x);

      x = this->GetMatrix()[1][1] / A;
      y = -this->GetMatrix()[0][1] / A;
      m_AngleZ = std::atan2(y, x);
      }
    else
      {
      m_AngleZ = NumericTraits<ScalarType>::ZeroValue();
      double x = this->GetMatrix()[0][0];
      double y = this->GetMatrix()[1][0];
      m_AngleY = std::atan2(y, x);
      }
    }
  this->ComputeMatrix();
}

// Compute the matrix
template<typename TParametersValueType>
void
Euler3DTransform<TParametersValueType>
::ComputeMatrix(void)
{
  // need to check if angles are in the right order
  const ScalarType cx = std::cos(m_AngleX);
  const ScalarType sx = std::sin(m_AngleX);
  const ScalarType cy = std::cos(m_AngleY);
  const ScalarType sy = std::sin(m_AngleY);
  const ScalarType cz = std::cos(m_AngleZ);
  const ScalarType sz = std::sin(m_AngleZ);
  const ScalarType one = NumericTraits<ScalarType>::OneValue();
  const ScalarType zero = NumericTraits<ScalarType>::ZeroValue();

  Matrix<TParametersValueType, 3, 3> RotationX;
  RotationX[0][0] = one;  RotationX[0][1] = zero; RotationX[0][2] = zero;
  RotationX[1][0] = zero; RotationX[1][1] = cx;   RotationX[1][2] = -sx;
  RotationX[2][0] = zero; RotationX[2][1] = sx;   RotationX[2][2] = cx;

  Matrix<TParametersValueType, 3, 3> RotationY;
  RotationY[0][0] = cy;   RotationY[0][1] = zero; RotationY[0][2] = sy;
  RotationY[1][0] = zero; RotationY[1][1] = one;  RotationY[1][2] = zero;
  RotationY[2][0] = -sy;  RotationY[2][1] = zero; RotationY[2][2] = cy;

  Matrix<TParametersValueType, 3, 3> RotationZ;
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

template<typename TParametersValueType>
void
Euler3DTransform<TParametersValueType>
::ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const
{
  // need to check if angles are in the right order
  const double cx = std::cos(m_AngleX);
  const double sx = std::sin(m_AngleX);
  const double cy = std::cos(m_AngleY);
  const double sy = std::sin(m_AngleY);
  const double cz = std::cos(m_AngleZ);
  const double sz = std::sin(m_AngleZ);

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

template<typename TParametersValueType>
void
Euler3DTransform<TParametersValueType>
::SetComputeZYX (const bool flag)
{
  if( this->m_ComputeZYX != flag )
    {
    this->m_ComputeZYX = flag;
    this->ComputeMatrix();
    // The meaning of the parameters has changed so the transform
    // has been modified even if the parameter values have not.
    this->Modified();
    }
}

// Print self
template<typename TParametersValueType>
void
Euler3DTransform<TParametersValueType>::PrintSelf(std::ostream & os, Indent indent) const
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
