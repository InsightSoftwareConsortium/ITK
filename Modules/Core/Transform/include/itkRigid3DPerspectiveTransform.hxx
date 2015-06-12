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
#ifndef itkRigid3DPerspectiveTransform_hxx
#define itkRigid3DPerspectiveTransform_hxx

#include "itkRigid3DPerspectiveTransform.h"

namespace itk
{
// Constructor with default arguments
template<typename TParametersValueType>
Rigid3DPerspectiveTransform<TParametersValueType>
::Rigid3DPerspectiveTransform() : Superclass(ParametersDimension)
{
  m_Offset.Fill(0);
  m_Versor.SetIdentity();
  m_RotationMatrix = m_Versor.GetMatrix();
  m_FocalDistance = NumericTraits<ScalarType>::OneValue();
  m_FixedOffset.Fill(0);
  m_CenterOfRotation.Fill(0);
  this->m_Parameters.Fill(0);
  this->m_Parameters[3] = 1; // identity versor
}

// Destructor
template<typename TParametersValueType>
Rigid3DPerspectiveTransform<TParametersValueType>::
~Rigid3DPerspectiveTransform()
{
}

// Print self
template<typename TParametersValueType>
void
Rigid3DPerspectiveTransform<TParametersValueType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Parameters: "   << this->m_Parameters  << std::endl;
  os << indent << "Offset: "       << m_Offset   << std::endl;
  os << indent << "Rotation: "     << m_Versor << std::endl;
  os << indent << "FocalDistance: " << m_FocalDistance << std::endl;
  os << indent << "RotationMatrix: " << m_RotationMatrix   << std::endl;
  os << indent << "FixedOffset: " << m_FixedOffset   << std::endl;
  os << indent << "CenterOfRotation: " << m_CenterOfRotation   << std::endl;
}

// Set Parameters
template<typename TParametersValueType>
void
Rigid3DPerspectiveTransform<TParametersValueType>
::SetParameters(const ParametersType & parameters)
{
  itkDebugMacro(<< "Setting parameters " << parameters);

  // Save parameters. Needed for proper operation of TransformUpdateParameters.
  if( &parameters != &(this->m_Parameters) )
    {
    this->m_Parameters = parameters;
    }

  // Transfer the versor part

  AxisType axis;

  double norm = parameters[0] * parameters[0];
  axis[0] = parameters[0];
  norm += parameters[1] * parameters[1];
  axis[1] = parameters[1];
  norm += parameters[2] * parameters[2];
  axis[2] = parameters[2];
  if( norm > NumericTraits<double>::ZeroValue() )
    {
    norm = std::sqrt(norm);
    }

  double epsilon = 1e-10;
  if( norm >= 1.0 - epsilon )
    {
    axis = axis / ( norm + epsilon * norm );
    }

  m_Versor.Set(axis);

  itkDebugMacro( << "Versor is now " << this->GetRotation() );

  // Transfer the translation part
  OffsetType offset;
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    offset[i] = parameters[i + 3];
    }

  this->SetOffset(offset);

  this->ComputeMatrix();

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();
}

// Set Parameters
template<typename TParametersValueType>
const typename Rigid3DPerspectiveTransform<TParametersValueType>::ParametersType
& Rigid3DPerspectiveTransform<TParametersValueType>
::GetParameters() const
  {
  itkDebugMacro(<< "Getting parameters ");

  this->m_Parameters[0] = this->GetRotation().GetX();
  this->m_Parameters[1] = this->GetRotation().GetY();
  this->m_Parameters[2] = this->GetRotation().GetZ();

  // Transfer the translation
  this->m_Parameters[3] = this->GetOffset()[0];
  this->m_Parameters[4] = this->GetOffset()[1];
  this->m_Parameters[5] = this->GetOffset()[2];

  itkDebugMacro(<< "After getting parameters " << this->m_Parameters);

  return this->m_Parameters;
  }

// Set rotation
template<typename TParametersValueType>
void
Rigid3DPerspectiveTransform<TParametersValueType>::SetRotation(const VersorType & rotation)
{
  m_Versor          = rotation;
  m_RotationMatrix  = m_Versor.GetMatrix();
}

// Set rotation
template<typename TParametersValueType>
void
Rigid3DPerspectiveTransform<TParametersValueType>::SetRotation(const Vector<TParametersValueType, 3> & axis, double angle)
{
  const double sinus   = std::sin(angle / 2.0);
  const double cosinus = std::cos(angle / 2.0);

  Vector<TParametersValueType, 3> norm;
  norm = axis;
  norm.Normalize();
  norm *= sinus;
  VnlQuaternionType q;

  q[0] = cosinus;
  q[1] = norm[0];
  q[2] = norm[1];
  q[3] = norm[2];

  VersorType v;
  v.Set(q);
  this->SetRotation(v);
}

// Transform a point
template<typename TParametersValueType>
typename Rigid3DPerspectiveTransform<TParametersValueType>::OutputPointType
Rigid3DPerspectiveTransform<TParametersValueType>::TransformPoint(const InputPointType & point) const
{
  unsigned int   i;
  InputPointType centered;

  for( i = 0; i < 3; i++ )
    {
    centered[i] = point[i] - m_CenterOfRotation[i];
    }

  InputPointType rotated =  m_RotationMatrix * centered;

  InputPointType rigided;
  for( i = 0; i < 3; i++ )
    {
    rigided[i] = rotated[i] + m_Offset[i] + m_CenterOfRotation[i]
      + m_FixedOffset[i];
    }

  OutputPointType result;

  TParametersValueType factor = m_FocalDistance / rigided[2];

  result[0] = rigided[0] * factor;
  result[1] = rigided[1] * factor;

  return result;
}

// Transform a point
template<typename TParametersValueType>
void
Rigid3DPerspectiveTransform<TParametersValueType>::ComputeMatrix(void)
{
  m_RotationMatrix = m_Versor.GetMatrix();
}

template<typename TParametersValueType>
void
Rigid3DPerspectiveTransform<TParametersValueType>
::ComputeJacobianWithRespectToParameters(const InputPointType &,
                                         JacobianType & jacobian) const
{
  jacobian.SetSize( 3, this->GetNumberOfLocalParameters() );
  jacobian.Fill(0.0);
  // TODO
}

} // namespace

#endif
