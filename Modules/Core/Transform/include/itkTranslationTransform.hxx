/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkTranslationTransform_hxx
#define itkTranslationTransform_hxx

#include "itkMath.h"

namespace itk
{

template <typename TParametersValueType, unsigned int VDimension>
TranslationTransform<TParametersValueType, VDimension>::TranslationTransform()
  : Superclass(ParametersDimension)
  , m_IdentityJacobian(VDimension, VDimension)
{
  m_Offset.Fill(0);

  // The Jacobian of this transform is constant.
  // Therefore the m_IdentityJacobian variable can be
  // initialized here and be shared among all the threads.
  this->m_IdentityJacobian.Fill(0.0);
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    this->m_IdentityJacobian(i, i) = 1.0;
  }
}

template <typename TParametersValueType, unsigned int VDimension>
void
TranslationTransform<TParametersValueType, VDimension>::SetParameters(const ParametersType & parameters)
{
  if (parameters.Size() < SpaceDimension)
  {
    itkExceptionMacro(<< "Error setting parameters: parameters array size (" << parameters.Size()
                      << ") is less than expected (SpaceDimension = " << SpaceDimension << ")");
  }

  // Save parameters. Needed for proper operation of TransformUpdateParameters.
  if (&parameters != &(this->m_Parameters))
  {
    this->m_Parameters = parameters;
  }

  bool modified = false;
  for (unsigned int i = 0; i < SpaceDimension; ++i)
  {
    if (Math::NotExactlyEquals(m_Offset[i], parameters[i]))
    {
      m_Offset[i] = parameters[i];
      modified = true;
    }
  }
  if (modified)
  {
    this->Modified();
  }
}


template <typename TParametersValueType, unsigned int VDimension>
auto
TranslationTransform<TParametersValueType, VDimension>::GetParameters() const -> const ParametersType &
{
  for (unsigned int i = 0; i < SpaceDimension; ++i)
  {
    this->m_Parameters[i] = this->m_Offset[i];
  }
  return this->m_Parameters;
}


template <typename TParametersValueType, unsigned int VDimension>
void
TranslationTransform<TParametersValueType, VDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Offset: " << m_Offset << std::endl;
}


template <typename TParametersValueType, unsigned int VDimension>
void
TranslationTransform<TParametersValueType, VDimension>::Compose(const Self * other, bool)
{
  this->Translate(other->m_Offset);
}


template <typename TParametersValueType, unsigned int VDimension>
void
TranslationTransform<TParametersValueType, VDimension>::Translate(const OutputVectorType & offset, bool)
{
  ParametersType newOffset(SpaceDimension);

  for (unsigned int i = 0; i < SpaceDimension; ++i)
  {
    newOffset[i] = m_Offset[i] + offset[i];
  }
  this->SetParameters(newOffset);
}


template <typename TParametersValueType, unsigned int VDimension>
auto
TranslationTransform<TParametersValueType, VDimension>::TransformPoint(const InputPointType & point) const
  -> OutputPointType
{
  return point + m_Offset;
}


template <typename TParametersValueType, unsigned int VDimension>
auto
TranslationTransform<TParametersValueType, VDimension>::TransformVector(const InputVectorType & vect) const
  -> OutputVectorType
{
  return vect;
}


template <typename TParametersValueType, unsigned int VDimension>
auto
TranslationTransform<TParametersValueType, VDimension>::TransformVector(const InputVnlVectorType & vect) const
  -> OutputVnlVectorType
{
  return vect;
}


template <typename TParametersValueType, unsigned int VDimension>
typename TranslationTransform<TParametersValueType, VDimension>::OutputCovariantVectorType
TranslationTransform<TParametersValueType, VDimension>::TransformCovariantVector(
  const InputCovariantVectorType & vect) const
{
  return vect;
}


template <typename TParametersValueType, unsigned int VDimension>
bool
TranslationTransform<TParametersValueType, VDimension>::GetInverse(Self * inverse) const
{
  if (!inverse)
  {
    return false;
  }

  inverse->SetFixedParameters(this->GetFixedParameters());
  inverse->m_Offset = -m_Offset;
  return true;
}


template <typename TParametersValueType, unsigned int VDimension>
auto
TranslationTransform<TParametersValueType, VDimension>::GetInverseTransform() const -> InverseTransformBasePointer
{
  Pointer inv = New();

  return GetInverse(inv) ? inv.GetPointer() : nullptr;
}


template <typename TParametersValueType, unsigned int VDimension>
void
TranslationTransform<TParametersValueType, VDimension>::ComputeJacobianWithRespectToParameters(
  const InputPointType &,
  JacobianType & jacobian) const
{
  // the Jacobian is constant for this transform, and it has already been
  // initialized in the constructor, so we just need to return it here.
  jacobian = this->m_IdentityJacobian;
}


template <typename TParametersValueType, unsigned int VDimension>
void
TranslationTransform<TParametersValueType, VDimension>::ComputeJacobianWithRespectToPosition(
  const InputPointType &,
  JacobianPositionType & jac) const
{
  jac.set_identity();
}


template <typename TParametersValueType, unsigned int VDimension>
void
TranslationTransform<TParametersValueType, VDimension>::SetIdentity()
{
  m_Offset.Fill(0.0);
}

} // end namespace itk

#endif
