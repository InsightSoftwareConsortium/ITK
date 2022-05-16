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
#ifndef itkScaleTransform_hxx
#define itkScaleTransform_hxx

#include "itkMath.h"

namespace itk
{

template <typename TParametersValueType, unsigned int VDimension>
ScaleTransform<TParametersValueType, VDimension>::ScaleTransform()
  : Superclass(ParametersDimension)
{
  m_Scale.Fill(NumericTraits<ScalarType>::OneValue());
}

template <typename TParametersValueType, unsigned int VDimension>
void
ScaleTransform<TParametersValueType, VDimension>::SetParameters(const ParametersType & parameters)
{
  for (unsigned int i = 0; i < SpaceDimension; ++i)
  {
    m_Scale[i] = parameters[i];
  }
  // Save parameters. Needed for proper operation of TransformUpdateParameters.
  if (&parameters != &(this->m_Parameters))
  {
    this->m_Parameters = parameters;
  }

  this->ComputeMatrix();
  this->ComputeOffset();

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();
}


template <typename TParametersValueType, unsigned int VDimension>
auto
ScaleTransform<TParametersValueType, VDimension>::GetParameters() const -> const ParametersType &
{
  itkDebugMacro(<< "Getting parameters ");
  // Transfer the translation part
  for (unsigned int i = 0; i < SpaceDimension; ++i)
  {
    this->m_Parameters[i] = m_Scale[i];
  }

  itkDebugMacro(<< "After getting parameters " << this->m_Parameters);

  return this->m_Parameters;
}


template <typename TParametersValueType, unsigned int VDimension>
void
ScaleTransform<TParametersValueType, VDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Scale: " << m_Scale << std::endl;
}


template <typename TParametersValueType, unsigned int VDimension>
void
ScaleTransform<TParametersValueType, VDimension>::Compose(const Self * other, bool)
{
  for (unsigned int i = 0; i < SpaceDimension; ++i)
  {
    m_Scale[i] *= other->m_Scale[i];
  }
}


template <typename TParametersValueType, unsigned int VDimension>
void
ScaleTransform<TParametersValueType, VDimension>::Scale(const ScaleType & scale, bool)
{
  for (unsigned int i = 0; i < SpaceDimension; ++i)
  {
    m_Scale[i] *= scale[i];
  }
}


template <typename TParametersValueType, unsigned int VDimension>
auto
ScaleTransform<TParametersValueType, VDimension>::TransformPoint(const InputPointType & point) const -> OutputPointType
{
  OutputPointType        result;
  const InputPointType & center = this->GetCenter();

  for (unsigned int i = 0; i < SpaceDimension; ++i)
  {
    result[i] = (point[i] - center[i]) * m_Scale[i] + center[i];
  }
  return result;
}


template <typename TParametersValueType, unsigned int VDimension>
auto
ScaleTransform<TParametersValueType, VDimension>::TransformVector(const InputVectorType & vect) const
  -> OutputVectorType
{
  OutputVectorType result;

  for (unsigned int i = 0; i < SpaceDimension; ++i)
  {
    result[i] = vect[i] * m_Scale[i];
  }
  return result;
}


template <typename TParametersValueType, unsigned int VDimension>
auto
ScaleTransform<TParametersValueType, VDimension>::TransformVector(const InputVnlVectorType & vect) const
  -> OutputVnlVectorType
{
  OutputVnlVectorType result;

  for (unsigned int i = 0; i < SpaceDimension; ++i)
  {
    result[i] = vect[i] * m_Scale[i];
  }
  return result;
}


template <typename TParametersValueType, unsigned int VDimension>
auto
ScaleTransform<TParametersValueType, VDimension>::TransformCovariantVector(const InputCovariantVectorType & vect) const
  -> OutputCovariantVectorType
{
  // Covariant Vectors are scaled by the inverse
  OutputCovariantVectorType result;

  for (unsigned int i = 0; i < SpaceDimension; ++i)
  {
    result[i] = vect[i] / m_Scale[i];
  }
  return result;
}


template <typename TParametersValueType, unsigned int VDimension>
bool
ScaleTransform<TParametersValueType, VDimension>::GetInverse(Self * inverse) const
{
  if (!inverse)
  {
    return false;
  }
  inverse->SetFixedParameters(this->GetFixedParameters());
  for (unsigned int i = 0; i < SpaceDimension; ++i)
  {
    inverse->m_Scale[i] = 1.0 / m_Scale[i];
  }

  return true;
}


template <typename TParametersValueType, unsigned int VDimension>
auto
ScaleTransform<TParametersValueType, VDimension>::GetInverseTransform() const -> InverseTransformBasePointer
{
  Pointer inv = New();

  if (this->GetInverse(inv))
  {
    return inv.GetPointer();
  }
  return nullptr;
}

template <typename TParametersValueType, unsigned int VDimension>
void
ScaleTransform<TParametersValueType, VDimension>::SetIdentity()
{
  Superclass::SetIdentity();
  ScaleType i;
  i.Fill(1.0);
  this->SetScale(i);
}


template <typename TParametersValueType, unsigned int VDimension>
void
ScaleTransform<TParametersValueType, VDimension>::ComputeJacobianWithRespectToParameters(const InputPointType & p,
                                                                                         JacobianType &         j) const
{
  j.SetSize(SpaceDimension, this->GetNumberOfLocalParameters());
  j.Fill(0.0);
  const InputPointType & center = this->GetCenter();
  for (unsigned int dim = 0; dim < SpaceDimension; ++dim)
  {
    j(dim, dim) = p[dim] - center[dim];
  }
}


template <typename TParametersValueType, unsigned int VDimension>
void
ScaleTransform<TParametersValueType, VDimension>::ComputeJacobianWithRespectToPosition(const InputPointType &,
                                                                                       JacobianPositionType & jac) const
{
  jac.fill(0.0);
  for (unsigned int dim = 0; dim < VDimension; ++dim)
  {
    jac[dim][dim] = m_Scale[dim];
  }
}

template <typename TParametersValueType, unsigned int VDimension>
void
ScaleTransform<TParametersValueType, VDimension>::SetScale(const ScaleType & scale)
{
  m_Scale = scale;
  this->ComputeMatrix();
  this->ComputeOffset();
  this->Modified();
}

template <typename TParametersValueType, unsigned int VDimension>
void
ScaleTransform<TParametersValueType, VDimension>::ComputeMatrix()
{

  MatrixType matrix;

  matrix.SetIdentity();
  for (unsigned int dim = 0; dim < SpaceDimension; ++dim)
  {
    matrix[dim][dim] = m_Scale[dim];
  }

  this->SetVarMatrix(matrix);
}


// Back transform a point
template <typename TParametersValueType, unsigned int VDimension>
inline typename ScaleTransform<TParametersValueType, VDimension>::InputPointType
ScaleTransform<TParametersValueType, VDimension>::BackTransform(const OutputPointType & point) const
{
  InputPointType         result;
  const InputPointType & center = this->GetCenter();

  for (unsigned int i = 0; i < SpaceDimension; ++i)
  {
    result[i] = (point[i] + center[i]) / m_Scale[i] - center[i];
  }
  return result;
}

// Back transform a vector
template <typename TParametersValueType, unsigned int VDimension>
inline typename ScaleTransform<TParametersValueType, VDimension>::InputVectorType
ScaleTransform<TParametersValueType, VDimension>::BackTransform(const OutputVectorType & vect) const
{
  InputVectorType result;

  for (unsigned int i = 0; i < SpaceDimension; ++i)
  {
    result[i] = vect[i] / m_Scale[i];
  }
  return result;
}

// Back transform a vnl_vector
template <typename TParametersValueType, unsigned int VDimension>
inline typename ScaleTransform<TParametersValueType, VDimension>::InputVnlVectorType
ScaleTransform<TParametersValueType, VDimension>::BackTransform(const OutputVnlVectorType & vect) const
{
  InputVnlVectorType result;

  for (unsigned int i = 0; i < SpaceDimension; ++i)
  {
    result[i] = vect[i] / m_Scale[i];
  }
  return result;
}

// Back Transform a CovariantVector
template <typename TParametersValueType, unsigned int VDimension>
inline typename ScaleTransform<TParametersValueType, VDimension>::InputCovariantVectorType
ScaleTransform<TParametersValueType, VDimension>::BackTransform(const OutputCovariantVectorType & vect) const
{
  // Covariant Vectors are scaled by the inverse
  InputCovariantVectorType result;

  for (unsigned int i = 0; i < SpaceDimension; ++i)
  {
    result[i] = vect[i] * m_Scale[i];
  }
  return result;
}

} // end namespace itk

#endif
