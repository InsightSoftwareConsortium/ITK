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
#ifndef itkScalableAffineTransform_hxx
#define itkScalableAffineTransform_hxx

#include "itkMath.h"
#include "itkNumericTraits.h"
#include "vnl/algo/vnl_matrix_inverse.h"
#include "itkMath.h"

namespace itk
{

template <typename TParametersValueType, unsigned int VDimension>
ScalableAffineTransform<TParametersValueType, VDimension>::ScalableAffineTransform()
  : Superclass(Self::ParametersDimension)
{
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    m_Scale[i] = 1;
    m_MatrixScale[i] = 1;
  }
}

template <typename TParametersValueType, unsigned int VDimension>
ScalableAffineTransform<TParametersValueType, VDimension>::ScalableAffineTransform(unsigned int,
                                                                                   unsigned int parametersDimension)
  : Superclass(parametersDimension)
{
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    m_Scale[i] = 1;
    m_MatrixScale[i] = 1;
  }
}

template <typename TParametersValueType, unsigned int VDimension>
ScalableAffineTransform<TParametersValueType, VDimension>::ScalableAffineTransform(unsigned int parametersDimension)
  : Superclass(parametersDimension)
{
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    m_Scale[i] = 1;
    m_MatrixScale[i] = 1;
  }
}

template <typename TParametersValueType, unsigned int VDimension>
ScalableAffineTransform<TParametersValueType, VDimension>::ScalableAffineTransform(const MatrixType &       matrix,
                                                                                   const OutputVectorType & offset)
  : Superclass(matrix, offset)
{
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    m_Scale[i] = 1;
    m_MatrixScale[i] = 1;
  }
}

template <typename TParametersValueType, unsigned int VDimension>
void
ScalableAffineTransform<TParametersValueType, VDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  unsigned int i;

  os << indent << "Scale : ";
  for (i = 0; i < VDimension; ++i)
  {
    os << m_Scale[i] << " ";
  }
  os << std::endl;
  os << indent << "MatrixScale : ";
  for (i = 0; i < VDimension; ++i)
  {
    os << m_MatrixScale[i] << " ";
  }
  os << std::endl;
}

template <typename TParametersValueType, unsigned int VDimension>
void
ScalableAffineTransform<TParametersValueType, VDimension>::SetIdentity()
{
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    m_Scale[i] = 1;
    m_MatrixScale[i] = 1;
  }
  this->Superclass::SetIdentity();
}

template <typename TParametersValueType, unsigned int VDimension>
void
ScalableAffineTransform<TParametersValueType, VDimension>::SetScale(const InputVectorType & scale)
{
  unsigned int i;

  for (i = 0; i < VDimension; ++i)
  {
    m_Scale[i] = scale[i];
  }
  this->ComputeMatrix();
  this->Modified();
}

template <typename TParametersValueType, unsigned int VDimension>
void
ScalableAffineTransform<TParametersValueType, VDimension>::SetScale(const double scale[VDimension])
{
  unsigned int i;

  for (i = 0; i < VDimension; ++i)
  {
    m_Scale[i] = scale[i];
  }
  this->ComputeMatrix();
  this->Modified();
}

template <typename TParametersValueType, unsigned int VDimension>
bool
ScalableAffineTransform<TParametersValueType, VDimension>::GetInverse(Self * inverse) const
{
  return this->Superclass::GetInverse(inverse);
}

template <typename TParametersValueType, unsigned int VDimension>
auto
ScalableAffineTransform<TParametersValueType, VDimension>::GetInverseTransform() const -> InverseTransformBasePointer
{
  Pointer inv = New();

  return this->GetInverse(inv) ? inv.GetPointer() : nullptr;
}

template <typename TParametersValueType, unsigned int VDimension>
void
ScalableAffineTransform<TParametersValueType, VDimension>::ComputeMatrix()
{
  bool scaleChanged = false;
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    if (Math::NotExactlyEquals(m_Scale[i], m_MatrixScale[i]))
    {
      scaleChanged = true;
    }
  }
  if (scaleChanged)
  {
    MatrixType                                mat;
    typename MatrixType::InternalMatrixType & imat = mat.GetVnlMatrix();
    for (unsigned int i = 0; i < VDimension; ++i)
    {
      if (Math::NotAlmostEquals(m_MatrixScale[i],
                                NumericTraits<typename NumericTraits<InputVectorType>::ValueType>::ZeroValue()) &&
          Math::NotAlmostEquals(m_Scale[i], 0.0))
      {
        imat.put(i, i, m_Scale[i] / m_MatrixScale[i] * this->GetMatrix()[i][i]);
        m_MatrixScale[i] = m_Scale[i];
      }
      else
      {
        m_Scale[i] = 1;
        m_MatrixScale[i] = 1;
        imat.put(i, i, this->GetMatrix()[i][i]);
      }
    }
    Superclass::SetVarMatrix(mat);
  }
}

} // namespace itk

#endif
