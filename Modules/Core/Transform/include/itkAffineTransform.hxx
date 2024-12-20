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
#ifndef itkAffineTransform_hxx
#define itkAffineTransform_hxx

#include "itkNumericTraits.h"
#include "vnl/algo/vnl_matrix_inverse.h"

namespace itk
{

template <typename TParametersValueType, unsigned int VDimension>
AffineTransform<TParametersValueType, VDimension>::AffineTransform()
  : Superclass(ParametersDimension)
{}

template <typename TParametersValueType, unsigned int VDimension>
AffineTransform<TParametersValueType, VDimension>::AffineTransform(unsigned int parametersDimension)
  : Superclass(parametersDimension)
{}

#if !defined(ITK_LEGACY_REMOVE)
template <typename TParametersValueType, unsigned int VDimension>
AffineTransform<TParametersValueType, VDimension>::AffineTransform(const MatrixType & matrix,
                                                                   const OutputVectorType & offset)
  : Superclass(matrix, offset)
{}
#endif

template <typename TParametersValueType, unsigned int VDimension>
void
AffineTransform<TParametersValueType, VDimension>::Translate(const OutputVectorType & trans, bool pre)
{
  OutputVectorType newTranslation = this->GetTranslation();

  if (pre)
  {
    newTranslation += this->GetMatrix() * trans;
  }
  else
  {
    newTranslation += trans;
  }
  this->SetVarTranslation(newTranslation);
  this->ComputeOffset();
  this->Modified();
}

template <typename TParametersValueType, unsigned int VDimension>
void
AffineTransform<TParametersValueType, VDimension>::Scale(const TParametersValueType & factor, bool pre)
{
  if (pre)
  {
    MatrixType newMatrix = this->GetMatrix();
    newMatrix *= factor;
    this->SetVarMatrix(newMatrix);
  }
  else
  {
    MatrixType newMatrix = this->GetMatrix();
    newMatrix *= factor;
    this->SetVarMatrix(newMatrix);

    OutputVectorType newTranslation = this->GetTranslation();
    newTranslation *= factor;
    this->SetVarTranslation(newTranslation);
  }
  this->ComputeMatrixParameters();
  this->ComputeOffset();
  this->Modified();
}

template <typename TParametersValueType, unsigned int VDimension>
void
AffineTransform<TParametersValueType, VDimension>::Scale(const OutputVectorType & factor, bool pre)
{
  MatrixType trans;
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    for (unsigned int j = 0; j < VDimension; ++j)
    {
      trans[i][j] = 0.0;
    }
    trans[i][i] = factor[i];
  }
  if (pre)
  {
    this->SetVarMatrix(this->GetMatrix() * trans);
  }
  else
  {
    this->SetVarMatrix(trans * this->GetMatrix());
    this->SetVarTranslation(trans * this->GetTranslation());
  }
  this->ComputeMatrixParameters();
  this->ComputeOffset();
  this->Modified();
}

template <typename TParametersValueType, unsigned int VDimension>
void
AffineTransform<TParametersValueType, VDimension>::Rotate(int axis1, int axis2, TParametersValueType angle, bool pre)
{
  MatrixType trans;
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    for (unsigned int j = 0; j < VDimension; ++j)
    {
      trans[i][j] = 0.0;
    }
    trans[i][i] = 1.0;
  }
  trans[axis1][axis1] = std::cos(angle);
  trans[axis1][axis2] = std::sin(angle);
  trans[axis2][axis1] = -std::sin(angle);
  trans[axis2][axis2] = std::cos(angle);
  if (pre)
  {
    this->SetVarMatrix(this->GetMatrix() * trans);
  }
  else
  {
    this->SetVarMatrix(trans * this->GetMatrix());
    this->SetVarTranslation(trans * this->GetTranslation());
  }
  this->ComputeMatrixParameters();
  this->ComputeOffset();
  this->Modified();
}

template <typename TParametersValueType, unsigned int VDimension>
void
AffineTransform<TParametersValueType, VDimension>::Rotate2D(TParametersValueType angle, bool pre)
{
  MatrixType trans;

  trans[0][0] = std::cos(angle);
  trans[0][1] = -std::sin(angle);
  trans[1][0] = std::sin(angle);
  trans[1][1] = std::cos(angle);
  if (pre)
  {
    this->SetVarMatrix(this->GetMatrix() * trans);
  }
  else
  {
    this->SetVarMatrix(trans * this->GetMatrix());
    this->SetVarTranslation(trans * this->GetTranslation());
  }
  this->ComputeMatrixParameters();
  this->ComputeOffset();
  this->Modified();
}

template <typename TParametersValueType, unsigned int VDimension>
void
AffineTransform<TParametersValueType, VDimension>::Rotate3D(const OutputVectorType & axis,
                                                            TParametersValueType angle,
                                                            bool pre)
{
  // Convert the axis to a unit vector
  const ScalarType r = std::sqrt(axis[0] * axis[0] + axis[1] * axis[1] + axis[2] * axis[2]);
  const ScalarType x1 = axis[0] / r;
  const ScalarType x2 = axis[1] / r;
  const ScalarType x3 = axis[2] / r;

  // Compute quaternion elements
  const ScalarType q0 = std::cos(angle / 2.0);
  const ScalarType q1 = x1 * std::sin(angle / 2.0);
  const ScalarType q2 = x2 * std::sin(angle / 2.0);
  const ScalarType q3 = x3 * std::sin(angle / 2.0);

  MatrixType trans;
  // Compute elements of the rotation matrix
  trans[0][0] = q0 * q0 + q1 * q1 - q2 * q2 - q3 * q3;
  trans[0][1] = 2.0 * (q1 * q2 - q0 * q3);
  trans[0][2] = 2.0 * (q1 * q3 + q0 * q2);
  trans[1][0] = 2.0 * (q1 * q2 + q0 * q3);
  trans[1][1] = q0 * q0 + q2 * q2 - q1 * q1 - q3 * q3;
  trans[1][2] = 2.0 * (q2 * q3 - q0 * q1);
  trans[2][0] = 2.0 * (q1 * q3 - q0 * q2);
  trans[2][1] = 2.0 * (q2 * q3 + q0 * q1);
  trans[2][2] = q0 * q0 + q3 * q3 - q1 * q1 - q2 * q2;

  // Compose rotation matrix with the existing matrix
  if (pre)
  {
    this->SetVarMatrix(this->GetMatrix() * trans);
  }
  else
  {
    this->SetVarMatrix(trans * this->GetMatrix());
    this->SetVarTranslation(trans * this->GetTranslation());
  }
  this->ComputeMatrixParameters();
  this->ComputeOffset();
  this->Modified();
}

template <typename TParametersValueType, unsigned int VDimension>
void
AffineTransform<TParametersValueType, VDimension>::Shear(int axis1, int axis2, TParametersValueType coef, bool pre)
{
  MatrixType trans;
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    for (unsigned int j = 0; j < VDimension; ++j)
    {
      trans[i][j] = 0.0;
    }
    trans[i][i] = 1.0;
  }
  trans[axis1][axis2] = coef;
  if (pre)
  {
    this->SetVarMatrix(this->GetMatrix() * trans);
  }
  else
  {
    this->SetVarMatrix(trans * this->GetMatrix());
    this->SetVarTranslation(trans * this->GetTranslation());
  }
  this->ComputeMatrixParameters();
  this->ComputeOffset();
  this->Modified();
}

template <typename TParametersValueType, unsigned int VDimension>
bool
AffineTransform<TParametersValueType, VDimension>::GetInverse(Self * inverse) const
{
  return this->Superclass::GetInverse(inverse);
}

template <typename TParametersValueType, unsigned int VDimension>
auto
AffineTransform<TParametersValueType, VDimension>::GetInverseTransform() const -> InverseTransformBasePointer
{
  return Superclass::InvertTransform(*this);
}

template <typename TParametersValueType, unsigned int VDimension>
auto
AffineTransform<TParametersValueType, VDimension>::Metric(const Self * other) const -> ScalarType
{
  ScalarType result = 0.0;
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    for (unsigned int j = 0; j < VDimension; ++j)
    {
      const ScalarType term1 = this->GetMatrix()[i][j] - other->GetMatrix()[i][j];
      result += term1 * term1;
    }
    const ScalarType term2 = this->GetOffset()[i] - other->GetOffset()[i];
    result += term2 * term2;
  }
  return std::sqrt(result);
}

template <typename TParametersValueType, unsigned int VDimension>
auto
AffineTransform<TParametersValueType, VDimension>::Metric() const -> ScalarType
{
  ScalarType result = 0.0;
  for (unsigned int i = 0; i < VDimension; ++i)
  {

    for (unsigned int j = 0; j < VDimension; ++j)
    {
      ScalarType term;
      if (i == j)
      {
        term = this->GetMatrix()[i][j] - 1.0;
      }
      else
      {
        term = this->GetMatrix()[i][j];
      }
      result += term * term;
    }
    const ScalarType term2 = this->GetOffset()[i];
    result += term2 * term2;
  }

  return std::sqrt(result);
}
} // namespace itk

#endif
