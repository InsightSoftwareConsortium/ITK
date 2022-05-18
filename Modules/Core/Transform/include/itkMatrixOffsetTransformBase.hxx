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
#ifndef itkMatrixOffsetTransformBase_hxx
#define itkMatrixOffsetTransformBase_hxx

#include "itkNumericTraits.h"
#include "vnl/algo/vnl_matrix_inverse.h"
#include "itkMath.h"
#include "itkCrossHelper.h"

namespace itk
{

template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::MatrixOffsetTransformBase(
  unsigned int paramDims)
  : Superclass(paramDims)
{
  m_MatrixMTime.Modified();
  m_InverseMatrixMTime = m_MatrixMTime;
  this->m_FixedParameters.SetSize(VInputDimension);
  this->m_FixedParameters.Fill(0.0);
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::MatrixOffsetTransformBase(
  const MatrixType &       matrix,
  const OutputVectorType & offset)
  : m_Matrix(matrix)
  , m_Offset(offset)
{
  m_MatrixMTime.Modified();
  std::copy_n(offset.begin(), VOutputDimension, m_Translation.begin());
  this->ComputeMatrixParameters();
}

template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
void
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::PrintSelf(std::ostream & os,
                                                                                              Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  unsigned int i, j;

  os << indent << "Matrix: " << std::endl;
  for (i = 0; i < VInputDimension; ++i)
  {
    os << indent.GetNextIndent();
    for (j = 0; j < VOutputDimension; ++j)
    {
      os << m_Matrix[i][j] << " ";
    }
    os << std::endl;
  }

  os << indent << "Offset: " << m_Offset << std::endl;
  os << indent << "Center: " << m_Center << std::endl;
  os << indent << "Translation: " << m_Translation << std::endl;

  os << indent << "Inverse: " << std::endl;
  for (i = 0; i < VInputDimension; ++i)
  {
    os << indent.GetNextIndent();
    for (j = 0; j < VOutputDimension; ++j)
    {
      os << this->GetInverseMatrix()[i][j] << " ";
    }
    os << std::endl;
  }
  os << indent << "Singular: " << m_Singular << std::endl;
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
void
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::SetIdentity()
{
  m_Matrix.SetIdentity();
  m_MatrixMTime.Modified();
  m_Offset.Fill(NumericTraits<OutputVectorValueType>::ZeroValue());
  m_Translation.Fill(NumericTraits<OutputVectorValueType>::ZeroValue());
  m_Center.Fill(NumericTraits<InputPointValueType>::ZeroValue());
  m_Singular = false;
  m_InverseMatrix.SetIdentity();
  m_InverseMatrixMTime = m_MatrixMTime;
  this->Modified();
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
void
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::Compose(const Self * other,
                                                                                            bool         pre)
{
  if (pre)
  {
    m_Offset = m_Matrix * other->m_Offset + m_Offset;
    m_Matrix = m_Matrix * other->m_Matrix;
  }
  else
  {
    m_Offset = other->m_Matrix * m_Offset + other->m_Offset;
    m_Matrix = other->m_Matrix * m_Matrix;
  }

  this->ComputeTranslation();
  this->ComputeMatrixParameters();

  m_MatrixMTime.Modified();
  this->Modified();
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
typename MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::OutputPointType
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::TransformPoint(
  const InputPointType & point) const
{
  return m_Matrix * point + m_Offset;
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
typename MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::OutputVectorType
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::TransformVector(
  const InputVectorType & vect) const
{
  return m_Matrix * vect;
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
typename MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::OutputVnlVectorType
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::TransformVector(
  const InputVnlVectorType & vect) const
{
  return m_Matrix.GetVnlMatrix() * vect;
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
typename MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::OutputVectorPixelType
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::TransformVector(
  const InputVectorPixelType & vect) const
{
  const unsigned int vectorDim = vect.Size();

  vnl_vector<TParametersValueType> vnl_vect(vectorDim);
  vnl_matrix<TParametersValueType> vnl_mat(vectorDim, vect.Size(), 0.0);
  for (unsigned int i = 0; i < vectorDim; ++i)
  {
    vnl_vect[i] = vect[i];
    for (unsigned int j = 0; j < vectorDim; ++j)
    {
      if ((i < VInputDimension) && (j < VInputDimension))
      {
        vnl_mat(i, j) = m_Matrix(i, j);
      }
      else if (i == j)
      {
        vnl_mat(i, j) = 1.0;
      }
    }
  }

  vnl_vector<TParametersValueType> tvect = vnl_mat * vnl_vect;
  OutputVectorPixelType            outVect;
  outVect.SetSize(vectorDim);
  for (unsigned int i = 0; i < vectorDim; ++i)
  {
    outVect[i] = tvect(i);
  }

  return outVect;
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
typename MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::OutputCovariantVectorType
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::TransformCovariantVector(
  const InputCovariantVectorType & vec) const
{
  OutputCovariantVectorType result; // Converted vector

  for (unsigned int i = 0; i < VOutputDimension; ++i)
  {
    result[i] = NumericTraits<ScalarType>::ZeroValue();
    for (unsigned int j = 0; j < VInputDimension; ++j)
    {
      result[i] += this->GetInverseMatrix()[j][i] * vec[j]; // Inverse
                                                            // transposed
    }
  }
  return result;
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
typename MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::OutputVectorPixelType
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::TransformCovariantVector(
  const InputVectorPixelType & vect) const
{

  const unsigned int vectorDim = vect.Size();

  vnl_vector<TParametersValueType> vnl_vect(vectorDim);
  vnl_matrix<TParametersValueType> vnl_mat(vectorDim, vect.Size(), 0.0);
  for (unsigned int i = 0; i < vectorDim; ++i)
  {
    vnl_vect[i] = vect[i];
    for (unsigned int j = 0; j < vectorDim; ++j)
    {
      if ((i < VInputDimension) && (j < VInputDimension))
      {
        vnl_mat(i, j) = this->GetInverseMatrix()(j, i);
      }
      else if (i == j)
      {
        vnl_mat(i, j) = 1.0;
      }
    }
  }

  vnl_vector<TParametersValueType> tvect = vnl_mat * vnl_vect;
  OutputVectorPixelType            outVect;
  outVect.SetSize(vectorDim);
  for (unsigned int i = 0; i < vectorDim; ++i)
  {
    outVect[i] = tvect(i);
  }

  return outVect;
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
typename MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::OutputDiffusionTensor3DType
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::TransformDiffusionTensor3D(
  const InputDiffusionTensor3DType & tensor) const
{

  JacobianType jacobian;
  jacobian.SetSize(InverseMatrixType::RowDimensions, InverseMatrixType::ColumnDimensions);
  for (unsigned int i = 0; i < InverseMatrixType::RowDimensions; ++i)
  {
    for (unsigned int j = 0; j < InverseMatrixType::ColumnDimensions; ++j)
    {
      jacobian(i, j) = this->GetInverseMatrix()(i, j);
    }
  }

  OutputDiffusionTensor3DType result =
    this->PreservationOfPrincipalDirectionDiffusionTensor3DReorientation(tensor, jacobian);

  return result;
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
typename MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::OutputVectorPixelType
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::TransformDiffusionTensor3D(
  const InputVectorPixelType & tensor) const
{
  OutputVectorPixelType result(InputDiffusionTensor3DType::InternalDimension); // Converted tensor

  result.Fill(0.0);

  InputDiffusionTensor3DType dt(0.0);
  const unsigned int         tDim = tensor.Size();
  for (unsigned int i = 0; i < tDim; ++i)
  {
    dt[i] = tensor[i];
  }

  OutputDiffusionTensor3DType outDT = this->TransformDiffusionTensor3D(dt);
  for (unsigned int i = 0; i < InputDiffusionTensor3DType::InternalDimension; ++i)
  {
    result[i] = outDT[i];
  }

  return result;
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
typename MatrixOffsetTransformBase<TParametersValueType,
                                   VInputDimension,
                                   VOutputDimension>::OutputSymmetricSecondRankTensorType
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::TransformSymmetricSecondRankTensor(
  const InputSymmetricSecondRankTensorType & inputTensor) const
{
  JacobianType jacobian;
  jacobian.SetSize(VOutputDimension, VInputDimension);
  JacobianType invJacobian;
  invJacobian.SetSize(VInputDimension, VOutputDimension);
  JacobianType tensor;
  tensor.SetSize(VInputDimension, VInputDimension);

  for (unsigned int i = 0; i < VInputDimension; ++i)
  {
    for (unsigned int j = 0; j < VInputDimension; ++j)
    {
      tensor(i, j) = inputTensor(i, j);
    }
  }

  for (unsigned int i = 0; i < VInputDimension; ++i)
  {
    for (unsigned int j = 0; j < VOutputDimension; ++j)
    {
      jacobian(j, i) = this->GetMatrix()(j, i);
      invJacobian(i, j) = this->GetInverseMatrix()(i, j);
    }
  }

  JacobianType                        outTensor = jacobian * tensor * invJacobian;
  OutputSymmetricSecondRankTensorType outputTensor;

  for (unsigned int i = 0; i < VOutputDimension; ++i)
  {
    for (unsigned int j = 0; j < VOutputDimension; ++j)
    {
      outputTensor(i, j) = outTensor(i, j);
    }
  }

  return outputTensor;
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
typename MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::OutputVectorPixelType
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::TransformSymmetricSecondRankTensor(
  const InputVectorPixelType & inputTensor) const
{
  JacobianType jacobian;
  jacobian.SetSize(VOutputDimension, VInputDimension);
  JacobianType invJacobian;
  invJacobian.SetSize(VInputDimension, VOutputDimension);
  JacobianType tensor;
  tensor.SetSize(VInputDimension, VInputDimension);

  for (unsigned int i = 0; i < VInputDimension; ++i)
  {
    for (unsigned int j = 0; j < VInputDimension; ++j)
    {
      tensor(i, j) = inputTensor[j + VInputDimension * i];
    }
  }

  for (unsigned int i = 0; i < VInputDimension; ++i)
  {
    for (unsigned int j = 0; j < VOutputDimension; ++j)
    {
      jacobian(j, i) = this->GetMatrix()(j, i);
      invJacobian(i, j) = this->GetInverseMatrix()(i, j);
    }
  }

  JacobianType outTensor = jacobian * tensor * invJacobian;

  OutputVectorPixelType outputTensor;

  for (unsigned int i = 0; i < VOutputDimension; ++i)
  {
    for (unsigned int j = 0; j < VOutputDimension; ++j)
    {
      outputTensor[j + VOutputDimension * i] = outTensor(i, j);
    }
  }

  return outputTensor;
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
auto
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::GetInverseMatrix() const
  -> const InverseMatrixType &
{
  // If the transform has been modified we recompute the inverse
  if (m_InverseMatrixMTime != m_MatrixMTime)
  {
    m_Singular = false;
    try
    {
      m_InverseMatrix = m_Matrix.GetInverse();
    }
    catch (...)
    {
      m_Singular = true;
    }
    m_InverseMatrixMTime = m_MatrixMTime;
  }

  return m_InverseMatrix;
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
bool
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::GetInverse(
  InverseTransformType * inverse) const
{
  if (!inverse)
  {
    return false;
  }

  inverse->SetFixedParameters(this->GetFixedParameters());
  this->GetInverseMatrix();
  if (m_Singular)
  {
    return false;
  }

  inverse->m_Matrix = this->GetInverseMatrix();
  inverse->m_InverseMatrix = m_Matrix;
  inverse->m_Offset = -(this->GetInverseMatrix() * m_Offset);
  inverse->ComputeTranslation();
  inverse->ComputeMatrixParameters();

  return true;
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
typename MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::InverseTransformBasePointer
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::GetInverseTransform() const
{
  auto inv = InverseTransformType::New();

  return GetInverse(inv) ? inv.GetPointer() : nullptr;
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
void
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::SetFixedParameters(
  const FixedParametersType & fp)
{
  if (fp.size() < VInputDimension)
  {
    itkExceptionMacro(<< "Error setting fixed parameters: parameters array size (" << fp.size()
                      << ") is less than expected  (VInputDimension = " << VInputDimension << ")");
  }
  this->m_FixedParameters = fp;
  InputPointType c;
  for (unsigned int i = 0; i < VInputDimension; ++i)
  {
    c[i] = this->m_FixedParameters[i];
  }
  this->SetCenter(c);
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
const typename MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::FixedParametersType &
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::GetFixedParameters() const
{
  for (unsigned int i = 0; i < VInputDimension; ++i)
  {
    this->m_FixedParameters[i] = this->m_Center[i];
  }
  return this->m_FixedParameters;
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
auto
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::GetParameters() const
  -> const ParametersType &
{
  // Transfer the linear part
  unsigned int par = 0;
  for (unsigned int row = 0; row < VOutputDimension; ++row)
  {
    for (unsigned int col = 0; col < VInputDimension; ++col)
    {
      this->m_Parameters[par] = m_Matrix[row][col];
      ++par;
    }
  }
  // Transfer the constant part
  for (unsigned int i = 0; i < VOutputDimension; ++i)
  {
    this->m_Parameters[par] = m_Translation[i];
    ++par;
  }

  return this->m_Parameters;
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
void
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::SetParameters(
  const ParametersType & parameters)
{
  if (parameters.Size() < (VOutputDimension * VInputDimension + VOutputDimension))
  {
    itkExceptionMacro(<< "Error setting parameters: parameters array size (" << parameters.Size()
                      << ") is less than expected "
                      << " (VInputDimension * VOutputDimension + VOutputDimension) "
                      << " (" << VInputDimension << " * " << VOutputDimension << " + " << VOutputDimension << " = "
                      << VInputDimension * VOutputDimension + VOutputDimension << ")");
  }

  unsigned int par = 0;

  // Save parameters. Needed for proper operation of TransformUpdateParameters.
  if (&parameters != &(this->m_Parameters))
  {
    this->m_Parameters = parameters;
  }
  for (unsigned int row = 0; row < VOutputDimension; ++row)
  {
    for (unsigned int col = 0; col < VInputDimension; ++col)
    {
      m_Matrix[row][col] = this->m_Parameters[par];
      ++par;
    }
  }
  // Transfer the constant part
  for (unsigned int i = 0; i < VOutputDimension; ++i)
  {
    m_Translation[i] = this->m_Parameters[par];
    ++par;
  }

  m_MatrixMTime.Modified();

  this->ComputeMatrix(); // Not necessary since parameters explicitly define
                         //    the matrix
  this->ComputeOffset();

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();
}

template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
void
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::
  ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const
{
  // This will not reallocate memory if the dimensions are equal
  // to the matrix's current dimensions.
  jacobian.SetSize(VOutputDimension, this->GetNumberOfLocalParameters());
  jacobian.Fill(0.0);

  // The Jacobian of the affine transform is composed of
  // subblocks of diagonal matrices, each one of them having
  // a constant value in the diagonal.
  const InputVectorType v = p - this->GetCenter();

  unsigned int blockOffset = 0;
  for (unsigned int block = 0; block < VInputDimension; ++block)
  {
    for (unsigned int dim = 0; dim < VOutputDimension; ++dim)
    {
      jacobian(block, blockOffset + dim) = v[dim];
    }

    blockOffset += VInputDimension;
  }
  for (unsigned int dim = 0; dim < VOutputDimension; ++dim)
  {
    jacobian(dim, blockOffset + dim) = 1.0;
  }
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
void
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::
  ComputeJacobianWithRespectToPosition(const InputPointType &, JacobianPositionType & jac) const
{
  jac = this->GetMatrix().GetVnlMatrix();
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
void
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::
  ComputeInverseJacobianWithRespectToPosition(const InputPointType &, InverseJacobianPositionType & jac) const
{
  jac = this->GetInverseMatrix().GetVnlMatrix();
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
void
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::ComputeOffset()
{
  const MatrixType & matrix = this->GetMatrix();

  OffsetType offset;

  for (unsigned int i = 0; i < VOutputDimension; ++i)
  {
    offset[i] = m_Translation[i] + m_Center[i];
    for (unsigned int j = 0; j < VInputDimension; ++j)
    {
      offset[i] -= matrix[i][j] * m_Center[j];
    }
  }

  m_Offset = offset;
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
void
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::ComputeTranslation()
{
  const MatrixType & matrix = this->GetMatrix();

  OffsetType translation;

  for (unsigned int i = 0; i < VOutputDimension; ++i)
  {
    translation[i] = m_Offset[i] - m_Center[i];
    for (unsigned int j = 0; j < VInputDimension; ++j)
    {
      translation[i] += matrix[i][j] * m_Center[j];
    }
  }

  m_Translation = translation;
}

template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
void
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::ComputeMatrix()
{
  // Since parameters explicitly define the matrix in this base class, this
  // function does nothing.  Normally used to compute a matrix when
  // its parameterization (e.g., the class' versor) is modified.
}


template <typename TParametersValueType, unsigned int VInputDimension, unsigned int VOutputDimension>
void
MatrixOffsetTransformBase<TParametersValueType, VInputDimension, VOutputDimension>::ComputeMatrixParameters()
{
  // Since parameters explicitly define the matrix in this base class, this
  // function does nothing.  Normally used to update the parameterization
  // of the matrix (e.g., the class' versor) when the matrix is explicitly
  // set.
}

} // end namespace itk

#endif
