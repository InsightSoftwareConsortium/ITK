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
#ifndef __itkMatrixOffsetTransformBase_hxx
#define __itkMatrixOffsetTransformBase_hxx

#include "itkNumericTraits.h"
#include "itkMatrixOffsetTransformBase.h"
#include "vnl/algo/vnl_matrix_inverse.h"
#include "itkMath.h"
#include "itkCrossHelper.h"

namespace itk
{
// Constructor with default arguments
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::MatrixOffsetTransformBase() :
  Superclass(ParametersDimension)
{
  m_Matrix.SetIdentity();
  m_MatrixMTime.Modified();
  m_Offset.Fill(0);
  m_Center.Fill(0);
  m_Translation.Fill(0);
  m_Singular = false;
  m_InverseMatrix.SetIdentity();
  m_InverseMatrixMTime = m_MatrixMTime;
  this->m_FixedParameters.SetSize(NInputDimensions);
  this->m_FixedParameters.Fill(0.0);
}

// Constructor with default arguments
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::MatrixOffsetTransformBase(unsigned int paramDims) :
  Superclass(paramDims)
{
  m_Matrix.SetIdentity();
  m_MatrixMTime.Modified();
  m_Offset.Fill(0);
  m_Center.Fill(0);
  m_Translation.Fill(0);
  m_Singular = false;
  m_InverseMatrix.SetIdentity();
  m_InverseMatrixMTime = m_MatrixMTime;
}

// Constructor with explicit arguments
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::MatrixOffsetTransformBase(const MatrixType & matrix, const OutputVectorType & offset)
{
  m_Matrix = matrix;
  m_MatrixMTime.Modified();
  m_Offset = offset;
  m_Center.Fill(0);
  m_Translation.Fill(0);
  for( unsigned int i = 0; i < NOutputDimensions; i++ )
    {
    m_Translation[i] = offset[i];
    }
  this->ComputeMatrixParameters();
}

// Destructor
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::~MatrixOffsetTransformBase()
{
}

// Print self
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
void
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  unsigned int i, j;

  os << indent << "Matrix: " << std::endl;
  for( i = 0; i < NInputDimensions; i++ )
    {
    os << indent.GetNextIndent();
    for( j = 0; j < NOutputDimensions; j++ )
      {
      os << m_Matrix[i][j] << " ";
      }
    os << std::endl;
    }

  os << indent << "Offset: " << m_Offset << std::endl;
  os << indent << "Center: " << m_Center << std::endl;
  os << indent << "Translation: " << m_Translation << std::endl;

  os << indent << "Inverse: " << std::endl;
  for( i = 0; i < NInputDimensions; i++ )
    {
    os << indent.GetNextIndent();
    for( j = 0; j < NOutputDimensions; j++ )
      {
      os << this->GetInverseMatrix()[i][j] << " ";
      }
    os << std::endl;
    }
  os << indent << "Singular: " << m_Singular << std::endl;
}

// Constructor with explicit arguments
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
void
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::SetIdentity(void)
{
  m_Matrix.SetIdentity();
  m_MatrixMTime.Modified();
  m_Offset.Fill(NumericTraits<OutputVectorValueType>::Zero);
  m_Translation.Fill(NumericTraits<OutputVectorValueType>::Zero);
  m_Center.Fill(NumericTraits<InputPointValueType>::Zero);
  m_Singular = false;
  m_InverseMatrix.SetIdentity();
  m_InverseMatrixMTime = m_MatrixMTime;
  this->Modified();
}

// Compose with another affine transformation
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
void
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::Compose(const Self *other, bool pre)
{
  if( pre )
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

// Transform a point
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
typename MatrixOffsetTransformBase<TScalarType,
                                   NInputDimensions,
                                   NOutputDimensions>::OutputPointType
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::TransformPoint(const InputPointType & point) const
{
  return m_Matrix * point + m_Offset;
}

// Transform a vector
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
typename MatrixOffsetTransformBase<TScalarType,
                                   NInputDimensions,
                                   NOutputDimensions>::OutputVectorType
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::TransformVector(const InputVectorType & vect) const
{
  return m_Matrix * vect;
}

// Transform a vnl_vector_fixed
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
typename MatrixOffsetTransformBase<TScalarType,
                                   NInputDimensions,
                                   NOutputDimensions>::OutputVnlVectorType
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::TransformVector(const InputVnlVectorType & vect) const
{
  return m_Matrix * vect;
}

// Transform a variable length vector
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
typename MatrixOffsetTransformBase<TScalarType,
                                   NInputDimensions,
                                   NOutputDimensions>::OutputVectorPixelType
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::TransformVector(const InputVectorPixelType & vect) const
{
  const unsigned int vectorDim = vect.Size();

  vnl_vector<TScalarType> vnl_vect( vectorDim );
  vnl_matrix<TScalarType> vnl_mat( vectorDim, vect.Size(), 0.0 );
  for( unsigned int i = 0; i < vectorDim; i++ )
    {
    vnl_vect[i] = vect[i];
    for( unsigned int j = 0; j < vectorDim; j++ )
      {
      if( (i < NInputDimensions) && (j < NInputDimensions) )
        {
        vnl_mat(i, j) = m_Matrix(i, j);
        }
      else if( i == j )
        {
        vnl_mat(i, j) = 1.0;
        }
      }
    }

  vnl_vector<TScalarType> tvect = vnl_mat * vnl_vect;
  OutputVectorPixelType   outVect;
  outVect.SetSize( vectorDim );
  for( unsigned int i = 0; i < vectorDim; i++ )
    {
    outVect[i] = tvect(i);
    }

  return outVect;
}

// Transform a CovariantVector
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
typename MatrixOffsetTransformBase<TScalarType,
                                   NInputDimensions,
                                   NOutputDimensions>::OutputCovariantVectorType
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::TransformCovariantVector(const InputCovariantVectorType & vec) const
{
  OutputCovariantVectorType result;     // Converted vector

  for( unsigned int i = 0; i < NOutputDimensions; i++ )
    {
    result[i] = NumericTraits<ScalarType>::Zero;
    for( unsigned int j = 0; j < NInputDimensions; j++ )
      {
      result[i] += this->GetInverseMatrix()[j][i] * vec[j]; // Inverse
                                                            // transposed
      }
    }
  return result;
}

// Transform a variable length vector
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
typename MatrixOffsetTransformBase<TScalarType,
                                   NInputDimensions,
                                   NOutputDimensions>::OutputVectorPixelType
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::TransformCovariantVector(const InputVectorPixelType & vect) const
{

  const unsigned int vectorDim = vect.Size();

  vnl_vector<TScalarType> vnl_vect( vectorDim );
  vnl_matrix<TScalarType> vnl_mat( vectorDim, vect.Size(), 0.0 );
  for( unsigned int i = 0; i < vectorDim; i++ )
    {
    vnl_vect[i] = vect[i];
    for( unsigned int j = 0; j < vectorDim; j++ )
      {
      if( (i < NInputDimensions) && (j < NInputDimensions) )
        {
        vnl_mat(i, j) = this->GetInverseMatrix() (j, i);
        }
      else if( i == j )
        {
        vnl_mat(i, j) = 1.0;
        }
      }
    }

  vnl_vector<TScalarType> tvect = vnl_mat * vnl_vect;
  OutputVectorPixelType   outVect;
  outVect.SetSize( vectorDim );
  for( unsigned int i = 0; i < vectorDim; i++ )
    {
    outVect[i] = tvect(i);
    }

  return outVect;
}

// Transform a Tensor
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
typename MatrixOffsetTransformBase<TScalarType,
                                   NInputDimensions,
                                   NOutputDimensions>::OutputDiffusionTensor3DType
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::TransformDiffusionTensor3D(const InputDiffusionTensor3DType & tensor) const
{

  JacobianType jacobian;
  jacobian.SetSize( InverseMatrixType::RowDimensions, InverseMatrixType::ColumnDimensions );
  for (unsigned int i=0; i<InverseMatrixType::RowDimensions; i++)
    {
    for (unsigned int j=0; j<InverseMatrixType::ColumnDimensions; j++)
      {
      jacobian(i,j) = this->GetInverseMatrix()(i,j);
      }
    }

  OutputDiffusionTensor3DType result
    = this->PreservationOfPrincipalDirectionDiffusionTensor3DReorientation( tensor, jacobian );

  return result;
}

// Transform a Tensor
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
typename MatrixOffsetTransformBase<TScalarType,
                                   NInputDimensions,
                                   NOutputDimensions>::OutputVectorPixelType
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::TransformDiffusionTensor3D(const InputVectorPixelType & tensor) const
{
  OutputVectorPixelType result( InputDiffusionTensor3DType::InternalDimension );     // Converted tensor

  result.Fill( 0.0 );

  InputDiffusionTensor3DType dt(0.0);
  const unsigned int         tDim = tensor.Size();
  for( unsigned int i = 0; i < tDim; i++ )
    {
    dt[i] = tensor[i];
    }

  OutputDiffusionTensor3DType outDT = this->TransformDiffusionTensor3D( dt );
  for( unsigned int i = 0; i < InputDiffusionTensor3DType::InternalDimension; i++ )
    {
    result[i] = outDT[i];
    }

  return result;
}

/**
 * Transform tensor
 */
template <class TScalarType, unsigned int NInputDimensions, unsigned int NOutputDimensions>
typename MatrixOffsetTransformBase<TScalarType,
                                   NInputDimensions,
                                   NOutputDimensions>::OutputSymmetricSecondRankTensorType
MatrixOffsetTransformBase<TScalarType,NInputDimensions,NOutputDimensions>
::TransformSymmetricSecondRankTensor( const InputSymmetricSecondRankTensorType& inputTensor ) const
{

  JacobianType jacobian;
  jacobian.SetSize( NOutputDimensions, NInputDimensions );
  JacobianType invJacobian;
  invJacobian.SetSize( NInputDimensions, NOutputDimensions );
  JacobianType tensor;
  tensor.SetSize( NInputDimensions, NInputDimensions );

  for( unsigned int i = 0; i < NInputDimensions; i++ )
    {
    for( unsigned int j = 0; j < NInputDimensions; j++ )
      {
      tensor(i, j) = inputTensor(i, j);
      }
    }

  for( unsigned int i = 0; i < NInputDimensions; i++ )
    {
    for( unsigned int j = 0; j < NOutputDimensions; j++ )
      {
      jacobian( j, i ) = this->GetMatrix()( j, i );
      invJacobian( i, j ) = this->GetInverseMatrix()( i, j );
      }
    }

  JacobianType outTensor = jacobian * tensor * invJacobian;
  OutputSymmetricSecondRankTensorType outputTensor;

  for( unsigned int i = 0; i < NOutputDimensions; i++ )
    {
    for( unsigned int j = 0; j < NOutputDimensions; j++ )
      {
      outputTensor(i, j) = outTensor(i, j);
      }
    }

  return outputTensor;
}

/**
 * Transform tensor
 */
template <class TScalarType, unsigned int NInputDimensions, unsigned int NOutputDimensions>
typename MatrixOffsetTransformBase<TScalarType,
                                   NInputDimensions,
                                   NOutputDimensions>::OutputVectorPixelType
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::TransformSymmetricSecondRankTensor( const InputVectorPixelType& inputTensor ) const
{

  JacobianType jacobian;
  jacobian.SetSize( NOutputDimensions, NInputDimensions );
  JacobianType invJacobian;
  invJacobian.SetSize( NInputDimensions, NOutputDimensions );
  JacobianType tensor;
  tensor.SetSize( NInputDimensions, NInputDimensions );

  for( unsigned int i = 0; i < NInputDimensions; i++ )
    {
    for( unsigned int j = 0; j < NInputDimensions; j++ )
      {
      tensor(i, j) = inputTensor[j + NInputDimensions*i];
      }
    }

  for( unsigned int i = 0; i < NInputDimensions; i++ )
    {
    for( unsigned int j = 0; j < NOutputDimensions; j++ )
      {
      jacobian( j, i ) = this->GetMatrix()( j, i );
      invJacobian( i, j ) = this->GetInverseMatrix()( i, j );
      }
    }

  JacobianType outTensor = jacobian * tensor * invJacobian;

  OutputVectorPixelType outputTensor;

  for( unsigned int i = 0; i < NOutputDimensions; i++ )
    {
    for( unsigned int j = 0; j < NOutputDimensions; j++ )
      {
      outputTensor[j + NOutputDimensions*i] = outTensor(i, j);
      }
    }

  return outputTensor;
}

// Recompute the inverse matrix (internal)
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
const typename MatrixOffsetTransformBase<TScalarType,
                                         NInputDimensions,
                                         NOutputDimensions>::InverseMatrixType
& MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::GetInverseMatrix(void) const
  {
  // If the transform has been modified we recompute the inverse
  if( m_InverseMatrixMTime != m_MatrixMTime )
    {
    m_Singular = false;
    try
      {
      m_InverseMatrix  = m_Matrix.GetInverse();
      }
    catch( ... )
      {
      m_Singular = true;
      }
    m_InverseMatrixMTime = m_MatrixMTime;
    }

  return m_InverseMatrix;
  }

// return an inverse transformation
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
bool
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::GetInverse(Self *inverse) const
{
  if( !inverse )
    {
    return false;
    }

  this->GetInverseMatrix();
  if( m_Singular )
    {
    return false;
    }

  inverse->m_Matrix         = this->GetInverseMatrix();
  inverse->m_InverseMatrix  = m_Matrix;
  inverse->m_Offset         = -( this->GetInverseMatrix() * m_Offset );
  inverse->ComputeTranslation();
  inverse->ComputeMatrixParameters();

  return true;
}

// Return an inverse of this transform
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
typename MatrixOffsetTransformBase<TScalarType, NInputDimensions,
                                   NOutputDimensions>::InverseTransformBasePointer
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::GetInverseTransform() const
{
  Pointer inv = New();

  return GetInverse(inv) ? inv.GetPointer() : NULL;
}

// Get fixed parameters
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
void
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::SetFixedParameters(const ParametersType & fp)
{
  this->m_FixedParameters = fp;
  InputPointType c;
  typedef typename ParametersType::ValueType ParameterValueType;
  for( unsigned int i = 0; i < NInputDimensions; i++ )
    {
    c[i] = this->m_FixedParameters[i];
    }
  this->SetCenter(c);
}

/** Get the Fixed Parameters. */
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
const typename MatrixOffsetTransformBase<TScalarType,
                                         NInputDimensions,
                                         NOutputDimensions>::ParametersType
& MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::GetFixedParameters(void) const
  {
  this->m_FixedParameters.SetSize(NInputDimensions);
  for( unsigned int i = 0; i < NInputDimensions; i++ )
    {
    this->m_FixedParameters[i] = this->m_Center[i];
    }
  return this->m_FixedParameters;
  }

// Get parameters
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
const typename MatrixOffsetTransformBase<TScalarType,
                                         NInputDimensions,
                                         NOutputDimensions>::ParametersType
& MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::GetParameters(void) const
  {
  // Transfer the linear part
  unsigned int par = 0;
  for( unsigned int row = 0; row < NOutputDimensions; row++ )
    {
    for( unsigned int col = 0; col < NInputDimensions; col++ )
      {
      this->m_Parameters[par] = m_Matrix[row][col];
      ++par;
      }
    }
  // Transfer the constant part
  for( unsigned int i = 0; i < NOutputDimensions; i++ )
    {
    this->m_Parameters[par] = m_Translation[i];
    ++par;
    }

  return this->m_Parameters;
  }

// Set parameters
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
void
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::SetParameters(const ParametersType & parameters)
{
  if( parameters.Size() <
      ( NOutputDimensions * NInputDimensions + NOutputDimensions ) )
    {
    itkExceptionMacro
      (<< "Error setting parameters: parameters array size ("
       << parameters.Size() << ") is less than expected "
       << " (NInputDimensions * NOutputDimensions + NOutputDimensions) "
       << " (" << NInputDimensions << " * " << NOutputDimensions
       << " + " << NOutputDimensions
       << " = " << NInputDimensions * NOutputDimensions + NOutputDimensions << ")"
      );
    }

  unsigned int par = 0;

  // Save parameters. Needed for proper operation of TransformUpdateParameters.
  if( &parameters != &(this->m_Parameters) )
    {
    this->m_Parameters = parameters;
    }
  for( unsigned int row = 0; row < NOutputDimensions; row++ )
    {
    for( unsigned int col = 0; col < NInputDimensions; col++ )
      {
      m_Matrix[row][col] = this->m_Parameters[par];
      ++par;
      }
    }
  // Transfer the constant part
  for( unsigned int i = 0; i < NOutputDimensions; i++ )
    {
    m_Translation[i] = this->m_Parameters[par];
    ++par;
    }

  m_MatrixMTime.Modified();

  this->ComputeMatrix();  // Not necessary since parameters explicitly define
                          //    the matrix
  this->ComputeOffset();

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();
}

// Compute the Jacobian in one position
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
void
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const
{
  // This will not reallocate memory if the dimensions are equal
  // to the matrix's current dimensions.
  jacobian.SetSize( NOutputDimensions, this->GetNumberOfLocalParameters() );
  jacobian.Fill(0.0);

  // The Jacobian of the affine transform is composed of
  // subblocks of diagonal matrices, each one of them having
  // a constant value in the diagonal.
  const InputVectorType v = p - this->GetCenter();

  unsigned int blockOffset = 0;
  for( unsigned int block = 0; block < NInputDimensions; block++ )
    {
    for( unsigned int dim = 0; dim < NOutputDimensions; dim++ )
      {
      jacobian(block, blockOffset + dim) = v[dim];
      }

    blockOffset += NInputDimensions;
    }
  for( unsigned int dim = 0; dim < NOutputDimensions; dim++ )
    {
    jacobian(dim, blockOffset + dim) = 1.0;
    }
}

// Return jacobian with respect to position.
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
void
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::ComputeJacobianWithRespectToPosition(const InputPointType  &,
                                       JacobianType & jac) const
{
  jac.SetSize( MatrixType::RowDimensions, MatrixType::ColumnDimensions );
  for( unsigned int i = 0; i < MatrixType::RowDimensions; i++ )
    {
    for( unsigned int j = 0; j < MatrixType::ColumnDimensions; j++ )
      {
      jac[i][j] = this->GetMatrix()[i][j];
      }
    }
}

// Return jacobian with respect to position.
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
void
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::ComputeInverseJacobianWithRespectToPosition(const InputPointType  &,
                                       JacobianType & jac) const
{
  jac.SetSize( MatrixType::ColumnDimensions, MatrixType::RowDimensions );
  for( unsigned int i = 0; i < MatrixType::ColumnDimensions; i++ )
    {
    for( unsigned int j = 0; j < MatrixType::RowDimensions; j++ )
      {
      jac[i][j] = this->GetInverseMatrix()[i][j];
      }
    }
}

// Computes offset based on center, matrix, and translation variables
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
void
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::ComputeOffset(void)
{
  const MatrixType & matrix = this->GetMatrix();

  OffsetType offset;

  for( unsigned int i = 0; i < NOutputDimensions; i++ )
    {
    offset[i] = m_Translation[i] + m_Center[i];
    for( unsigned int j = 0; j < NInputDimensions; j++ )
      {
      offset[i] -= matrix[i][j] * m_Center[j];
      }
    }

  m_Offset = offset;
}

// Computes translation based on offset, matrix, and center
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
void
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::ComputeTranslation(void)
{
  const MatrixType & matrix = this->GetMatrix();

  OffsetType translation;

  for( unsigned int i = 0; i < NOutputDimensions; i++ )
    {
    translation[i] = m_Offset[i] - m_Center[i];
    for( unsigned int j = 0; j < NInputDimensions; j++ )
      {
      translation[i] += matrix[i][j] * m_Center[j];
      }
    }

  m_Translation = translation;
}

// Computes matrix - base class does nothing.  In derived classes is
//    used to convert, for example, versor into a matrix
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
void
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::ComputeMatrix(void)
{
  // Since parameters explicitly define the matrix in this base class, this
  // function does nothing.  Normally used to compute a matrix when
  // its parameterization (e.g., the class' versor) is modified.
}

// Computes parameters - base class does nothing.  In derived classes is
//    used to convert, for example, matrix into a versor
template <class TScalarType, unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
void
MatrixOffsetTransformBase<TScalarType, NInputDimensions, NOutputDimensions>
::ComputeMatrixParameters(void)
{
  // Since parameters explicitly define the matrix in this base class, this
  // function does nothing.  Normally used to update the parameterization
  // of the matrix (e.g., the class' versor) when the matrix is explicitly
  // set.
}

} // namespace

#endif
