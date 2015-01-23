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
#ifndef itkTransform_hxx
#define itkTransform_hxx

#include "itkTransform.h"
#include "itkCrossHelper.h"
#include "vnl/algo/vnl_matrix_inverse.h"

namespace itk
{

template <typename TScalar,
          unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
Transform<TScalar, NInputDimensions, NOutputDimensions>
::Transform() :
  m_Parameters(1),
  m_FixedParameters()
#ifdef ITKV3_COMPATIBILITY
  , m_SharedLocalJacobian(NOutputDimensions, 1)
#endif
{
  itkWarningMacro(
    << "Using default transform constructor.  Should specify NOutputDims and NParameters as args to constructor.");
}


template <typename TScalar,
          unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
Transform<TScalar, NInputDimensions, NOutputDimensions>
::Transform(NumberOfParametersType numberOfParameters) :
  m_Parameters(numberOfParameters),
  m_FixedParameters()
#ifdef ITKV3_COMPATIBILITY
  , m_SharedLocalJacobian(NOutputDimensions, numberOfParameters)
#endif
{
}


template <typename TScalar,
          unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
std::string Transform<TScalar, NInputDimensions, NOutputDimensions>
::GetTransformTypeAsString() const
{
  std::ostringstream n;

  n << GetNameOfClass();
  n << "_";
  n << this->GetTransformTypeAsString(static_cast<TScalar *>(ITK_NULLPTR) );
  n << "_" << this->GetInputSpaceDimension() << "_" << this->GetOutputSpaceDimension();
  return n.str();
}


template <typename TScalar,
          unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
typename LightObject::Pointer
Transform<TScalar, NInputDimensions, NOutputDimensions>
::InternalClone() const
{
  // Default implementation just copies the parameters from
  // this to new transform.
  typename LightObject::Pointer loPtr = Superclass::InternalClone();

  typename Self::Pointer rval =
    dynamic_cast<Self *>(loPtr.GetPointer());
  if(rval.IsNull())
    {
    itkExceptionMacro(<< "downcast to type "
                      << this->GetNameOfClass()
                      << " failed.");
    }
  rval->SetFixedParameters(this->GetFixedParameters());
  rval->SetParameters(this->GetParameters());
  return loPtr;
}


template <typename TScalar,
          unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
void
Transform<TScalar, NInputDimensions, NOutputDimensions>
::UpdateTransformParameters( const DerivativeType & update, TScalar factor )
{
  NumberOfParametersType numberOfParameters = this->GetNumberOfParameters();

  if( update.Size() != numberOfParameters )
    {
    itkExceptionMacro("Parameter update size, " << update.Size() << ", must "
                      " be same as transform parameter size, "
                                                << numberOfParameters << std::endl);
    }

  /* Make sure m_Parameters is updated to reflect the current values in
   * the transform's other parameter-related variables. This is effective for
   * managing the parallel variables used for storing parameter data,
   * but inefficient. However for small global transforms, shouldn't be
   * too bad. Dense-field transform will want to make sure m_Parameters
   * is always updated whenever the transform is changed, so GetParameters
   * can be skipped in their implementations of UpdateTransformParameters. */
  this->GetParameters();

  if( factor == 1.0 )
    {
    for( NumberOfParametersType k = 0; k < numberOfParameters; k++ )
      {
      this->m_Parameters[k] += update[k];
      }
    }
  else
    {
    for( NumberOfParametersType k = 0; k < numberOfParameters; k++ )
      {
      this->m_Parameters[k] += update[k] * factor;
      }
    }

  /* Call SetParameters with the updated parameters.
   * SetParameters in most transforms is used to assign the input params
   * to member variables, possibly with some processing. The member variables
   * are then used in TransformPoint.
   * In the case of dense-field transforms that are updated in blocks from
   * a threaded implementation, SetParameters doesn't do this, and is
   * optimized to not copy the input parameters when == m_Parameters.
   */
  this->SetParameters( this->m_Parameters );

  /* Call Modified, following behavior of other transform when their
   * parameters change, e.g. MatrixOffsetTransformBase */
  this->Modified();
}


template <typename TScalar,
          unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
typename Transform<TScalar, NInputDimensions, NOutputDimensions>::OutputVectorType
Transform<TScalar, NInputDimensions, NOutputDimensions>
::TransformVector( const InputVectorType& vector, const InputPointType & point ) const
{
  JacobianType jacobian;
  this->ComputeJacobianWithRespectToPosition( point, jacobian );
  OutputVectorType result;
  for( unsigned int i = 0; i < NOutputDimensions; i++ )
    {
    result[i] = NumericTraits<ScalarType>::ZeroValue();
    for( unsigned int j = 0; j < NInputDimensions; j++ )
      {
      result[i] += jacobian[i][j] * vector[j];
      }
    }

  return result;
}


template <typename TScalar,
          unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
typename Transform<TScalar, NInputDimensions, NOutputDimensions>::OutputVnlVectorType
Transform<TScalar, NInputDimensions, NOutputDimensions>
::TransformVector( const InputVnlVectorType& vector, const InputPointType & point ) const
{
  JacobianType jacobian;
  this->ComputeJacobianWithRespectToPosition( point, jacobian );
  OutputVnlVectorType result;
  for( unsigned int i = 0; i < NOutputDimensions; i++ )
    {
    result[i] = NumericTraits<ScalarType>::ZeroValue();
    for( unsigned int j = 0; j < NInputDimensions; j++ )
      {
      result[i] += jacobian[i][j] * vector[j];
      }
    }

  return result;
}


template <typename TScalar,
          unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
typename Transform<TScalar, NInputDimensions, NOutputDimensions>::OutputVectorPixelType
Transform<TScalar, NInputDimensions, NOutputDimensions>
::TransformVector( const InputVectorPixelType& vector, const InputPointType & point ) const
{

  if ( vector.GetSize() != NInputDimensions )
    {
    itkExceptionMacro( "Input Vector is not of size NInputDimensions = " << NInputDimensions << std::endl );
    }

  JacobianType jacobian;
  this->ComputeJacobianWithRespectToPosition( point, jacobian );

  OutputVectorPixelType result;
  result.SetSize( NOutputDimensions );

  for( unsigned int i = 0; i < NOutputDimensions; i++ )
    {
    result[i] = NumericTraits<ScalarType>::ZeroValue();
    for( unsigned int j = 0; j < NInputDimensions; j++ )
      {
      result[i] += jacobian[i][j] * vector[j];
      }
    }

  return result;
}


template <typename TScalar,
          unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
typename Transform<TScalar, NInputDimensions, NOutputDimensions>::OutputCovariantVectorType
Transform<TScalar, NInputDimensions, NOutputDimensions>
::TransformCovariantVector( const InputCovariantVectorType& vector, const InputPointType & point ) const
{
  JacobianType jacobian;
  this->ComputeInverseJacobianWithRespectToPosition( point, jacobian );
  OutputCovariantVectorType result;
  for( unsigned int i = 0; i < NOutputDimensions; i++ )
    {
    result[i] = NumericTraits<ScalarType>::ZeroValue();
    for( unsigned int j = 0; j < NInputDimensions; j++ )
      {
      result[i] += jacobian[j][i] * vector[j];
      }
    }

  return result;
}


template <typename TScalar,
          unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
typename Transform<TScalar, NInputDimensions, NOutputDimensions>::OutputVectorPixelType
Transform<TScalar, NInputDimensions, NOutputDimensions>
::TransformCovariantVector( const InputVectorPixelType& vector, const InputPointType & point ) const
{

  if ( vector.GetSize() != NInputDimensions )
    {
    itkExceptionMacro( "Input Vector is not of size NInputDimensions = " << NInputDimensions << std::endl );
    }

  JacobianType jacobian;
  this->ComputeInverseJacobianWithRespectToPosition( point, jacobian );

  OutputVectorPixelType result;
  result.SetSize( NOutputDimensions );

  for( unsigned int i = 0; i < NOutputDimensions; i++ )
    {
    result[i] = NumericTraits<ScalarType>::ZeroValue();
    for( unsigned int j = 0; j < NInputDimensions; j++ )
      {
      result[i] += jacobian[j][i] * vector[j];
      }
    }

  return result;
}


template <typename TScalar, unsigned int NInputDimensions, unsigned int NOutputDimensions>
typename Transform<TScalar, NInputDimensions, NOutputDimensions>::OutputDiffusionTensor3DType
Transform<TScalar, NInputDimensions, NOutputDimensions>
::TransformDiffusionTensor3D( const InputDiffusionTensor3DType& inputTensor, const InputPointType & point ) const
{
  JacobianType invJacobian;
  this->ComputeInverseJacobianWithRespectToPosition( point, invJacobian );

  OutputDiffusionTensor3DType result
    = this->PreservationOfPrincipalDirectionDiffusionTensor3DReorientation( inputTensor, invJacobian );

  return result;
}


template <typename TScalar, unsigned int NInputDimensions, unsigned int NOutputDimensions>
typename Transform<TScalar, NInputDimensions, NOutputDimensions>::OutputVectorPixelType
Transform<TScalar, NInputDimensions, NOutputDimensions>
::TransformDiffusionTensor3D( const InputVectorPixelType & inputTensor, const InputPointType & point ) const
{
  if (inputTensor.GetSize() != 6 )
    {
    itkExceptionMacro( "Input DiffusionTensor3D does not have 6 elements" << std::endl );
    }

  InputDiffusionTensor3DType inTensor;
  for (unsigned int i=0; i<5; i++)
    {
    inTensor[i] = inputTensor[i];
    }

  OutputDiffusionTensor3DType outTensor = this->TransformDiffusionTensor3D( inTensor, point );

  OutputVectorPixelType outputTensor;
  outputTensor.SetSize( 6 );
  for (unsigned int i=0; i<5; i++)
    {
    outputTensor[i] = outTensor[i];
    }

  return outputTensor;

}


template <typename TScalar, unsigned int NInputDimensions, unsigned int NOutputDimensions>
typename Transform<TScalar, NInputDimensions, NOutputDimensions>::OutputDiffusionTensor3DType
Transform<TScalar, NInputDimensions, NOutputDimensions>
::PreservationOfPrincipalDirectionDiffusionTensor3DReorientation( const InputDiffusionTensor3DType inputTensor,
                                                                  const JacobianType jacobian ) const
{
  Matrix<TScalar,3,3> matrix;

  matrix.Fill(0.0);
  for( unsigned int i = 0; i < 3; i++ )
    {
    matrix(i, i) = 1.0;
    }

  for( unsigned int i = 0; i < NInputDimensions; i++ )
    {
    for( unsigned int j = 0; j < NOutputDimensions; j++ )
      {
      if( (i < 3) && (j < 3) )
        {
        matrix(i, j) = jacobian(i, j);
        }
      }
    }

  typename InputDiffusionTensor3DType::EigenValuesArrayType eigenValues;
  typename InputDiffusionTensor3DType::EigenVectorsMatrixType eigenVectors;
  inputTensor.ComputeEigenAnalysis( eigenValues, eigenVectors );

  Vector<TScalar,3> ev1;
  Vector<TScalar,3> ev2;
  Vector<TScalar,3> ev3;
  for( unsigned int i = 0; i < 3; i++ )
    {
    ev1[i] = eigenVectors(2, i);
    ev2[i] = eigenVectors(1, i);
    }

  // Account for image direction changes between moving and fixed spaces
  ev1 = matrix * ev1;
  ev1.Normalize();

  // Get aspect of rotated e2 that is perpendicular to rotated e1
  ev2 = matrix * ev2;
  double dp = ev2 * ev1;
  if( dp < 0 )
    {
    ev2 = ev2 * (-1.0);
    dp = dp * (-1.0);
    }
  ev2 = ev2 - ev1 * dp;
  ev2.Normalize();

  CrossHelper< Vector<TScalar,3> > vectorCross;
  ev3 = vectorCross( ev1, ev2 );

  // Outer product matrices
  Matrix<TScalar,3,3> e1;
  Matrix<TScalar,3,3> e2;
  Matrix<TScalar,3,3> e3;
  for( unsigned int i = 0; i < 3; i++ )
    {
    for( unsigned int j = 0; j < 3; j++ )
      {
      e1(i, j) = eigenValues[2] * ev1[i] * ev1[j];
      e2(i, j) = eigenValues[1] * ev2[i] * ev2[j];
      e3(i, j) = eigenValues[0] * ev3[i] * ev3[j];
      }
    }

  Matrix<TScalar,3,3> rotated = e1 + e2 + e3;

  OutputDiffusionTensor3DType result;     // Converted vector
  result[0] = rotated(0, 0);
  result[1] = rotated(0, 1);
  result[2] = rotated(0, 2);
  result[3] = rotated(1, 1);
  result[4] = rotated(1, 2);
  result[5] = rotated(2, 2);

  return result;
}


template <typename TScalar, unsigned int NInputDimensions, unsigned int NOutputDimensions>
typename Transform<TScalar, NInputDimensions, NOutputDimensions>::OutputSymmetricSecondRankTensorType
Transform<TScalar, NInputDimensions, NOutputDimensions>
::TransformSymmetricSecondRankTensor( const InputSymmetricSecondRankTensorType& inputTensor, const InputPointType & point ) const
{
  JacobianType jacobian;
  this->ComputeJacobianWithRespectToPosition( point, jacobian );
  JacobianType invJacobian;
  this->ComputeInverseJacobianWithRespectToPosition( point, invJacobian );
  JacobianType tensor;
  tensor.SetSize( NInputDimensions, NInputDimensions );

  for( unsigned int i = 0; i < NInputDimensions; i++ )
    {
    for( unsigned int j = 0; j < NInputDimensions; j++ )
      {
      tensor(i, j) = inputTensor(i, j);
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


template <typename TScalar, unsigned int NInputDimensions, unsigned int NOutputDimensions>
typename Transform<TScalar, NInputDimensions, NOutputDimensions>::OutputVectorPixelType
Transform<TScalar, NInputDimensions, NOutputDimensions>
::TransformSymmetricSecondRankTensor( const InputVectorPixelType& inputTensor, const InputPointType & point ) const
{

  if (inputTensor.GetSize() != (NInputDimensions*NInputDimensions) )
    {
    itkExceptionMacro( "Input DiffusionTensor3D does not have " << NInputDimensions*NInputDimensions << " elements" << std::endl );
    }

  JacobianType jacobian;
  this->ComputeJacobianWithRespectToPosition( point, jacobian );
  JacobianType invJacobian;
  this->ComputeInverseJacobianWithRespectToPosition( point, invJacobian );
  JacobianType tensor;
  tensor.SetSize( NInputDimensions, NInputDimensions );

  for( unsigned int i = 0; i < NInputDimensions; i++ )
    {
    for( unsigned int j = 0; j < NInputDimensions; j++ )
      {
      tensor(i, j) = inputTensor[j + NInputDimensions*i];
      }
    }

  JacobianType outTensor = jacobian * tensor * invJacobian;

  OutputVectorPixelType outputTensor;
  outputTensor.SetSize( NOutputDimensions*NOutputDimensions );

  for( unsigned int i = 0; i < NOutputDimensions; i++ )
    {
    for( unsigned int j = 0; j < NOutputDimensions; j++ )
      {
      outputTensor[j + NOutputDimensions*i] = outTensor(i, j);
      }
    }

  return outputTensor;
}


template <typename TScalar,
          unsigned int NInputDimensions,
          unsigned int NOutputDimensions>
void
Transform<TScalar, NInputDimensions, NOutputDimensions>
::ComputeInverseJacobianWithRespectToPosition( const InputPointType & pnt, JacobianType & jacobian ) const
{
  JacobianType forward_jacobian;
  this->ComputeJacobianWithRespectToPosition( pnt, forward_jacobian );

  jacobian.SetSize(NInputDimensions, NOutputDimensions);

  vnl_svd<typename JacobianType::ValueType> svd( forward_jacobian );
  for( unsigned int i = 0; i < jacobian.rows(); i++ )
    {
    for( unsigned int j = 0; j < jacobian.cols(); j++ )
      {
      jacobian(i, j) = svd.inverse() (i, j);
      }
    }
}

} // end namespace itk

#endif
