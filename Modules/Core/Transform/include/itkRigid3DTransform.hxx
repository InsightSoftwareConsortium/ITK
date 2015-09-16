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
#ifndef itkRigid3DTransform_hxx
#define itkRigid3DTransform_hxx

#include "itkRigid3DTransform.h"

namespace itk
{
// Constructor with default arguments
template<typename TParametersValueType>
Rigid3DTransform<TParametersValueType>::Rigid3DTransform():
  Superclass(ParametersDimension)
{}

// Constructor with default arguments
template<typename TParametersValueType>
Rigid3DTransform<TParametersValueType>::Rigid3DTransform(unsigned int paramDim):
  Superclass(paramDim)
{}

// Constructor with default arguments
template<typename TParametersValueType>
Rigid3DTransform<TParametersValueType>::Rigid3DTransform(const MatrixType & matrix,
                                                  const OutputVectorType & offset):
  Superclass(matrix, offset)
{}

// Destructor
template<typename TParametersValueType>
Rigid3DTransform<TParametersValueType>::
~Rigid3DTransform()
{}

// Print self
template<typename TParametersValueType>
void
Rigid3DTransform<TParametersValueType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

// Check if input matrix is orthogonal to within tolerance
template<typename TParametersValueType>
bool
Rigid3DTransform<TParametersValueType>
::MatrixIsOrthogonal(
  const MatrixType & matrix,
  const TParametersValueType tolerance)
{
  typename MatrixType::InternalMatrixType test =
    matrix.GetVnlMatrix() * matrix.GetTranspose();

  if ( !test.is_identity(tolerance) )
    {
    return false;
    }

  return true;
}

// Directly set the rotation matrix
template<typename TParametersValueType>
void
Rigid3DTransform<TParametersValueType>
::SetMatrix(const MatrixType & matrix)
{
  const TParametersValueType tolerance = MatrixOrthogonalityTolerance<TParametersValueType>::GetTolerance();
  this->SetMatrix( matrix, tolerance );
}

template<typename TParametersValueType>
void
Rigid3DTransform<TParametersValueType>
::SetMatrix(const MatrixType & matrix, const TParametersValueType tolerance)
{
  if ( !this->MatrixIsOrthogonal(matrix, tolerance) )
    {
    itkExceptionMacro(<< "Attempting to set a non-orthogonal rotation matrix");
    }

  this->Superclass::SetMatrix(matrix);
}

// Set optimizable parameters from array
template<typename TParametersValueType>
void
Rigid3DTransform<TParametersValueType>
::SetParameters(const ParametersType & parameters)
{
  //Save parameters. Needed for proper operation of TransformUpdateParameters.
  if( &parameters != &(this->m_Parameters) )
    {
    this->m_Parameters = parameters;
    }

  unsigned int     par = 0;
  MatrixType       matrix;
  OutputVectorType translation;

  for ( unsigned int row = 0; row < 3; row++ )
    {
    for ( unsigned int col = 0; col < 3; col++ )
      {
      matrix[row][col] = this->m_Parameters[par];
      ++par;
      }
    }

  for ( unsigned int dim = 0; dim < 3; dim++ )
    {
    translation[dim] = this->m_Parameters[par];
    ++par;
    }

  const TParametersValueType tolerance = MatrixOrthogonalityTolerance<TParametersValueType>::GetTolerance();
  if ( !this->MatrixIsOrthogonal(matrix, tolerance) )
    {
    itkExceptionMacro(<< "Attempting to set a non-orthogonal rotation matrix");
    }

  this->SetVarMatrix(matrix);
  this->SetVarTranslation(translation);

  // Update matrix and offset.
  // Technically ComputeMatrix() is not require as the parameters are
  // directly the elements of the matrix.
  this->ComputeMatrix();
  this->ComputeOffset();

  this->Modified();
}

// Compose with a translation
template<typename TParametersValueType>
void
Rigid3DTransform<TParametersValueType>::Translate(const OffsetType & offset, bool)
{
  OutputVectorType newOffset = this->GetOffset();

  newOffset += offset;
  this->SetOffset(newOffset);
  this->ComputeTranslation();
}

#ifdef ITKV3_COMPATIBILITY
#if !defined(ITK_LEGACY_REMOVE)
template<typename TParametersValueType>
bool
Rigid3DTransform<TParametersValueType>::GetInverse(Self *inverse) const
{
  return this->Superclass::GetInverse(inverse);
}

template<typename TParametersValueType>
typename Rigid3DTransform<TParametersValueType>::InverseTransformBasePointer
Rigid3DTransform<TParametersValueType>::GetInverseTransform() const
{
  Pointer inv = New();
  return this->GetInverse(inv) ? inv.GetPointer() : ITK_NULLPTR;
}

template<typename TParametersValueType>
const typename Rigid3DTransform<TParametersValueType>::MatrixType &
Rigid3DTransform<TParametersValueType>::GetRotationMatrix() const
{
  return this->GetMatrix();
}

template<typename TParametersValueType>
void
Rigid3DTransform<TParametersValueType>::SetRotationMatrix(const MatrixType & matrix)
{
  this->SetMatrix(matrix);
}
#endif // end ITK_LEGACY_REMOVE
#endif // ITKV3_COMPATIBILITY

#if !defined(ITK_LEGACY_REMOVE)
// Back transform a point
template<typename TParametersValueType>
typename Rigid3DTransform<TParametersValueType>::InputPointType
Rigid3DTransform<TParametersValueType>::BackTransform(const OutputPointType & point) const
{
  itkWarningMacro(
    << "BackTransform(): This method is slated to be removed from ITK."
    <<
    "Instead, please use GetInverse() to generate an inverse transform and then perform the transform using that inverted transform.");
  return this->GetInverseMatrix() * ( point - this->GetOffset() );
}

// Back transform a vector
template<typename TParametersValueType>
typename Rigid3DTransform<TParametersValueType>::InputVectorType
Rigid3DTransform<TParametersValueType>::BackTransform(const OutputVectorType & vect) const
{
  itkWarningMacro(
    << "BackTransform(): This method is slated to be removed from ITK."
    <<
    "Instead, please use GetInverse() to generate an inverse transform and then perform the transform using that inverted transform.");
  return this->GetInverseMatrix() * vect;
}

// Back transform a vnl_vector
template<typename TParametersValueType>
typename Rigid3DTransform<TParametersValueType>::InputVnlVectorType
Rigid3DTransform<TParametersValueType>::BackTransform(const OutputVnlVectorType & vect) const
{
  itkWarningMacro(
    << "BackTransform(): This method is slated to be removed from ITK."
    <<
    " Instead, please use GetInverse() to generate an inverse transform and then perform the transform using that inverted transform.");
  return this->GetInverseMatrix() * vect;
}

// Back Transform a CovariantVector
template<typename TParametersValueType>
typename Rigid3DTransform<TParametersValueType>::InputCovariantVectorType
Rigid3DTransform<TParametersValueType>::BackTransform(const OutputCovariantVectorType & vect) const
{
  itkWarningMacro(
    << "BackTransform(): This method is slated to be removed from ITK."
    <<
    " Instead, please use GetInverse() to generate an inverse transform and then perform the transform using that inverted transform.");
  return this->GetMatrix() * vect;
}
#endif // end ITK_LEGACY_REMOVE

} // namespace

#endif
