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
#ifndef __itkRigid3DTransform_hxx
#define __itkRigid3DTransform_hxx

#include "itkRigid3DTransform.h"

namespace itk
{
// Constructor with default arguments
template< typename TScalar >
Rigid3DTransform< TScalar >::Rigid3DTransform():
  Superclass(ParametersDimension)
{}

// Constructor with default arguments
template< typename TScalar >
Rigid3DTransform< TScalar >::Rigid3DTransform(unsigned int paramDim):
  Superclass(paramDim)
{}

// Constructor with default arguments
template< typename TScalar >
Rigid3DTransform< TScalar >::Rigid3DTransform(const MatrixType & matrix,
                                                  const OutputVectorType & offset):
  Superclass(matrix, offset)
{}

// Destructor
template< typename TScalar >
Rigid3DTransform< TScalar >::
~Rigid3DTransform()
{}

// Print self
template< typename TScalar >
void
Rigid3DTransform< TScalar >::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

// Check if input matrix is orthogonal to within tolerance
template< typename TScalar >
bool
Rigid3DTransform< TScalar >
::MatrixIsOrthogonal(
  const MatrixType & matrix,
  double tolerance)
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
template< typename TScalar >
void
Rigid3DTransform< TScalar >
::SetMatrix(const MatrixType & matrix)
{
  const double tolerance = 1e-10;
  this->SetMatrix( matrix, tolerance );
}

template< typename TScalar >
void
Rigid3DTransform< TScalar >
::SetMatrix(const MatrixType & matrix, double tolerance)
{
  if ( !this->MatrixIsOrthogonal(matrix, tolerance) )
    {
    itkExceptionMacro(<< "Attempting to set a non-orthogonal rotation matrix");
    }

  this->Superclass::SetMatrix(matrix);
}

// Set optimizable parameters from array
template< typename TScalar >
void
Rigid3DTransform< TScalar >
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

  const double tolerance = 1e-10;
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
template< typename TScalar >
void
Rigid3DTransform< TScalar >::Translate(const OffsetType & offset, bool)
{
  OutputVectorType newOffset = this->GetOffset();

  newOffset += offset;
  this->SetOffset(newOffset);
  this->ComputeTranslation();
}

#ifdef ITKV3_COMPATIBILITY
#if !defined(ITK_LEGACY_REMOVE)
template< typename TScalar >
bool
Rigid3DTransform< TScalar >::GetInverse(Self *inverse) const
{
  return this->Superclass::GetInverse(inverse);
}

template< typename TScalar >
typename Rigid3DTransform< TScalar >::InverseTransformBasePointer
Rigid3DTransform< TScalar >::GetInverseTransform() const
{
  Pointer inv = New();
  return this->GetInverse(inv) ? inv.GetPointer() : ITK_NULLPTR;
}

template< typename TScalar >
const typename Rigid3DTransform< TScalar >::MatrixType &
Rigid3DTransform< TScalar >::GetRotationMatrix() const
{
  return this->GetMatrix();
}

template< typename TScalar >
void
Rigid3DTransform< TScalar >::SetRotationMatrix(const MatrixType & matrix)
{
  this->SetMatrix(matrix);
}
#endif // end ITK_LEGACY_REMOVE
#endif // ITKV3_COMPATIBILITY

#if !defined(ITK_LEGACY_REMOVE)
// Back transform a point
template< typename TScalar >
typename Rigid3DTransform< TScalar >::InputPointType
Rigid3DTransform< TScalar >::BackTransform(const OutputPointType & point) const
{
  itkWarningMacro(
    << "BackTransform(): This method is slated to be removed from ITK."
    <<
    "Instead, please use GetInverse() to generate an inverse transform and then perform the transform using that inverted transform.");
  return this->GetInverseMatrix() * ( point - this->GetOffset() );
}

// Back transform a vector
template< typename TScalar >
typename Rigid3DTransform< TScalar >::InputVectorType
Rigid3DTransform< TScalar >::BackTransform(const OutputVectorType & vect) const
{
  itkWarningMacro(
    << "BackTransform(): This method is slated to be removed from ITK."
    <<
    "Instead, please use GetInverse() to generate an inverse transform and then perform the transform using that inverted transform.");
  return this->GetInverseMatrix() * vect;
}

// Back transform a vnl_vector
template< typename TScalar >
typename Rigid3DTransform< TScalar >::InputVnlVectorType
Rigid3DTransform< TScalar >::BackTransform(const OutputVnlVectorType & vect) const
{
  itkWarningMacro(
    << "BackTransform(): This method is slated to be removed from ITK."
    <<
    " Instead, please use GetInverse() to generate an inverse transform and then perform the transform using that inverted transform.");
  return this->GetInverseMatrix() * vect;
}

// Back Transform a CovariantVector
template< typename TScalar >
typename Rigid3DTransform< TScalar >::InputCovariantVectorType
Rigid3DTransform< TScalar >::BackTransform(const OutputCovariantVectorType & vect) const
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
