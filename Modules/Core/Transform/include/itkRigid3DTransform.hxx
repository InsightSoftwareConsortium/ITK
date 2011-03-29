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
template< class TScalarType >
Rigid3DTransform< TScalarType >::Rigid3DTransform():
  Superclass(OutputSpaceDimension, ParametersDimension)
{}

// Constructor with default arguments
template< class TScalarType >
Rigid3DTransform< TScalarType >::Rigid3DTransform(unsigned int spaceDim,
                                                  unsigned int paramDim):
  Superclass(spaceDim, paramDim)
{}

// Constructor with default arguments
template< class TScalarType >
Rigid3DTransform< TScalarType >::Rigid3DTransform(const MatrixType & matrix,
                                                  const OutputVectorType & offset):
  Superclass(matrix, offset)
{}

// Destructor
template< class TScalarType >
Rigid3DTransform< TScalarType >::
~Rigid3DTransform()
{}

// Print self
template< class TScalarType >
void
Rigid3DTransform< TScalarType >::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

// Check if input matrix is orthogonal to within tolerance
template< class TScalarType >
bool
Rigid3DTransform< TScalarType >
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
template< class TScalarType >
void
Rigid3DTransform< TScalarType >
::SetMatrix(const MatrixType & matrix)
{
  const double tolerance = 1e-10;

  if ( !this->MatrixIsOrthogonal(matrix, tolerance) )
    {
    itkExceptionMacro(<< "Attempting to set a non-orthogonal rotation matrix");
    }

  this->Superclass::SetMatrix(matrix);
}

// Set optimizable parameters from array
template< class TScalarType >
void
Rigid3DTransform< TScalarType >
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
template< class TScalarType >
void
Rigid3DTransform< TScalarType >::Translate(const OffsetType & offset, bool)
{
  OutputVectorType newOffset = this->GetOffset();

  newOffset += offset;
  this->SetOffset(newOffset);
  this->ComputeTranslation();
}

// Back transform a point
template< class TScalarType >
typename Rigid3DTransform< TScalarType >::InputPointType
Rigid3DTransform< TScalarType >::BackTransform(const OutputPointType & point) const
{
  itkWarningMacro(
    << "BackTransform(): This method is slated to be removed from ITK."
    <<
    "Instead, please use GetInverse() to generate an inverse transform and then perform the transform using that inverted transform.");
  return this->GetInverseMatrix() * ( point - this->GetOffset() );
}

// Back transform a vector
template< class TScalarType >
typename Rigid3DTransform< TScalarType >::InputVectorType
Rigid3DTransform< TScalarType >::BackTransform(const OutputVectorType & vect) const
{
  itkWarningMacro(
    << "BackTransform(): This method is slated to be removed from ITK."
    <<
    "Instead, please use GetInverse() to generate an inverse transform and then perform the transform using that inverted transform.");
  return this->GetInverseMatrix() * vect;
}

// Back transform a vnl_vector
template< class TScalarType >
typename Rigid3DTransform< TScalarType >::InputVnlVectorType
Rigid3DTransform< TScalarType >::BackTransform(const OutputVnlVectorType & vect) const
{
  itkWarningMacro(
    << "BackTransform(): This method is slated to be removed from ITK."
    <<
    " Instead, please use GetInverse() to generate an inverse transform and then perform the transform using that inverted transform.");
  return this->GetInverseMatrix() * vect;
}

// Back Transform a CovariantVector
template< class TScalarType >
typename Rigid3DTransform< TScalarType >::InputCovariantVectorType
Rigid3DTransform< TScalarType >::BackTransform(const OutputCovariantVectorType & vect) const
{
  itkWarningMacro(
    << "BackTransform(): This method is slated to be removed from ITK."
    <<
    " Instead, please use GetInverse() to generate an inverse transform and then perform the transform using that inverted transform.");
  return this->GetMatrix() * vect;
}
} // namespace

#endif
