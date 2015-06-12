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
#ifndef itkAffineTransform_hxx
#define itkAffineTransform_hxx

#include "itkNumericTraits.h"
#include "itkAffineTransform.h"
#include "vnl/algo/vnl_matrix_inverse.h"

namespace itk
{
/** Constructor with default arguments */
template<typename TParametersValueType, unsigned int NDimensions>
AffineTransform<TParametersValueType, NDimensions>::AffineTransform():Superclass(ParametersDimension)
{}

/** Constructor with default arguments */
template<typename TParametersValueType, unsigned int NDimensions>
AffineTransform<TParametersValueType, NDimensions>::AffineTransform(unsigned int parametersDimension):
  Superclass(parametersDimension)
{}

/** Constructor with explicit arguments */
template<typename TParametersValueType, unsigned int NDimensions>
AffineTransform<TParametersValueType, NDimensions>::AffineTransform(const MatrixType & matrix,
                                                             const OutputVectorType & offset):
  Superclass(matrix, offset)
{}

/**  Destructor */
template<typename TParametersValueType, unsigned int NDimensions>
AffineTransform<TParametersValueType, NDimensions>::
~AffineTransform()
{
}

/** Print self */
template<typename TParametersValueType, unsigned int NDimensions>
void
AffineTransform<TParametersValueType, NDimensions>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

/** Compose with a translation */
template<typename TParametersValueType, unsigned int NDimensions>
void
AffineTransform<TParametersValueType, NDimensions>::Translate(const OutputVectorType & trans, bool pre)
{
  OutputVectorType newTranslation = this->GetTranslation();

  if ( pre )
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

/** Compose with isotropic scaling */
template<typename TParametersValueType, unsigned int NDimensions>
void
AffineTransform<TParametersValueType, NDimensions>
::Scale(const TParametersValueType & factor, bool pre)
{
  if ( pre )
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

/** Compose with anisotropic scaling */
template<typename TParametersValueType, unsigned int NDimensions>
void
AffineTransform<TParametersValueType, NDimensions>
::Scale(const OutputVectorType & factor, bool pre)
{
  MatrixType   trans;
  unsigned int i, j;

  for ( i = 0; i < NDimensions; i++ )
    {
    for ( j = 0; j < NDimensions; j++ )
      {
      trans[i][j] = 0.0;
      }
    trans[i][i] = factor[i];
    }
  if ( pre )
    {
    this->SetVarMatrix(this->GetMatrix() * trans);
    }
  else
    {
    this->SetVarMatrix( trans * this->GetMatrix() );
    this->SetVarTranslation( trans * this->GetTranslation() );
    }
  this->ComputeMatrixParameters();
  this->ComputeOffset();
  this->Modified();
}

/** Compose with elementary rotation */
template<typename TParametersValueType, unsigned int NDimensions>
void
AffineTransform<TParametersValueType, NDimensions>
::Rotate(int axis1, int axis2, TParametersValueType angle, bool pre)
{
  MatrixType   trans;
  unsigned int i, j;

  for ( i = 0; i < NDimensions; i++ )
    {
    for ( j = 0; j < NDimensions; j++ )
      {
      trans[i][j] = 0.0;
      }
    trans[i][i] = 1.0;
    }
  trans[axis1][axis1] =  std::cos(angle);
  trans[axis1][axis2] =  std::sin(angle);
  trans[axis2][axis1] = -std::sin(angle);
  trans[axis2][axis2] =  std::cos(angle);
  if ( pre )
    {
    this->SetVarMatrix(this->GetMatrix() * trans);
    }
  else
    {
    this->SetVarMatrix( trans * this->GetMatrix() );
    this->SetVarTranslation( trans * this->GetTranslation() );
    }
  this->ComputeMatrixParameters();
  this->ComputeOffset();
  this->Modified();
}

/** Compose with 2D rotation
 * \todo Find a way to generate a compile-time error
 * is this is used with NDimensions != 2. */
template<typename TParametersValueType, unsigned int NDimensions>
void
AffineTransform<TParametersValueType, NDimensions>
::Rotate2D(TParametersValueType angle, bool pre)
{
  MatrixType trans;

  trans[0][0] =  std::cos(angle);
  trans[0][1] = -std::sin(angle);
  trans[1][0] =  std::sin(angle);
  trans[1][1] =  std::cos(angle);
  if ( pre )
    {
    this->SetVarMatrix(this->GetMatrix() * trans);
    }
  else
    {
    this->SetVarMatrix( trans * this->GetMatrix() );
    this->SetVarTranslation( trans * this->GetTranslation() );
    }
  this->ComputeMatrixParameters();
  this->ComputeOffset();
  this->Modified();
}

/** Compose with 3D rotation
 *  \todo Find a way to generate a compile-time error
 *  is this is used with NDimensions != 3. */
template<typename TParametersValueType, unsigned int NDimensions>
void
AffineTransform<TParametersValueType, NDimensions>
::Rotate3D(const OutputVectorType & axis, TParametersValueType angle, bool pre)
{
  MatrixType trans;
  ScalarType r, x1, x2, x3;
  ScalarType q0, q1, q2, q3;

  // Convert the axis to a unit vector
  r = std::sqrt(axis[0] * axis[0] + axis[1] * axis[1] + axis[2] * axis[2]);
  x1 = axis[0] / r;
  x2 = axis[1] / r;
  x3 = axis[2] / r;

  // Compute quaternion elements
  q0 = std::cos(angle / 2.0);
  q1 = x1 * std::sin(angle / 2.0);
  q2 = x2 * std::sin(angle / 2.0);
  q3 = x3 * std::sin(angle / 2.0);

  // Compute elements of the rotation matrix
  trans[0][0] = q0 * q0 + q1 * q1 - q2 * q2 - q3 * q3;
  trans[0][1] = 2.0 * ( q1 * q2 - q0 * q3 );
  trans[0][2] = 2.0 * ( q1 * q3 + q0 * q2 );
  trans[1][0] = 2.0 * ( q1 * q2 + q0 * q3 );
  trans[1][1] = q0 * q0 + q2 * q2 - q1 * q1 - q3 * q3;
  trans[1][2] = 2.0 * ( q2 * q3 - q0 * q1 );
  trans[2][0] = 2.0 * ( q1 * q3 - q0 * q2 );
  trans[2][1] = 2.0 * ( q2 * q3 + q0 * q1 );
  trans[2][2] = q0 * q0 + q3 * q3 - q1 * q1 - q2 * q2;

  // Compose rotation matrix with the existing matrix
  if ( pre )
    {
    this->SetVarMatrix(this->GetMatrix() * trans);
    }
  else
    {
    this->SetVarMatrix( trans * this->GetMatrix() );
    this->SetVarTranslation( trans * this->GetTranslation() );
    }
  this->ComputeMatrixParameters();
  this->ComputeOffset();
  this->Modified();
}

/** Compose with elementary rotation */
template<typename TParametersValueType, unsigned int NDimensions>
void
AffineTransform<TParametersValueType, NDimensions>
::Shear(int axis1, int axis2, TParametersValueType coef, bool pre)
{
  MatrixType   trans;
  unsigned int i, j;

  for ( i = 0; i < NDimensions; i++ )
    {
    for ( j = 0; j < NDimensions; j++ )
      {
      trans[i][j] = 0.0;
      }
    trans[i][i] = 1.0;
    }
  trans[axis1][axis2] =  coef;
  if ( pre )
    {
    this->SetVarMatrix(this->GetMatrix() * trans);
    }
  else
    {
    this->SetVarMatrix( trans * this->GetMatrix() );
    this->SetVarTranslation( trans * this->GetTranslation() );
    }
  this->ComputeMatrixParameters();
  this->ComputeOffset();
  this->Modified();
}

/** Get an inverse of this transform. */
template<typename TParametersValueType, unsigned int NDimensions>
bool
AffineTransform<TParametersValueType, NDimensions>
::GetInverse(Self *inverse) const
{
  return this->Superclass::GetInverse(inverse);
}

/** Return an inverse of this transform. */
template<typename TParametersValueType, unsigned int NDimensions>
typename AffineTransform<TParametersValueType, NDimensions>::InverseTransformBasePointer
AffineTransform<TParametersValueType, NDimensions>
::GetInverseTransform() const
{
  Pointer inv = New();

  return this->GetInverse(inv) ? inv.GetPointer() : ITK_NULLPTR;
}

/** Compute a distance between two affine transforms */
template<typename TParametersValueType, unsigned int NDimensions>
typename AffineTransform<TParametersValueType, NDimensions>::ScalarType
AffineTransform<TParametersValueType, NDimensions>
::Metric(const Self *other) const
{
  ScalarType result = 0.0, term;

  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    for ( unsigned int j = 0; j < NDimensions; j++ )
      {
      term = this->GetMatrix()[i][j] - other->GetMatrix()[i][j];
      result += term * term;
      }
    term = this->GetOffset()[i] - other->GetOffset()[i];
    result += term * term;
    }
  return std::sqrt(result);
}

/** Compute a distance between self and the identity transform */
template<typename TParametersValueType, unsigned int NDimensions>
typename AffineTransform<TParametersValueType, NDimensions>::ScalarType
AffineTransform<TParametersValueType, NDimensions>
::Metric(void) const
{
  ScalarType result = 0.0, term;

  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    for ( unsigned int j = 0; j < NDimensions; j++ )
      {
      if ( i == j )
        {
        term = this->GetMatrix()[i][j] - 1.0;
        }
      else
        {
        term = this->GetMatrix()[i][j];
        }
      result += term * term;
      }
    term = this->GetOffset()[i];
    result += term * term;
    }

  return std::sqrt(result);
}
} // namespace

#endif
