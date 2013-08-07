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
#ifndef __itkCenteredAffineTransform_hxx
#define __itkCenteredAffineTransform_hxx

#include "itkNumericTraits.h"
#include "itkCenteredAffineTransform.h"
#include "vnl/algo/vnl_matrix_inverse.h"

namespace itk
{
// Constructor with default arguments
template <class TScalarType, unsigned int NDimensions>
CenteredAffineTransform<TScalarType, NDimensions>::CenteredAffineTransform() : Superclass(ParametersDimension)
{
}

// Destructor
template <class TScalarType, unsigned int NDimensions>
CenteredAffineTransform<TScalarType, NDimensions>::
~CenteredAffineTransform()
{
}

// Get parameters
template <class TScalarType, unsigned int NDimensions>
const typename CenteredAffineTransform<TScalarType,
                                       NDimensions>::ParametersType
& CenteredAffineTransform<TScalarType, NDimensions>::GetParameters(void) const
  {
  // Transfer the linear part
  unsigned int par = 0;

  const MatrixType & matrix = this->GetMatrix();
  for( unsigned int row = 0; row < NDimensions; row++ )
    {
    for( unsigned int col = 0; col < NDimensions; col++ )
      {
      this->m_Parameters[par] = matrix[row][col];
      ++par;
      }
    }

  // Transfer the rotation center
  InputPointType center = this->GetCenter();
  for( unsigned int j = 0; j < NDimensions; j++ )
    {
    this->m_Parameters[par] = center[j];
    ++par;
    }

  // Transfer the translation
  OutputVectorType translation = this->GetTranslation();
  for( unsigned int k = 0; k < NDimensions; k++ )
    {
    this->m_Parameters[par] = translation[k];
    ++par;
    }

  return this->m_Parameters;
  }

/** Set the parameters */
template <class TScalarType, unsigned int NDimensions>
void
CenteredAffineTransform<TScalarType, NDimensions>::SetParameters(const ParametersType & parameters)
{
  // Transfer the linear part
  unsigned int par = 0;

  // Save parameters. Needed for proper operation of TransformUpdateParameters.
  if( &parameters != &(this->m_Parameters) )
    {
    this->m_Parameters = parameters;
    }

  MatrixType matrix;
  for( unsigned int row = 0; row < NDimensions; row++ )
    {
    for( unsigned int col = 0; col < NDimensions; col++ )
      {
      matrix[row][col] = this->m_Parameters[par];
      ++par;
      }
    }

  this->SetMatrix(matrix);

  // Transfer the rotation center
  InputPointType center;
  for( unsigned int i = 0; i < NDimensions; i++ )
    {
    center[i] = this->m_Parameters[par];
    ++par;
    }
  this->SetCenter(center);

  // Transfer the translation
  OutputVectorType translation;
  for( unsigned int k = 0; k < NDimensions; k++ )
    {
    translation[k] = this->m_Parameters[par];
    ++par;
    }
  this->SetTranslation(translation);

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();
}

template <class TScalarType, unsigned int NDimensions>
void
CenteredAffineTransform<TScalarType, NDimensions>
::ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const
{
  // The Jacobian of the affine transform is composed of
  // subblocks of diagonal matrices, each one of them having
  // a constant value in the diagonal.
  // The block corresponding to the center parameters is
  // composed by ( Identity matrix - Rotation Matrix).
  jacobian.SetSize( NDimensions, this->GetNumberOfLocalParameters() );
  jacobian.Fill(0.0);

  unsigned int blockOffset = 0;
  for( unsigned int block = 0; block < SpaceDimension; block++ )
    {
    for( unsigned int dim = 0; dim < SpaceDimension; dim++ )
      {
      jacobian(block, blockOffset + dim) = p[dim];
      }
    blockOffset += SpaceDimension;
    }

  // Block associated with the center parameters
  const MatrixType & matrix = this->GetMatrix();
  for( unsigned int k = 0; k < SpaceDimension; k++ )
    {
    jacobian(k, blockOffset + k) = 1.0;
    for( unsigned int dim = 0; dim < SpaceDimension; dim++ )
      {
      jacobian(k, blockOffset + dim) -= matrix[k][dim];
      }
    }
  blockOffset += SpaceDimension;
  // Block associated with the translations
  for( unsigned int dim = 0; dim < SpaceDimension; dim++ )
    {
    jacobian(dim, blockOffset + dim) = 1.0;
    }
}

// Get an inverse of this transform
template <class TScalarType, unsigned int NDimensions>
bool
CenteredAffineTransform<TScalarType, NDimensions>
::GetInverse(Self *inverse) const
{
  return this->Superclass::GetInverse(inverse);
}

// Return an inverse of this transform
template <class TScalarType, unsigned int NDimensions>
typename CenteredAffineTransform<TScalarType, NDimensions>::InverseTransformBasePointer
CenteredAffineTransform<TScalarType, NDimensions>
::GetInverseTransform() const
{
  Pointer inv = New();

  return this->GetInverse(inv) ? inv.GetPointer() : NULL;
}

} // namespace

#endif
