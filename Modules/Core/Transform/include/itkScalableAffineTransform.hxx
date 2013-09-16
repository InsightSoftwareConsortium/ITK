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
#ifndef __itkScalableAffineTransform_hxx
#define __itkScalableAffineTransform_hxx

#include "itkNumericTraits.h"
#include "itkScalableAffineTransform.h"
#include "vnl/algo/vnl_matrix_inverse.h"

namespace itk
{
/** Constructor with default arguments */
template< typename TScalar, unsigned int NDimensions >
ScalableAffineTransform< TScalar, NDimensions >
::ScalableAffineTransform():
  Superclass(Self::ParametersDimension)
{
  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    m_Scale[i] = 1;
    m_MatrixScale[i] = 1;
    }
}

/** Constructor with default arguments */
template< typename TScalar, unsigned int NDimensions >
ScalableAffineTransform< TScalar, NDimensions >
::ScalableAffineTransform(unsigned int , unsigned int parametersDimension):
  Superclass(parametersDimension)
{
  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    m_Scale[i] = 1;
    m_MatrixScale[i] = 1;
    }
}

template< typename TScalar, unsigned int NDimensions >
ScalableAffineTransform< TScalar, NDimensions >
::ScalableAffineTransform(unsigned int parametersDimension):
  Superclass(parametersDimension)
{
  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    m_Scale[i] = 1;
    m_MatrixScale[i] = 1;
    }
}

/** Constructor with default arguments */
template< typename TScalar, unsigned int NDimensions >
ScalableAffineTransform< TScalar, NDimensions >
::ScalableAffineTransform(const MatrixType & matrix,
                          const OutputVectorType & offset):
  Superclass(matrix, offset)
{
  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    m_Scale[i] = 1;
    m_MatrixScale[i] = 1;
    }
}

/** Destructor */
template< typename TScalar, unsigned int NDimensions >
ScalableAffineTransform< TScalar, NDimensions >
::~ScalableAffineTransform()
{
}

/** Print self */
template< typename TScalar, unsigned int NDimensions >
void
ScalableAffineTransform< TScalar, NDimensions >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  unsigned int i;

  os << indent << "Scale : ";
  for ( i = 0; i < NDimensions; i++ )
    {
    os << m_Scale[i] << " ";
    }
  os << std::endl;
  os << indent << "MatrixScale : ";
  for ( i = 0; i < NDimensions; i++ )
    {
    os << m_MatrixScale[i] << " ";
    }
  os << std::endl;
}

// Set the parameters in order to fit an Identity transform
template< typename TScalar, unsigned int NDimensions >
void
ScalableAffineTransform< TScalar, NDimensions >
::SetIdentity(void)
{
  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    m_Scale[i] = 1;
    m_MatrixScale[i] = 1;
    }
  this->Superclass::SetIdentity();
}

/** Set the scale of the transformation */
template< typename TScalar, unsigned int NDimensions >
void
ScalableAffineTransform< TScalar, NDimensions >
::SetScale(const InputVectorType & scale)
{
  unsigned int i;

  for ( i = 0; i < NDimensions; i++ )
    {
    m_Scale[i] = scale[i];
    }
  this->ComputeMatrix();
  this->Modified();
}

template< typename TScalar, unsigned int NDimensions >
void
ScalableAffineTransform< TScalar, NDimensions >
::SetScale(const double scale[NDimensions])
{
  unsigned int i;

  for ( i = 0; i < NDimensions; i++ )
    {
    m_Scale[i] = scale[i];
    }
  this->ComputeMatrix();
  this->Modified();
}

// Get an inverse of this transform
template< typename TScalar, unsigned int NDimensions >
bool
ScalableAffineTransform< TScalar, NDimensions >
::GetInverse(Self *inverse) const
{
  return this->Superclass::GetInverse(inverse);
}

// Return an inverse of this transform
template< typename TScalar, unsigned int NDimensions >
typename ScalableAffineTransform< TScalar, NDimensions >
::InverseTransformBasePointer
ScalableAffineTransform< TScalar, NDimensions >
::GetInverseTransform() const
{
  Pointer inv = New();

  return this->GetInverse(inv) ? inv.GetPointer() : NULL;
}

/** Set the scale of the transformation */
template< typename TScalar, unsigned int NDimensions >
void
ScalableAffineTransform< TScalar, NDimensions >
::ComputeMatrix()
{
  bool scaleChanged = false;
  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    if ( m_Scale[i] != m_MatrixScale[i] )
      {
      scaleChanged = true;
      }
    }
  if ( scaleChanged )
    {
    MatrixType mat;
    typename MatrixType::InternalMatrixType & imat = mat.GetVnlMatrix();
    for ( unsigned int i = 0; i < NDimensions; i++ )
      {
      if ( m_MatrixScale[i] != 0 && m_Scale[i] != 0 )
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

#if !defined(ITK_LEGACY_REMOVE)
template< typename TScalar, unsigned int NDimensions >
void
ScalableAffineTransform< TScalar, NDimensions >
::SetMatrixComponent(const MatrixType & matrix)
{
  this->SetMatrix(matrix);
}

template< typename TScalar, unsigned int NDimensions >
const typename ScalableAffineTransform< TScalar, NDimensions >::MatrixType &
ScalableAffineTransform< TScalar, NDimensions >
::GetMatrixComponent() const
{
  return this->GetMatrix();
}

template< typename TScalar, unsigned int NDimensions >
void
ScalableAffineTransform< TScalar, NDimensions >
::SetOffsetComponent(const OffsetType & offset)
{
  this->SetTranslation(offset);
}

template< typename TScalar, unsigned int NDimensions >
const typename ScalableAffineTransform< TScalar, NDimensions >::OffsetType &
ScalableAffineTransform< TScalar, NDimensions >
::GetOffsetComponent(void) const
{
  return this->GetTranslation();
}
#endif

} // namespace

#endif
