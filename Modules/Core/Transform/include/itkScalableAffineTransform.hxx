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
#ifndef itkScalableAffineTransform_hxx
#define itkScalableAffineTransform_hxx

#include "itkMath.h"
#include "itkNumericTraits.h"
#include "itkScalableAffineTransform.h"
#include "vnl/algo/vnl_matrix_inverse.h"
#include "itkMath.h"

namespace itk
{
/** Constructor with default arguments */
template<typename TParametersValueType, unsigned int NDimensions>
ScalableAffineTransform<TParametersValueType, NDimensions>
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
template<typename TParametersValueType, unsigned int NDimensions>
ScalableAffineTransform<TParametersValueType, NDimensions>
::ScalableAffineTransform(unsigned int , unsigned int parametersDimension):
  Superclass(parametersDimension)
{
  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    m_Scale[i] = 1;
    m_MatrixScale[i] = 1;
    }
}

template<typename TParametersValueType, unsigned int NDimensions>
ScalableAffineTransform<TParametersValueType, NDimensions>
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
template<typename TParametersValueType, unsigned int NDimensions>
ScalableAffineTransform<TParametersValueType, NDimensions>
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
template<typename TParametersValueType, unsigned int NDimensions>
ScalableAffineTransform<TParametersValueType, NDimensions>
::~ScalableAffineTransform()
{
}

/** Print self */
template<typename TParametersValueType, unsigned int NDimensions>
void
ScalableAffineTransform<TParametersValueType, NDimensions>
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
template<typename TParametersValueType, unsigned int NDimensions>
void
ScalableAffineTransform<TParametersValueType, NDimensions>
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
template<typename TParametersValueType, unsigned int NDimensions>
void
ScalableAffineTransform<TParametersValueType, NDimensions>
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

template<typename TParametersValueType, unsigned int NDimensions>
void
ScalableAffineTransform<TParametersValueType, NDimensions>
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
template<typename TParametersValueType, unsigned int NDimensions>
bool
ScalableAffineTransform<TParametersValueType, NDimensions>
::GetInverse(Self *inverse) const
{
  return this->Superclass::GetInverse(inverse);
}

// Return an inverse of this transform
template<typename TParametersValueType, unsigned int NDimensions>
typename ScalableAffineTransform<TParametersValueType, NDimensions>
::InverseTransformBasePointer
ScalableAffineTransform<TParametersValueType, NDimensions>
::GetInverseTransform() const
{
  Pointer inv = New();

  return this->GetInverse(inv) ? inv.GetPointer() : ITK_NULLPTR;
}

/** Set the scale of the transformation */
template<typename TParametersValueType, unsigned int NDimensions>
void
ScalableAffineTransform<TParametersValueType, NDimensions>
::ComputeMatrix()
{
  bool scaleChanged = false;
  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    if ( Math::NotExactlyEquals(m_Scale[i], m_MatrixScale[i]) )
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
      if ( Math::NotAlmostEquals( m_MatrixScale[i], NumericTraits< typename NumericTraits<InputVectorType>::ValueType>::ZeroValue() )
              && Math::NotAlmostEquals( m_Scale[i], NumericTraits< double >::ZeroValue() ) )
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
template<typename TParametersValueType, unsigned int NDimensions>
void
ScalableAffineTransform<TParametersValueType, NDimensions>
::SetMatrixComponent(const MatrixType & matrix)
{
  this->SetMatrix(matrix);
}

template<typename TParametersValueType, unsigned int NDimensions>
const typename ScalableAffineTransform<TParametersValueType, NDimensions>::MatrixType &
ScalableAffineTransform<TParametersValueType, NDimensions>
::GetMatrixComponent() const
{
  return this->GetMatrix();
}

template<typename TParametersValueType, unsigned int NDimensions>
void
ScalableAffineTransform<TParametersValueType, NDimensions>
::SetOffsetComponent(const OffsetType & offset)
{
  this->SetTranslation(offset);
}

template<typename TParametersValueType, unsigned int NDimensions>
const typename ScalableAffineTransform<TParametersValueType, NDimensions>::OffsetType &
ScalableAffineTransform<TParametersValueType, NDimensions>
::GetOffsetComponent(void) const
{
  return this->GetTranslation();
}
#endif

} // namespace

#endif
