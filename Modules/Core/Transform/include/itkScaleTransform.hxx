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
#ifndef itkScaleTransform_hxx
#define itkScaleTransform_hxx

#include "itkScaleTransform.h"
#include "itkMath.h"

namespace itk
{

template <typename ScalarType, unsigned int NDimensions>
ScaleTransform<ScalarType, NDimensions>::ScaleTransform() : Superclass(ParametersDimension)
{
  m_Scale.Fill(NumericTraits<ScalarType>::One);
}


template <typename ScalarType, unsigned int NDimensions>
ScaleTransform<ScalarType, NDimensions>::
~ScaleTransform()
{
}


template <typename ScalarType, unsigned int NDimensions>
void
ScaleTransform<ScalarType, NDimensions>
::SetParameters(const ParametersType & parameters)
{
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    m_Scale[i] = parameters[i];
    }
  // Save parameters. Needed for proper operation of TransformUpdateParameters.
  if( &parameters != &(this->m_Parameters) )
    {
    this->m_Parameters = parameters;
    }

  this->ComputeMatrix();
  this->ComputeOffset();

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();
}


template <typename TScalar, unsigned int NDimensions>
const typename ScaleTransform<TScalar, NDimensions>::ParametersType &
ScaleTransform<TScalar, NDimensions>
::GetParameters() const
{
  itkDebugMacro(<< "Getting parameters ");
  // Transfer the translation part
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    this->m_Parameters[i] = m_Scale[i];
    }

  itkDebugMacro(<< "After getting parameters " << this->m_Parameters);

  return this->m_Parameters;
}


template <typename ScalarType, unsigned int NDimensions>
void
ScaleTransform<ScalarType, NDimensions>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Scale: " << m_Scale << std::endl;
}


template <typename ScalarType, unsigned int NDimensions>
void
ScaleTransform<ScalarType, NDimensions>
::Compose(const Self *other, bool)
{
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    m_Scale[i] *= other->m_Scale[i];
    }
}


template <typename ScalarType, unsigned int NDimensions>
void
ScaleTransform<ScalarType, NDimensions>
::Scale(const ScaleType & scale, bool)
{
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    m_Scale[i] *= scale[i];
    }
}


template <typename ScalarType, unsigned int NDimensions>
typename ScaleTransform<ScalarType, NDimensions>::OutputPointType
ScaleTransform<ScalarType, NDimensions>
::TransformPoint(const InputPointType & point) const
{
  OutputPointType       result;
  const InputPointType &center = this->GetCenter();

  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    result[i] = ( point[i] - center[i] ) * m_Scale[i] + center[i];
    }
  return result;
}


template <typename ScalarType, unsigned int NDimensions>
typename ScaleTransform<ScalarType, NDimensions>::OutputVectorType
ScaleTransform<ScalarType, NDimensions>
::TransformVector(const InputVectorType & vect) const
{
  OutputVectorType result;

  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    result[i] = vect[i] * m_Scale[i];
    }
  return result;
}


template <typename ScalarType, unsigned int NDimensions>
typename ScaleTransform<ScalarType, NDimensions>::OutputVnlVectorType
ScaleTransform<ScalarType, NDimensions>
::TransformVector(const InputVnlVectorType & vect) const
{
  OutputVnlVectorType result;

  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    result[i] = vect[i] * m_Scale[i];
    }
  return result;
}


template <typename ScalarType, unsigned int NDimensions>
typename ScaleTransform<ScalarType, NDimensions>::OutputCovariantVectorType
ScaleTransform<ScalarType, NDimensions>
::TransformCovariantVector(const InputCovariantVectorType & vect) const
{
  // Covariant Vectors are scaled by the inverse
  OutputCovariantVectorType result;

  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    result[i] = vect[i] / m_Scale[i];
    }
  return result;
}


template <typename ScalarType, unsigned int NDimensions>
bool
ScaleTransform<ScalarType, NDimensions>
::GetInverse(Self *inverse) const
{
  if( !inverse )
    {
    return false;
    }
  inverse->SetFixedParameters(this->GetFixedParameters());
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    inverse->m_Scale[i] = NumericTraits<double>::OneValue() / m_Scale[i];
    }

  return true;
}


template <typename ScalarType, unsigned int NDimensions>
typename ScaleTransform<ScalarType, NDimensions>::InverseTransformBasePointer
ScaleTransform<ScalarType, NDimensions>
::GetInverseTransform() const
{
  Pointer inv = New();

  if( this->GetInverse(inv) )
    {
    return inv.GetPointer();
    }
  return ITK_NULLPTR;
}

template <typename ScalarType, unsigned int NDimensions>
void
ScaleTransform<ScalarType, NDimensions>
::SetIdentity()
{
  Superclass::SetIdentity();
  ScaleType i;
  i.Fill(1.0);
  this->SetScale(i);
}


template <typename ScalarType, unsigned int NDimensions>
void
ScaleTransform<ScalarType, NDimensions>
::ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & j) const
{
  j.SetSize( SpaceDimension, this->GetNumberOfLocalParameters() );
  j.Fill(0.0);
  const InputPointType &center = this->GetCenter();
  for( unsigned int dim = 0; dim < SpaceDimension; dim++ )
    {
    j(dim, dim) = p[dim] - center[dim];
    }
}


template <typename ScalarType, unsigned int NDimensions>
void
ScaleTransform<ScalarType, NDimensions>
::ComputeJacobianWithRespectToPosition(const InputPointType &,
                                       JacobianType & jac) const
{
  jac.SetSize( NDimensions, NDimensions );
  jac.Fill(0.0);
  for( unsigned int dim = 0; dim < NDimensions; dim++ )
    {
    jac[dim][dim] = m_Scale[dim];
    }
}

template <typename ScalarType, unsigned int NDimensions>
void
ScaleTransform<ScalarType, NDimensions>
::SetScale(const ScaleType & scale)
{
  m_Scale = scale;
  this->ComputeMatrix();
  this->ComputeOffset();
  this->Modified();
}

template <typename ScalarType, unsigned int NDimensions>
void
ScaleTransform<ScalarType, NDimensions>
::ComputeMatrix()
{

  MatrixType matrix;

  matrix.SetIdentity();
  for( unsigned int dim = 0; dim < SpaceDimension; dim++ )
    {
    matrix[dim][dim] = m_Scale[dim];
    }

  this->SetVarMatrix(matrix);

}


// Back transform a point
template <typename ScalarType, unsigned int NDimensions>
inline
typename ScaleTransform<ScalarType, NDimensions>::InputPointType
ScaleTransform<ScalarType, NDimensions>::BackTransform(const OutputPointType & point) const
{
  InputPointType        result;
  const InputPointType &center = this->GetCenter();

  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    result[i] = ( point[i] + center[i] ) / m_Scale[i] - center[i];
    }
  return result;
}

// Back transform a vector
template <typename ScalarType, unsigned int NDimensions>
inline
typename ScaleTransform<ScalarType, NDimensions>::InputVectorType
ScaleTransform<ScalarType, NDimensions>::BackTransform(const OutputVectorType & vect) const
{
  InputVectorType result;

  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    result[i] = vect[i] / m_Scale[i];
    }
  return result;
}

// Back transform a vnl_vector
template <typename ScalarType, unsigned int NDimensions>
inline
typename ScaleTransform<ScalarType, NDimensions>::InputVnlVectorType
ScaleTransform<ScalarType, NDimensions>::BackTransform(const OutputVnlVectorType & vect) const
{
  InputVnlVectorType result;

  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    result[i] = vect[i] / m_Scale[i];
    }
  return result;
}

// Back Transform a CovariantVector
template <typename ScalarType, unsigned int NDimensions>
inline
typename ScaleTransform<ScalarType, NDimensions>::InputCovariantVectorType
ScaleTransform<ScalarType, NDimensions>::BackTransform(const OutputCovariantVectorType & vect) const
{
  // Covariant Vectors are scaled by the inverse
  InputCovariantVectorType result;

  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    result[i] = vect[i] * m_Scale[i];
    }
  return result;
}

} // end namespace itk

#endif
