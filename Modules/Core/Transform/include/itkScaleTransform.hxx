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

template<typename TParametersValueType, unsigned int NDimensions>
ScaleTransform<TParametersValueType, NDimensions>
::ScaleTransform() : Superclass(ParametersDimension)
{
  m_Scale.Fill(NumericTraits<ScalarType>::OneValue());
}


template<typename TParametersValueType, unsigned int NDimensions>
ScaleTransform<TParametersValueType, NDimensions>::
~ScaleTransform()
{
}


template<typename TParametersValueType, unsigned int NDimensions>
void
ScaleTransform<TParametersValueType, NDimensions>
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


template<typename TParametersValueType, unsigned int NDimensions>
const typename ScaleTransform<TParametersValueType, NDimensions>::ParametersType &
ScaleTransform<TParametersValueType, NDimensions>
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


template<typename TParametersValueType, unsigned int NDimensions>
void
ScaleTransform<TParametersValueType, NDimensions>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Scale: " << m_Scale << std::endl;
}


template<typename TParametersValueType, unsigned int NDimensions>
void
ScaleTransform<TParametersValueType, NDimensions>
::Compose(const Self *other, bool)
{
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    m_Scale[i] *= other->m_Scale[i];
    }
}


template<typename TParametersValueType, unsigned int NDimensions>
void
ScaleTransform<TParametersValueType, NDimensions>
::Scale(const ScaleType & scale, bool)
{
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    m_Scale[i] *= scale[i];
    }
}


template<typename TParametersValueType, unsigned int NDimensions>
typename ScaleTransform<TParametersValueType, NDimensions>::OutputPointType
ScaleTransform<TParametersValueType, NDimensions>
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


template<typename TParametersValueType, unsigned int NDimensions>
typename ScaleTransform<TParametersValueType, NDimensions>::OutputVectorType
ScaleTransform<TParametersValueType, NDimensions>
::TransformVector(const InputVectorType & vect) const
{
  OutputVectorType result;

  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    result[i] = vect[i] * m_Scale[i];
    }
  return result;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename ScaleTransform<TParametersValueType, NDimensions>::OutputVnlVectorType
ScaleTransform<TParametersValueType, NDimensions>
::TransformVector(const InputVnlVectorType & vect) const
{
  OutputVnlVectorType result;

  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    result[i] = vect[i] * m_Scale[i];
    }
  return result;
}


template<typename TParametersValueType, unsigned int NDimensions>
typename ScaleTransform<TParametersValueType, NDimensions>::OutputCovariantVectorType
ScaleTransform<TParametersValueType, NDimensions>
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


template<typename TParametersValueType, unsigned int NDimensions>
bool
ScaleTransform<TParametersValueType, NDimensions>
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


template<typename TParametersValueType, unsigned int NDimensions>
typename ScaleTransform<TParametersValueType, NDimensions>::InverseTransformBasePointer
ScaleTransform<TParametersValueType, NDimensions>
::GetInverseTransform() const
{
  Pointer inv = New();

  if( this->GetInverse(inv) )
    {
    return inv.GetPointer();
    }
  return ITK_NULLPTR;
}

template<typename TParametersValueType, unsigned int NDimensions>
void
ScaleTransform<TParametersValueType, NDimensions>
::SetIdentity()
{
  Superclass::SetIdentity();
  ScaleType i;
  i.Fill(1.0);
  this->SetScale(i);
}


template<typename TParametersValueType, unsigned int NDimensions>
void
ScaleTransform<TParametersValueType, NDimensions>
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


template<typename TParametersValueType, unsigned int NDimensions>
void
ScaleTransform<TParametersValueType, NDimensions>
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

template<typename TParametersValueType, unsigned int NDimensions>
void
ScaleTransform<TParametersValueType, NDimensions>
::SetScale(const ScaleType & scale)
{
  m_Scale = scale;
  this->ComputeMatrix();
  this->ComputeOffset();
  this->Modified();
}

template<typename TParametersValueType, unsigned int NDimensions>
void
ScaleTransform<TParametersValueType, NDimensions>
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
template<typename TParametersValueType, unsigned int NDimensions>
inline
typename ScaleTransform<TParametersValueType, NDimensions>::InputPointType
ScaleTransform<TParametersValueType, NDimensions>::BackTransform(const OutputPointType & point) const
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
template<typename TParametersValueType, unsigned int NDimensions>
inline
typename ScaleTransform<TParametersValueType, NDimensions>::InputVectorType
ScaleTransform<TParametersValueType, NDimensions>::BackTransform(const OutputVectorType & vect) const
{
  InputVectorType result;

  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    result[i] = vect[i] / m_Scale[i];
    }
  return result;
}

// Back transform a vnl_vector
template<typename TParametersValueType, unsigned int NDimensions>
inline
typename ScaleTransform<TParametersValueType, NDimensions>::InputVnlVectorType
ScaleTransform<TParametersValueType, NDimensions>::BackTransform(const OutputVnlVectorType & vect) const
{
  InputVnlVectorType result;

  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    result[i] = vect[i] / m_Scale[i];
    }
  return result;
}

// Back Transform a CovariantVector
template<typename TParametersValueType, unsigned int NDimensions>
inline
typename ScaleTransform<TParametersValueType, NDimensions>::InputCovariantVectorType
ScaleTransform<TParametersValueType, NDimensions>::BackTransform(const OutputCovariantVectorType & vect) const
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
