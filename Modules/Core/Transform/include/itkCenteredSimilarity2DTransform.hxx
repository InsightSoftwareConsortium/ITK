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
#ifndef itkCenteredSimilarity2DTransform_hxx
#define itkCenteredSimilarity2DTransform_hxx

#include "itkCenteredSimilarity2DTransform.h"

namespace itk
{

template<typename TParametersValueType>
CenteredSimilarity2DTransform<TParametersValueType>
::CenteredSimilarity2DTransform() : Superclass(ParametersDimension)
{
}


template<typename TParametersValueType>
CenteredSimilarity2DTransform<TParametersValueType>::CenteredSimilarity2DTransform(unsigned int spaceDimension,
                                                                      unsigned int parametersDimension) :
  Superclass(spaceDimension, parametersDimension)
{
}


template<typename TParametersValueType>
void
CenteredSimilarity2DTransform<TParametersValueType>
::SetParameters(const ParametersType & parameters)
{
  itkDebugMacro(<< "Setting parameters " << parameters);

  // Save parameters. Needed for proper operation of TransformUpdateParameters.
  if( &parameters != &(this->m_Parameters) )
    {
    this->m_Parameters = parameters;
    }

  // Set scale
  const TParametersValueType scale = parameters[0];
  this->SetVarScale(scale);

  // Set angle
  const TParametersValueType angle = parameters[1];
  this->SetVarAngle(angle);

  InputPointType center;
  for( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    center[j] = parameters[j + 2];
    }
  this->SetVarCenter(center);

  // Set translation
  OffsetType translation;
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    translation[i] = parameters[i + 4];
    }

  this->SetVarTranslation(translation);

  this->ComputeMatrix();
  this->ComputeOffset();

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();

  itkDebugMacro(<< "After setting parameters ");
}


template<typename TParametersValueType>
const typename CenteredSimilarity2DTransform<TParametersValueType> ::ParametersType &
CenteredSimilarity2DTransform<TParametersValueType>
::GetParameters() const
{
  itkDebugMacro(<< "Getting parameters ");

  this->m_Parameters[0] = this->GetScale();
  this->m_Parameters[1] = this->GetAngle();

  InputPointType center = this->GetCenter();
  for( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    this->m_Parameters[j + 2] = center[j];
    }

  OffsetType translation = this->GetTranslation();
  for( unsigned int i = 0; i < SpaceDimension; i++ )
    {
    this->m_Parameters[i + 4] = translation[i];
    }

  itkDebugMacro(<< "After getting parameters " << this->m_Parameters);

  return this->m_Parameters;
}


template<typename TParametersValueType>
void
CenteredSimilarity2DTransform<TParametersValueType>
::ComputeJacobianWithRespectToParameters(const InputPointType & p, JacobianType & jacobian) const
{
  const double angle = this->GetAngle();
  const double ca = std::cos(angle);
  const double sa = std::sin(angle);

  jacobian.SetSize( 2, this->GetNumberOfLocalParameters() );
  jacobian.Fill(0.0);

  const InputPointType center = this->GetCenter();
  const double         cx = center[0];
  const double         cy = center[1];

  // derivatives with respect to the scale
  jacobian[0][0] =    ca * ( p[0] - cx ) - sa * ( p[1] - cy );
  jacobian[1][0] =    sa * ( p[0] - cx ) + ca * ( p[1] - cy );

  // derivatives with respect to the angle
  jacobian[0][1] = ( -sa * ( p[0] - cx ) - ca * ( p[1] - cy ) )
    * this->GetScale();
  jacobian[1][1] = ( ca * ( p[0] - cx ) - sa * ( p[1] - cy ) )
    * this->GetScale();

  // compute derivatives with respect to the center part
  // first with respect to cx
  jacobian[0][2] = 1.0 - ca * this-> GetScale();
  jacobian[1][2] =     -sa * this->  GetScale();
  // then with respect to cy
  jacobian[0][3] =       sa * this->GetScale();
  jacobian[1][3] = 1.0 - ca * this-> GetScale();

  // compute derivatives with respect to the translation part
  // first with respect to tx
  jacobian[0][4] = 1.0;
  jacobian[1][4] = 0.0;
  // first with respect to ty
  jacobian[0][5] = 0.0;
  jacobian[1][5] = 1.0;
}


template<typename TParametersValueType>
void
CenteredSimilarity2DTransform<TParametersValueType>
::SetFixedParameters( const FixedParametersType & itkNotUsed(parameters) )
{
  // no fixed parameters
}


template<typename TParametersValueType>
const typename CenteredSimilarity2DTransform<TParametersValueType>
::FixedParametersType &
CenteredSimilarity2DTransform<TParametersValueType>
::GetFixedParameters() const
{
  // return dummy parameters
  return this->m_FixedParameters;
}


template<typename TParametersValueType>
void
CenteredSimilarity2DTransform<TParametersValueType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}


template<typename TParametersValueType>
void
CenteredSimilarity2DTransform<TParametersValueType>
::CloneInverseTo(Pointer & result) const
{
  result = New();
  this->GetInverse( result.GetPointer() );
}


template<typename TParametersValueType>
bool
CenteredSimilarity2DTransform<TParametersValueType>
::GetInverse(Self *inverse) const
{
  if( !inverse )
    {
    return false;
    }

  inverse->SetFixedParameters( this->GetFixedParameters() );
  this->GetInverseMatrix();
  if( this->GetSingular() )
    {
    return false;
    }
  inverse->SetCenter( this->GetCenter() );  // inverse have the same center
  inverse->SetScale( NumericTraits<double>::OneValue() / this->GetScale() );
  inverse->SetAngle( -this->GetAngle() );
  inverse->SetTranslation( -( this->GetInverseMatrix() * this->GetTranslation() ) );
  return true;
}


template<typename TParametersValueType>
typename CenteredSimilarity2DTransform<TParametersValueType>::InverseTransformBasePointer
CenteredSimilarity2DTransform<TParametersValueType>
::GetInverseTransform() const
{
  Pointer inv = New();

  if( this->GetInverse(inv) )
    {
    return inv.GetPointer();
    }
  return ITK_NULLPTR;
}


template<typename TParametersValueType>
void
CenteredSimilarity2DTransform<TParametersValueType>
::CloneTo(Pointer & result) const
{
  result = New();
  result->SetCenter( this->GetCenter() );
  result->SetScale( this->GetScale() );
  result->SetAngle( this->GetAngle() );
  result->SetTranslation( this->GetTranslation() );
}

} // end namespace itk

#endif
