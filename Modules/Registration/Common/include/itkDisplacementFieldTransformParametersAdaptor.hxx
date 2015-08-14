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
#ifndef itkDisplacementFieldTransformParametersAdaptor_hxx
#define itkDisplacementFieldTransformParametersAdaptor_hxx

#include "itkDisplacementFieldTransformParametersAdaptor.h"

#include "itkIdentityTransform.h"
#include "itkVectorResampleImageFilter.h"
#include "itkVectorLinearInterpolateImageFunction.h"
#include "itkMath.h"

namespace itk
{

template<typename TTransform>
DisplacementFieldTransformParametersAdaptor<TTransform>
::DisplacementFieldTransformParametersAdaptor()
{
  this->m_RequiredFixedParameters.SetSize( SpaceDimension * ( SpaceDimension + 3 ) );
  this->m_RequiredFixedParameters.Fill( 0.0 );
}

template<typename TTransform>
DisplacementFieldTransformParametersAdaptor<TTransform>
::~DisplacementFieldTransformParametersAdaptor()
{
}

template<typename TTransform>
void
DisplacementFieldTransformParametersAdaptor<TTransform>
::SetRequiredSize( const SizeType & size )
{
  bool isModified = false;
  for( SizeValueType d = 0; d < SpaceDimension; d++ )
    {
    if( Math::NotExactlyEquals(this->m_RequiredFixedParameters[d], size[d]) )
      {
      isModified = true;
      }
    this->m_RequiredFixedParameters[d] = size[d];
    }

  if( isModified )
    {
    itkDebugMacro( "Setting size to " << size );
    this->Modified();
    }
}

template<typename TTransform>
const typename DisplacementFieldTransformParametersAdaptor<TTransform>::SizeType
DisplacementFieldTransformParametersAdaptor<TTransform>
::GetRequiredSize() const
{
  SizeType size;
  for( SizeValueType d = 0; d < SpaceDimension; d++ )
    {
    size[d] = static_cast<SizeValueType>( this->m_RequiredFixedParameters[d] );
    }
  return size;
}

template<typename TTransform>
void
DisplacementFieldTransformParametersAdaptor<TTransform>
::SetRequiredOrigin( const PointType & origin )
{
  bool isModified = false;
  for( SizeValueType d = 0; d < SpaceDimension; d++ )
    {
    if( Math::NotExactlyEquals(this->m_RequiredFixedParameters[SpaceDimension + d], origin[d]) )
      {
      isModified = true;
      }
    this->m_RequiredFixedParameters[SpaceDimension + d] = origin[d];
    }

  if( isModified )
    {
    itkDebugMacro( "Setting origin to " << origin );
    this->Modified();
    }
}

template<typename TTransform>
const typename DisplacementFieldTransformParametersAdaptor<TTransform>::PointType
DisplacementFieldTransformParametersAdaptor<TTransform>
::GetRequiredOrigin() const
{
  PointType origin;
  for( SizeValueType d = 0; d < SpaceDimension; d++ )
    {
    origin[d] = this->m_RequiredFixedParameters[SpaceDimension + d];
    }
  return origin;
}

template<typename TTransform>
void
DisplacementFieldTransformParametersAdaptor<TTransform>
::SetRequiredSpacing( const SpacingType & spacing )
{
  bool isModified = false;
  for( SizeValueType d = 0; d < SpaceDimension; d++ )
    {
    if( Math::NotExactlyEquals(this->m_RequiredFixedParameters[2*SpaceDimension + d], spacing[d]) )
      {
      isModified = true;
      }
    this->m_RequiredFixedParameters[2*SpaceDimension + d] = spacing[d];
    }

  if( isModified )
    {
    itkDebugMacro( "Setting spacing to " << spacing );
    this->Modified();
    }
}

template<typename TTransform>
const typename DisplacementFieldTransformParametersAdaptor<TTransform>::SpacingType
DisplacementFieldTransformParametersAdaptor<TTransform>
::GetRequiredSpacing() const
{
  SpacingType spacing;
  for( SizeValueType d = 0; d < SpaceDimension; d++ )
    {
    spacing[d] = this->m_RequiredFixedParameters[2*SpaceDimension + d];
    }
  return spacing;
}

template<typename TTransform>
void
DisplacementFieldTransformParametersAdaptor<TTransform>
::SetRequiredDirection( const DirectionType & direction )
{
  bool isModified = false;
  for( SizeValueType di = 0; di < SpaceDimension; di++ )
    {
    for( SizeValueType dj = 0; dj < SpaceDimension; dj++ )
      {
      if( Math::NotExactlyEquals(this->m_RequiredFixedParameters[3 * SpaceDimension + ( di * SpaceDimension + dj )], direction[di][dj]) )
        {
        isModified = true;
        }
      this->m_RequiredFixedParameters[3 * SpaceDimension + ( di * SpaceDimension + dj )] = direction[di][dj];
      }
    }

  if( isModified )
    {
    itkDebugMacro( "Setting direction to " << direction );
    this->Modified();
    }
}

template<typename TTransform>
const typename DisplacementFieldTransformParametersAdaptor<TTransform>::DirectionType
DisplacementFieldTransformParametersAdaptor<TTransform>
::GetRequiredDirection() const
{
  DirectionType direction;
  for( SizeValueType di = 0; di < SpaceDimension; di++ )
    {
    for( SizeValueType dj = 0; dj < SpaceDimension; dj++ )
      {
      direction[di][dj] = this->m_RequiredFixedParameters[3 * SpaceDimension + ( di * SpaceDimension + dj )];
      }
    }
  return direction;
}

template<typename TTransform>
void
DisplacementFieldTransformParametersAdaptor<TTransform>
::AdaptTransformParameters()
{
  if( !this->m_Transform )
    {
    itkExceptionMacro( "Transform has not been set." );
    return;
    }

  if( this->m_RequiredFixedParameters == this->m_Transform->GetFixedParameters() )
    {
    return;
    }

  const SizeType newFieldSize = this->GetRequiredSize();
  const PointType newFieldOrigin = this->GetRequiredOrigin();
  const SpacingType newFieldSpacing = this->GetRequiredSpacing();
  const DirectionType newFieldDirection = this->GetRequiredDirection();

  typedef IdentityTransform<ParametersValueType, SpaceDimension> IdentityTransformType;
  typename IdentityTransformType::Pointer identityTransform = IdentityTransformType::New();
  identityTransform->SetIdentity();

  typedef VectorLinearInterpolateImageFunction<DisplacementFieldType, ParametersValueType> LinearInterpolatorType;
  typename LinearInterpolatorType::Pointer interpolator = LinearInterpolatorType::New();
  interpolator->SetInputImage( this->m_Transform->GetDisplacementField() );

  typedef VectorResampleImageFilter<DisplacementFieldType, DisplacementFieldType, ParametersValueType> ResamplerType;
  typename ResamplerType::Pointer resampler = ResamplerType::New();
  resampler->SetInput( this->m_Transform->GetDisplacementField() );
  resampler->SetOutputDirection( newFieldDirection );
  resampler->SetOutputOrigin( newFieldOrigin );
  resampler->SetOutputSpacing( newFieldSpacing );
  resampler->SetSize( newFieldSize );
  resampler->SetTransform( identityTransform );
  resampler->SetInterpolator( interpolator );

  typename DisplacementFieldType::Pointer newDisplacementField = resampler->GetOutput();
  newDisplacementField->Update();
  newDisplacementField->DisconnectPipeline();

  typename DisplacementFieldType::Pointer newInverseDisplacementField = ITK_NULLPTR;
  if( this->m_Transform->GetInverseDisplacementField() )
    {
    typename LinearInterpolatorType::Pointer inverseInterpolator = LinearInterpolatorType::New();
    inverseInterpolator->SetInputImage( this->m_Transform->GetInverseDisplacementField() );

    typename ResamplerType::Pointer inverseResampler = ResamplerType::New();
    inverseResampler->SetInput( this->m_Transform->GetInverseDisplacementField() );
    inverseResampler->SetOutputDirection( newFieldDirection );
    inverseResampler->SetOutputOrigin( newFieldOrigin );
    inverseResampler->SetOutputSpacing( newFieldSpacing );
    inverseResampler->SetSize( newFieldSize );
    inverseResampler->SetTransform( identityTransform );
    inverseResampler->SetInterpolator( inverseInterpolator );

    newInverseDisplacementField = inverseResampler->GetOutput();
    newInverseDisplacementField->Update();
    newInverseDisplacementField->DisconnectPipeline();
    }
  this->m_Transform->SetDisplacementField( newDisplacementField );
  this->m_Transform->SetInverseDisplacementField( newInverseDisplacementField );
}

}  // namespace itk

#endif
