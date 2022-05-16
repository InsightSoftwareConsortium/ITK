/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkConstantVelocityFieldTransformParametersAdaptor_hxx
#define itkConstantVelocityFieldTransformParametersAdaptor_hxx


#include "itkIdentityTransform.h"
#include "itkResampleImageFilter.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkMath.h"

namespace itk
{

template <typename TTransform>
ConstantVelocityFieldTransformParametersAdaptor<TTransform>::ConstantVelocityFieldTransformParametersAdaptor()
{
  this->m_RequiredFixedParameters.SetSize(ConstantVelocityFieldDimension * (ConstantVelocityFieldDimension + 3));
  this->m_RequiredFixedParameters.Fill(0.0);
}

template <typename TTransform>
void
ConstantVelocityFieldTransformParametersAdaptor<TTransform>::SetRequiredSize(const SizeType & size)
{
  bool isModified = false;
  for (SizeValueType d = 0; d < ConstantVelocityFieldDimension; ++d)
  {
    if (Math::NotExactlyEquals(this->m_RequiredFixedParameters[d], size[d]))
    {
      isModified = true;
    }
    this->m_RequiredFixedParameters[d] = size[d];
  }

  if (isModified)
  {
    itkDebugMacro("Setting size to " << size);
    this->Modified();
  }
}

template <typename TTransform>
auto
ConstantVelocityFieldTransformParametersAdaptor<TTransform>::GetRequiredSize() const -> const SizeType
{
  SizeType size;
  for (SizeValueType d = 0; d < ConstantVelocityFieldDimension; ++d)
  {
    size[d] = static_cast<SizeValueType>(this->m_RequiredFixedParameters[d]);
  }
  return size;
}

template <typename TTransform>
void
ConstantVelocityFieldTransformParametersAdaptor<TTransform>::SetRequiredOrigin(const PointType & origin)
{
  bool isModified = false;
  for (SizeValueType d = 0; d < ConstantVelocityFieldDimension; ++d)
  {
    if (Math::NotExactlyEquals(this->m_RequiredFixedParameters[ConstantVelocityFieldDimension + d], origin[d]))
    {
      isModified = true;
    }
    this->m_RequiredFixedParameters[ConstantVelocityFieldDimension + d] = origin[d];
  }

  if (isModified)
  {
    itkDebugMacro("Setting origin to " << origin);
    this->Modified();
  }
}

template <typename TTransform>
auto
ConstantVelocityFieldTransformParametersAdaptor<TTransform>::GetRequiredOrigin() const -> const PointType
{
  PointType origin;
  for (SizeValueType d = 0; d < ConstantVelocityFieldDimension; ++d)
  {
    origin[d] = this->m_RequiredFixedParameters[ConstantVelocityFieldDimension + d];
  }
  return origin;
}

template <typename TTransform>
void
ConstantVelocityFieldTransformParametersAdaptor<TTransform>::SetRequiredSpacing(const SpacingType & spacing)
{
  bool isModified = false;
  for (SizeValueType d = 0; d < ConstantVelocityFieldDimension; ++d)
  {
    if (Math::NotExactlyEquals(this->m_RequiredFixedParameters[2 * ConstantVelocityFieldDimension + d], spacing[d]))
    {
      isModified = true;
    }
    this->m_RequiredFixedParameters[2 * ConstantVelocityFieldDimension + d] = spacing[d];
  }

  if (isModified)
  {
    itkDebugMacro("Setting spacing to " << spacing);
    this->Modified();
  }
}

template <typename TTransform>
auto
ConstantVelocityFieldTransformParametersAdaptor<TTransform>::GetRequiredSpacing() const -> const SpacingType
{
  SpacingType spacing;
  for (SizeValueType d = 0; d < ConstantVelocityFieldDimension; ++d)
  {
    spacing[d] = this->m_RequiredFixedParameters[2 * ConstantVelocityFieldDimension + d];
  }
  return spacing;
}

template <typename TTransform>
void
ConstantVelocityFieldTransformParametersAdaptor<TTransform>::SetRequiredDirection(const DirectionType & direction)
{
  bool isModified = false;
  for (SizeValueType di = 0; di < ConstantVelocityFieldDimension; ++di)
  {
    for (SizeValueType dj = 0; dj < ConstantVelocityFieldDimension; ++dj)
    {
      if (Math::NotExactlyEquals(this->m_RequiredFixedParameters[3 * ConstantVelocityFieldDimension +
                                                                 (di * ConstantVelocityFieldDimension + dj)],
                                 direction[di][dj]))
      {
        isModified = true;
      }
      this->m_RequiredFixedParameters[3 * ConstantVelocityFieldDimension + (di * ConstantVelocityFieldDimension + dj)] =
        direction[di][dj];
    }
  }

  if (isModified)
  {
    itkDebugMacro("Setting direction to " << direction);
    this->Modified();
  }
}

template <typename TTransform>
auto
ConstantVelocityFieldTransformParametersAdaptor<TTransform>::GetRequiredDirection() const -> const DirectionType
{
  DirectionType direction;
  for (SizeValueType di = 0; di < ConstantVelocityFieldDimension; ++di)
  {
    for (SizeValueType dj = 0; dj < ConstantVelocityFieldDimension; ++dj)
    {
      direction[di][dj] =
        this
          ->m_RequiredFixedParameters[3 * ConstantVelocityFieldDimension + (di * ConstantVelocityFieldDimension + dj)];
    }
  }
  return direction;
}

template <typename TTransform>
void
ConstantVelocityFieldTransformParametersAdaptor<TTransform>::AdaptTransformParameters()
{
  if (!this->m_Transform)
  {
    itkExceptionMacro("Transform has not been set.");
  }

  if (this->m_RequiredFixedParameters == this->m_Transform->GetFixedParameters())
  {
    return;
  }

  const SizeType      newFieldSize = this->GetRequiredSize();
  const PointType     newFieldOrigin = this->GetRequiredOrigin();
  const SpacingType   newFieldSpacing = this->GetRequiredSpacing();
  const DirectionType newFieldDirection = this->GetRequiredDirection();

  using IdentityTransformType = IdentityTransform<ParametersValueType, ConstantVelocityFieldDimension>;
  auto identityTransform = IdentityTransformType::New();
  identityTransform->SetIdentity();

  using LinearInterpolatorType = LinearInterpolateImageFunction<ConstantVelocityFieldType, ParametersValueType>;
  auto interpolator = LinearInterpolatorType::New();
  interpolator->SetInputImage(this->m_Transform->GetConstantVelocityField());

  using ResamplerType = ResampleImageFilter<ConstantVelocityFieldType, ConstantVelocityFieldType, ParametersValueType>;
  auto resampler = ResamplerType::New();
  resampler->SetInput(this->m_Transform->GetConstantVelocityField());
  resampler->SetOutputDirection(newFieldDirection);
  resampler->SetOutputOrigin(newFieldOrigin);
  resampler->SetOutputSpacing(newFieldSpacing);
  resampler->SetSize(newFieldSize);
  resampler->SetTransform(identityTransform);
  resampler->SetInterpolator(interpolator);

  typename ConstantVelocityFieldType::Pointer newConstantVelocityField = resampler->GetOutput();
  newConstantVelocityField->Update();
  newConstantVelocityField->DisconnectPipeline();

  this->m_Transform->SetConstantVelocityField(newConstantVelocityField);
  this->m_Transform->IntegrateVelocityField();
}

} // namespace itk

#endif
