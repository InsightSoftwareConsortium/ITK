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
#ifndef itkTimeVaryingVelocityFieldIntegrationImageFilter_hxx
#define itkTimeVaryingVelocityFieldIntegrationImageFilter_hxx

#include "itkImageRegionIteratorWithIndex.h"
#include "itkVectorLinearInterpolateImageFunction.h"

namespace itk
{

/*
 * TimeVaryingVelocityFieldIntegrationImageFilter class definitions
 */
template <typename TTimeVaryingVelocityField, typename TDisplacementField>
TimeVaryingVelocityFieldIntegrationImageFilter<TTimeVaryingVelocityField,
                                               TDisplacementField>::TimeVaryingVelocityFieldIntegrationImageFilter()
{
  this->m_LowerTimeBound = 0.0, this->m_UpperTimeBound = 1.0, this->m_NumberOfIntegrationSteps = 100;
  this->m_NumberOfTimePoints = 0;
  this->SetNumberOfRequiredInputs(1);

  if (InputImageDimension - 1 != OutputImageDimension)
  {
    itkExceptionMacro("The time-varying velocity field (input) should have "
                      << "dimensionality of 1 greater than the deformation field (output). ");
  }

  using DefaultVelocityFieldInterpolatorType =
    VectorLinearInterpolateImageFunction<TimeVaryingVelocityFieldType, ScalarType>;

  typename DefaultVelocityFieldInterpolatorType::Pointer velocityFieldInterpolator =
    DefaultVelocityFieldInterpolatorType::New();

  this->m_VelocityFieldInterpolator = velocityFieldInterpolator;

  using DefaultDisplacementFieldInterpolatorType =
    VectorLinearInterpolateImageFunction<DisplacementFieldType, ScalarType>;

  typename DefaultDisplacementFieldInterpolatorType::Pointer deformationFieldInterpolator =
    DefaultDisplacementFieldInterpolatorType::New();

  this->m_DisplacementFieldInterpolator = deformationFieldInterpolator;
  this->DynamicMultiThreadingOn();
}

template <typename TTimeVaryingVelocityField, typename TDisplacementField>
void
TimeVaryingVelocityFieldIntegrationImageFilter<TTimeVaryingVelocityField,
                                               TDisplacementField>::GenerateOutputInformation()
{
  const TimeVaryingVelocityFieldType * input = this->GetInput();
  DisplacementFieldType *              output = this->GetOutput();
  this->m_NumberOfTimePoints = input->GetLargestPossibleRegion().GetSize()[OutputImageDimension];
  if (!input || !output)
  {
    return;
  }

  //
  // The ImageBase::CopyInformation() method ca not be used here
  // because these two images have different dimensions. Therefore
  // the individual elements must be copied for the common dimensions.
  //
  using SizeType = typename DisplacementFieldType::SizeType;
  using SpacingType = typename DisplacementFieldType::SpacingType;
  using OriginType = typename DisplacementFieldType::PointType;
  using DirectionType = typename DisplacementFieldType::DirectionType;

  SizeType      size;
  SpacingType   spacing;
  OriginType    origin;
  DirectionType direction;

  using InputSizeType = typename TimeVaryingVelocityFieldType::SizeType;
  using InputSpacingType = typename TimeVaryingVelocityFieldType::SpacingType;
  using InputOriginType = typename TimeVaryingVelocityFieldType::PointType;
  using InputDirectionType = typename TimeVaryingVelocityFieldType::DirectionType;
  using InputRegionType = typename TimeVaryingVelocityFieldType::RegionType;

  const InputSpacingType &   inputSpacing = input->GetSpacing();
  const InputOriginType &    inputOrigin = input->GetOrigin();
  const InputDirectionType & inputDirection = input->GetDirection();
  const InputRegionType      requestedRegion = input->GetRequestedRegion();
  const InputSizeType        requestedSize = requestedRegion.GetSize();

  for (unsigned int i = 0; i < OutputImageDimension; ++i)
  {
    size[i] = requestedSize[i];
    spacing[i] = inputSpacing[i];
    origin[i] = inputOrigin[i];

    for (unsigned int j = 0; j < OutputImageDimension; ++j)
    {
      direction[i][j] = inputDirection[i][j];
    }
  }

  output->SetOrigin(origin);
  output->SetSpacing(spacing);
  output->SetDirection(direction);
  output->SetRegions(size);
}

template <typename TTimeVaryingVelocityField, typename TDisplacementField>
void
TimeVaryingVelocityFieldIntegrationImageFilter<TTimeVaryingVelocityField,
                                               TDisplacementField>::BeforeThreadedGenerateData()
{
  this->m_VelocityFieldInterpolator->SetInputImage(this->GetInput());
  this->m_NumberOfTimePoints = this->GetInput()->GetLargestPossibleRegion().GetSize()[InputImageDimension - 1];
  if (!this->m_InitialDiffeomorphism.IsNull())
  {
    this->m_DisplacementFieldInterpolator->SetInputImage(this->m_InitialDiffeomorphism);
  }
}

template <typename TTimeVaryingVelocityField, typename TDisplacementField>
void
TimeVaryingVelocityFieldIntegrationImageFilter<TTimeVaryingVelocityField, TDisplacementField>::
  DynamicThreadedGenerateData(const OutputRegionType & region)
{
  if (Math::ExactlyEquals(this->m_LowerTimeBound, this->m_UpperTimeBound) || this->m_NumberOfIntegrationSteps == 0)
  {
    this->GetOutput()->FillBuffer(itk::NumericTraits<typename DisplacementFieldType::PixelType>::Zero);
    return;
  }

  const TimeVaryingVelocityFieldType * inputField = this->GetInput();

  typename DisplacementFieldType::Pointer outputField = this->GetOutput();

  ImageRegionIteratorWithIndex<DisplacementFieldType> It(outputField, region);

  for (It.GoToBegin(); !It.IsAtEnd(); ++It)
  {
    PointType point;
    outputField->TransformIndexToPhysicalPoint(It.GetIndex(), point);
    VectorType displacement = this->IntegrateVelocityAtPoint(point, inputField);
    It.Set(displacement);
  }
}

template <typename TTimeVaryingVelocityField, typename TDisplacementField>
typename TimeVaryingVelocityFieldIntegrationImageFilter<TTimeVaryingVelocityField, TDisplacementField>::VectorType
TimeVaryingVelocityFieldIntegrationImageFilter<TTimeVaryingVelocityField, TDisplacementField>::IntegrateVelocityAtPoint(
  const PointType &                    initialSpatialPoint,
  const TimeVaryingVelocityFieldType * inputField)
{
  // Solve the initial value problem using fourth-order Runge-Kutta
  //    y' = f(t, y), y(t_0) = y_0

  VectorType zeroVector;
  zeroVector.Fill(0.0);

  // Initial conditions

  VectorType displacement = zeroVector;
  if (!this->m_InitialDiffeomorphism.IsNull())
  {
    if (this->m_DisplacementFieldInterpolator->IsInsideBuffer(initialSpatialPoint))
    {
      displacement = this->m_DisplacementFieldInterpolator->Evaluate(initialSpatialPoint);
    }
  }

  // Perform the integration.
  // With TimeBoundsAsRates On, we need to map the time dimension of the input image to the
  // normalized domain of [0,1].

  RealType timeOrigin = 0.;
  RealType timeScale = 1.;
  if (m_TimeBoundsAsRates)
  {
    typename TimeVaryingVelocityFieldType::PointType spaceTimeOrigin = inputField->GetOrigin();

    using RegionType = typename TimeVaryingVelocityFieldType::RegionType;
    RegionType region = inputField->GetLargestPossibleRegion();

    typename RegionType::IndexType lastIndex = region.GetIndex();
    typename RegionType::SizeType  size = region.GetSize();
    for (unsigned int d = 0; d < InputImageDimension; ++d)
    {
      lastIndex[d] += (size[d] - 1);
    }

    typename TimeVaryingVelocityFieldType::PointType spaceTimeEnd;
    inputField->TransformIndexToPhysicalPoint(lastIndex, spaceTimeEnd);

    timeOrigin = spaceTimeOrigin[InputImageDimension - 1];
    const RealType timeEnd = spaceTimeEnd[InputImageDimension - 1];
    timeScale = timeEnd - timeOrigin;
  }

  RealType timePointInImage = timeOrigin + this->m_LowerTimeBound * timeScale;

  // Calculate the delta time used for integration
  const RealType deltaTime =
    (this->m_UpperTimeBound - this->m_LowerTimeBound) / static_cast<RealType>(this->m_NumberOfIntegrationSteps);
  const RealType deltaTimeInImage = timeScale * deltaTime;

  for (unsigned int n = 0; n < this->m_NumberOfIntegrationSteps; ++n)
  {
    typename TimeVaryingVelocityFieldType::PointType x1;
    typename TimeVaryingVelocityFieldType::PointType x2;
    typename TimeVaryingVelocityFieldType::PointType x3;
    typename TimeVaryingVelocityFieldType::PointType x4;

    for (unsigned int d = 0; d < OutputImageDimension; ++d)
    {
      x1[d] = initialSpatialPoint[d] + displacement[d];
      x2[d] = x1[d];
      x3[d] = x1[d];
      x4[d] = x1[d];
    }

    x1[OutputImageDimension] = timePointInImage;
    x2[OutputImageDimension] = timePointInImage + 0.5 * deltaTimeInImage;
    x3[OutputImageDimension] = timePointInImage + 0.5 * deltaTimeInImage;
    x4[OutputImageDimension] = timePointInImage + deltaTimeInImage;

    VectorType f1 = zeroVector;
    if (this->m_VelocityFieldInterpolator->IsInsideBuffer(x1))
    {
      f1 = this->m_VelocityFieldInterpolator->Evaluate(x1);
      for (unsigned int jj = 0; jj < OutputImageDimension; ++jj)
      {
        x2[jj] += f1[jj] * deltaTime * 0.5;
      }
    }

    VectorType f2 = zeroVector;
    if (this->m_VelocityFieldInterpolator->IsInsideBuffer(x2))
    {
      f2 = this->m_VelocityFieldInterpolator->Evaluate(x2);
      for (unsigned int jj = 0; jj < OutputImageDimension; ++jj)
      {
        x3[jj] += f2[jj] * deltaTime * 0.5;
      }
    }

    VectorType f3 = zeroVector;
    if (this->m_VelocityFieldInterpolator->IsInsideBuffer(x3))
    {
      f3 = this->m_VelocityFieldInterpolator->Evaluate(x3);
      for (unsigned int jj = 0; jj < OutputImageDimension; ++jj)
      {
        x4[jj] += f3[jj] * deltaTime;
      }
    }

    VectorType f4 = zeroVector;
    if (this->m_VelocityFieldInterpolator->IsInsideBuffer(x4))
    {
      f4 = this->m_VelocityFieldInterpolator->Evaluate(x4);
    }

    for (unsigned int jj = 0; jj < OutputImageDimension; ++jj)
    {
      x1[jj] += deltaTime / 6.0 * (f1[jj] + 2.0 * f2[jj] + 2.0 * f3[jj] + f4[jj]);
      displacement[jj] = x1[jj] - initialSpatialPoint[jj];
    }

    timePointInImage += deltaTimeInImage;
  }
  return displacement;
}

template <typename TTimeVaryingVelocityField, typename TDisplacementField>
void
TimeVaryingVelocityFieldIntegrationImageFilter<TTimeVaryingVelocityField, TDisplacementField>::PrintSelf(
  std::ostream & os,
  Indent         indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "VelocityFieldInterpolator: " << this->m_VelocityFieldInterpolator << std::endl;
  os << indent << "LowerTimeBound: " << this->m_LowerTimeBound << std::endl;
  os << indent << "UpperTimeBound: " << this->m_UpperTimeBound << std::endl;
  os << indent << "NumberOfIntegrationSteps: " << this->m_NumberOfIntegrationSteps << std::endl;
  itkPrintSelfObjectMacro(InitialDiffeomorphism);
  itkPrintSelfObjectMacro(DisplacementFieldInterpolator);
}

} // end namespace itk

#endif
