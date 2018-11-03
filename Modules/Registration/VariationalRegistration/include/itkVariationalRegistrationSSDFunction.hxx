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
#ifndef itkVariationalRegistrationSSDFunction_hxx
#define itkVariationalRegistrationSSDFunction_hxx

#include "itkVariationalRegistrationSSDFunction.h"
#include "itkMath.h"

namespace itk
{

/**
 * Default constructor
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
VariationalRegistrationSSDFunction<TFixedImage, TMovingImage, TDisplacementField>::VariationalRegistrationSSDFunction()
{
  RadiusType r;
  for (unsigned int j = 0; j < ImageDimension; j++)
  {
    r[j] = 0;
  }
  this->SetRadius(r);

  m_IntensityDifferenceThreshold = 0.001;

  m_Normalizer = 1.0;
  m_FixedImageGradientCalculator = GradientCalculatorType::New();
  m_WarpedImageGradientCalculator = GradientCalculatorType::New();

  m_GradientType = GRADIENT_TYPE_WARPED;
}

/**
 * Set the function state values before each iteration
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalRegistrationSSDFunction<TFixedImage, TMovingImage, TDisplacementField>::InitializeIteration()
{
  // Call superclass method
  Superclass::InitializeIteration();

  // cache fixed image information
  SpacingType fixedImageSpacing = this->GetFixedImage()->GetSpacing();
  m_ZeroUpdateReturn.Fill(0.0);

  // compute the normalizer
  m_Normalizer = 0.0;
  for (unsigned int k = 0; k < ImageDimension; k++)
  {
    m_Normalizer += fixedImageSpacing[k] * fixedImageSpacing[k];
  }
  m_Normalizer /= static_cast<double>(ImageDimension);

  // setup gradient calculator
  m_WarpedImageGradientCalculator->SetInputImage(this->GetWarpedImage());
  m_FixedImageGradientCalculator->SetInputImage(this->GetFixedImage());
}

/**
 * Compute update at a specific neighborhood
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
typename VariationalRegistrationSSDFunction<TFixedImage, TMovingImage, TDisplacementField>::PixelType
VariationalRegistrationSSDFunction<TFixedImage, TMovingImage, TDisplacementField>::ComputeUpdate(
  const NeighborhoodType & it,
  void *                   gd,
  const FloatOffsetType &  itkNotUsed(offset))
{
  // Get fixed image related information
  // Note: no need to check the index is within
  // fixed image buffer. This is done by the external filter.
  const IndexType index = it.GetIndex();

  // Check if index lies inside mask
  const MaskImageType * mask = this->GetMaskImage();
  if (mask && (mask->GetPixel(index) <= this->GetMaskBackgroundThreshold()))
  {
    return m_ZeroUpdateReturn;
  }

  const auto warpedValue = (double)this->GetWarpedImage()->GetPixel(index);
  const auto fixedValue = (double)this->GetFixedImage()->GetPixel(index);

  // Calculate spped value
  const double speedValue = fixedValue - warpedValue;
  const double sqr_speedValue = itk::Math::sqr(speedValue);

  // Calculate update
  PixelType update;
  if (itk::Math::abs(speedValue) < m_IntensityDifferenceThreshold)
  {
    update = m_ZeroUpdateReturn;
  }
  else
  {
    typename GradientCalculatorType::OutputType gradient;

    // Compute the gradient of either fixed or moving image
    if (m_GradientType == GRADIENT_TYPE_WARPED)
    {
      gradient = m_WarpedImageGradientCalculator->EvaluateAtIndex(index);
    }
    else if (m_GradientType == GRADIENT_TYPE_FIXED)
    {
      gradient = m_FixedImageGradientCalculator->EvaluateAtIndex(index);
    }
    else if (m_GradientType == GRADIENT_TYPE_SYMMETRIC)
    {
      // Does not have to be divided by 2, normalization is done afterwards
      gradient = m_WarpedImageGradientCalculator->EvaluateAtIndex(index) +
                 m_FixedImageGradientCalculator->EvaluateAtIndex(index);
    }
    else
    {
      itkExceptionMacro(<< "Unknown gradient type!");
    }

    for (unsigned int j = 0; j < ImageDimension; j++)
    {
      update[j] = speedValue * gradient[j];
    }
  }

  // Update the global data (metric etc.)
  auto * globalData = (GlobalDataStruct *)gd;
  if (globalData)
  {
    globalData->m_NumberOfPixelsProcessed += 1;
    globalData->m_SumOfMetricValues += sqr_speedValue;
    globalData->m_SumOfSquaredChange += update.GetSquaredNorm();
  }

  return update;
}

/**
 * Standard "PrintSelf" method.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalRegistrationSSDFunction<TFixedImage, TMovingImage, TDisplacementField>::PrintSelf(std::ostream & os,
                                                                                             Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FixedImageGradientCalculator: ";
  os << m_FixedImageGradientCalculator.GetPointer() << std::endl;
  os << indent << "WarpedImageGradientCalculator: ";
  os << m_WarpedImageGradientCalculator.GetPointer() << std::endl;
  os << indent << "GradientType: ";
  os << m_GradientType << std::endl;

  os << indent << "IntensityDifferenceThreshold: ";
  os << m_IntensityDifferenceThreshold << std::endl;
  os << indent << "Normalizer: ";
  os << m_Normalizer << std::endl;
}

} // end namespace itk

#endif
