/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkVariationalRegistrationDemonsFunction_hxx
#define itkVariationalRegistrationDemonsFunction_hxx

#include "itkVariationalRegistrationDemonsFunction.h"
#include "itkMath.h"

namespace itk
{

/**
 * Default constructor
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
VariationalRegistrationDemonsFunction<TFixedImage, TMovingImage, TDisplacementField>::
  VariationalRegistrationDemonsFunction()
{
  m_DenominatorThreshold = 1e-9;
  m_IntensityDifferenceThreshold = 0.001;

  m_Normalizer = 1.0;
  m_FixedImageGradientCalculator = GradientCalculatorType::New();
  m_WarpedImageGradientCalculator = GradientCalculatorType::New();

  m_GradientType = GRADIENT_TYPE_WARPED;
}

/**
 * Standard "PrintSelf" method.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalRegistrationDemonsFunction<TFixedImage, TMovingImage, TDisplacementField>::PrintSelf(std::ostream & os,
                                                                                                Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FixedImageGradientCalculator: ";
  os << m_FixedImageGradientCalculator.GetPointer() << std::endl;
  os << indent << "WarpedImageGradientCalculator: ";
  os << m_WarpedImageGradientCalculator.GetPointer() << std::endl;
  os << indent << "GradientType: ";
  os << m_GradientType << std::endl;

  os << indent << "DenominatorThreshold: ";
  os << m_DenominatorThreshold << std::endl;
  os << indent << "IntensityDifferenceThreshold: ";
  os << m_IntensityDifferenceThreshold << std::endl;
  os << indent << "Normalizer: ";
  os << m_Normalizer << std::endl;
}

/**
 * Set the function state values before each iteration
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalRegistrationDemonsFunction<TFixedImage, TMovingImage, TDisplacementField>::InitializeIteration()
{
  // Call superclass method
  Superclass::InitializeIteration();

  // cache fixed image information
  m_ZeroUpdateReturn.Fill(0.0);

  // compute the normalizer
  m_Normalizer = 0.0;
  SpacingType fixedImageSpacing = this->GetFixedImage()->GetSpacing();
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
typename VariationalRegistrationDemonsFunction<TFixedImage, TMovingImage, TDisplacementField>::PixelType
VariationalRegistrationDemonsFunction<TFixedImage, TMovingImage, TDisplacementField>::ComputeUpdate(
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
    gradient =
      m_WarpedImageGradientCalculator->EvaluateAtIndex(index) + m_FixedImageGradientCalculator->EvaluateAtIndex(index);
  }
  else
  {
    itkExceptionMacro(<< "Unknown gradient type!");
  }

  // Compute Update.
  // In the original equation the denominator is defined as (g-f)^2 + grad_mag^2.
  // However there is a mismatch in units between the two terms.
  // The units for the second term is intensity^2/mm^2 while the
  // units for the first term is intensity^2. This mismatch is particularly
  // problematic when the fixed image does not have unit spacing.
  // In this implementation, we normalize the first term by a factor K,
  // such that denominator = (g-f)^2/K + grad_mag^2
  // where K = mean square spacing to compensate for the mismatch in units.

  // Calculate gradient squared magnitude
  const double gradientSquaredMagnitude = gradient.GetSquaredNorm();

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
    // Calculate the denominator
    const double denominator = sqr_speedValue / m_Normalizer + gradientSquaredMagnitude;

    if (denominator < m_DenominatorThreshold)
      update = m_ZeroUpdateReturn;
    else
      for (unsigned int j = 0; j < ImageDimension; j++)
      {
        update[j] = speedValue * gradient[j] / denominator;
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

} // end namespace itk

#endif
