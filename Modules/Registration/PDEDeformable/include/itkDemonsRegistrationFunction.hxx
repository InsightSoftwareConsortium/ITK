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
#ifndef itkDemonsRegistrationFunction_hxx
#define itkDemonsRegistrationFunction_hxx

#include "itkDemonsRegistrationFunction.h"
#include "itkMacro.h"
#include "itkMath.h"

namespace itk
{
/**
 * Default constructor
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
DemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::DemonsRegistrationFunction()
{
  RadiusType   r;
  unsigned int j;

  for ( j = 0; j < ImageDimension; j++ )
    {
    r[j] = 0;
    }
  this->SetRadius(r);

  m_TimeStep = 1.0;
  m_DenominatorThreshold = 1e-9;
  m_IntensityDifferenceThreshold = 0.001;
  this->SetMovingImage(ITK_NULLPTR);
  this->SetFixedImage(ITK_NULLPTR);
  //m_FixedImageSpacing.Fill( 1.0 );
  //m_FixedImageOrigin.Fill( 0.0 );
  m_Normalizer = 1.0;
  m_FixedImageGradientCalculator = GradientCalculatorType::New();

  typename DefaultInterpolatorType::Pointer interp =
    DefaultInterpolatorType::New();

  m_MovingImageInterpolator = static_cast< InterpolatorType * >(
    interp.GetPointer() );

  m_Metric = NumericTraits< double >::max();
  m_SumOfSquaredDifference = 0.0;
  m_NumberOfPixelsProcessed = 0L;
  m_RMSChange = NumericTraits< double >::max();
  m_SumOfSquaredChange = 0.0;

  m_MovingImageGradientCalculator = MovingImageGradientCalculatorType::New();
  m_UseMovingImageGradient = false;
}

/**
 * Standard "PrintSelf" method.
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
DemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "MovingImageIterpolator: ";
  os << m_MovingImageInterpolator.GetPointer() << std::endl;
  os << indent << "FixedImageGradientCalculator: ";
  os << m_FixedImageGradientCalculator.GetPointer() << std::endl;
  os << indent << "DenominatorThreshold: ";
  os << m_DenominatorThreshold << std::endl;
  os << indent << "IntensityDifferenceThreshold: ";
  os << m_IntensityDifferenceThreshold << std::endl;

  os << indent << "UseMovingImageGradient: ";
  os << m_UseMovingImageGradient << std::endl;

  os << indent << "Metric: ";
  os << m_Metric << std::endl;
  os << indent << "SumOfSquaredDifference: ";
  os << m_SumOfSquaredDifference << std::endl;
  os << indent << "NumberOfPixelsProcessed: ";
  os << m_NumberOfPixelsProcessed << std::endl;
  os << indent << "RMSChange: ";
  os << m_RMSChange << std::endl;
  os << indent << "SumOfSquaredChange: ";
  os << m_SumOfSquaredChange << std::endl;
}

/**
 *
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
DemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::SetIntensityDifferenceThreshold(double threshold)
{
  m_IntensityDifferenceThreshold = threshold;
}

/**
 *
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
double
DemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::GetIntensityDifferenceThreshold() const
{
  return m_IntensityDifferenceThreshold;
}

/**
 * Set the function state values before each iteration
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
DemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::InitializeIteration()
{
  if ( !this->GetMovingImage() || !this->GetFixedImage() || !m_MovingImageInterpolator )
    {
    itkExceptionMacro(<< "MovingImage, FixedImage and/or Interpolator not set");
    }

  // cache fixed image information
  SpacingType fixedImageSpacing    = this->GetFixedImage()->GetSpacing();
  m_ZeroUpdateReturn.Fill(0.0);

  // compute the normalizer
  m_Normalizer      = 0.0;
  for ( unsigned int k = 0; k < ImageDimension; k++ )
    {
    m_Normalizer += fixedImageSpacing[k] * fixedImageSpacing[k];
    }
  m_Normalizer /= static_cast< double >( ImageDimension );

  // setup gradient calculator
  m_FixedImageGradientCalculator->SetInputImage( this->GetFixedImage() );
  m_MovingImageGradientCalculator->SetInputImage( this->GetMovingImage() );

  // setup moving image interpolator
  m_MovingImageInterpolator->SetInputImage( this->GetMovingImage() );

  // initialize metric computation variables
  m_SumOfSquaredDifference  = 0.0;
  m_NumberOfPixelsProcessed = 0L;
  m_SumOfSquaredChange      = 0.0;
}

/**
 * Compute update at a specify neighbourhood
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
typename DemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::PixelType
DemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::ComputeUpdate( const NeighborhoodType & it, void *gd,
                 const FloatOffsetType & itkNotUsed(offset) )
{
  // Get fixed image related information
  // Note: no need to check the index is within
  // fixed image buffer. This is done by the external filter.
  const IndexType index = it.GetIndex();
  const double    fixedValue = (double)this->GetFixedImage()->GetPixel(index);

  // Get moving image related information
  PointType mappedPoint;

  this->GetFixedImage()->TransformIndexToPhysicalPoint(index, mappedPoint);
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    mappedPoint[j] += it.GetCenterPixel()[j];
    }

  double movingValue;
  if ( m_MovingImageInterpolator->IsInsideBuffer(mappedPoint) )
    {
    movingValue = m_MovingImageInterpolator->Evaluate(mappedPoint);
    }
  else
    {
    return m_ZeroUpdateReturn;
    }

  CovariantVectorType gradient;
  // Compute the gradient of either fixed or moving image
  if ( !m_UseMovingImageGradient )
    {
    gradient = m_FixedImageGradientCalculator->EvaluateAtIndex(index);
    }
  else
    {
    gradient = m_MovingImageGradientCalculator->Evaluate(mappedPoint);
    }

  double gradientSquaredMagnitude = 0;
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    gradientSquaredMagnitude += itk::Math::sqr(gradient[j]);
    }

  /**
   * Compute Update.
   * In the original equation the denominator is defined as (g-f)^2 + grad_mag^2.
   * However there is a mismatch in units between the two terms.
   * The units for the second term is intensity^2/mm^2 while the
   * units for the first term is intensity^2. This mismatch is particularly
   * problematic when the fixed image does not have unit spacing.
   * In this implementation, we normalize the first term by a factor K,
   * such that denominator = (g-f)^2/K + grad_mag^2
   * where K = mean square spacing to compensate for the mismatch in units.
   */
  const double speedValue = fixedValue - movingValue;
  const double sqr_speedValue = itk::Math::sqr(speedValue);

  // update the metric
  GlobalDataStruct *globalData = (GlobalDataStruct *)gd;
  if ( globalData )
    {
    globalData->m_SumOfSquaredDifference += sqr_speedValue;
    globalData->m_NumberOfPixelsProcessed += 1;
    }

  const double denominator = sqr_speedValue / m_Normalizer
                             + gradientSquaredMagnitude;

  if ( itk::Math::abs(speedValue) < m_IntensityDifferenceThreshold
       || denominator < m_DenominatorThreshold )
    {
    return m_ZeroUpdateReturn;
    }

  PixelType update;
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    update[j] = speedValue * gradient[j] / denominator;
    if ( globalData )
      {
      globalData->m_SumOfSquaredChange += itk::Math::sqr(update[j]);
      }
    }
  return update;
}

/**
 * Update the metric and release the per-thread-global data.
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
DemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::ReleaseGlobalDataPointer(void *gd) const
{
  GlobalDataStruct *globalData = (GlobalDataStruct *)gd;

  m_MetricCalculationLock.Lock();
  m_SumOfSquaredDifference += globalData->m_SumOfSquaredDifference;
  m_NumberOfPixelsProcessed += globalData->m_NumberOfPixelsProcessed;
  m_SumOfSquaredChange += globalData->m_SumOfSquaredChange;
  if ( m_NumberOfPixelsProcessed )
    {
    m_Metric = m_SumOfSquaredDifference
               / static_cast< double >( m_NumberOfPixelsProcessed );
    m_RMSChange = std::sqrt( m_SumOfSquaredChange
                            / static_cast< double >( m_NumberOfPixelsProcessed ) );
    }
  m_MetricCalculationLock.Unlock();

  delete globalData;
}
} // end namespace itk

#endif
