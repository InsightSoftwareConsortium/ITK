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
#ifndef itkFastSymmetricForcesDemonsRegistrationFunction_hxx
#define itkFastSymmetricForcesDemonsRegistrationFunction_hxx

#include "itkFastSymmetricForcesDemonsRegistrationFunction.h"
#include "itkMacro.h"
#include "itkMath.h"

namespace itk
{
/**
 * Default constructor
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
FastSymmetricForcesDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::FastSymmetricForcesDemonsRegistrationFunction()
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
  m_Normalizer = 0.0;
  m_FixedImageGradientCalculator = GradientCalculatorType::New();

  typename DefaultInterpolatorType::Pointer interp =
    DefaultInterpolatorType::New();

  m_MovingImageInterpolator = static_cast< InterpolatorType * >(
    interp.GetPointer() );

  m_WarpedMovingImageGradientCalculator = MovingGradientCalculatorType::New();
  m_MovingImageWarper = WarperType::New();
  m_MovingImageWarper->SetInterpolator(m_MovingImageInterpolator);

  m_Metric = NumericTraits< double >::max();
  m_SumOfSquaredDifference = 0.0;
  m_NumberOfPixelsProcessed = 0L;
  m_RMSChange = NumericTraits< double >::max();
  m_SumOfSquaredChange = 0.0;
}

/**
 * Standard "PrintSelf" method.
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
FastSymmetricForcesDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
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
FastSymmetricForcesDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::SetIntensityDifferenceThreshold(double threshold)
{
  m_IntensityDifferenceThreshold = threshold;
}

/**
 *
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
double
FastSymmetricForcesDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::GetIntensityDifferenceThreshold() const
{
  return m_IntensityDifferenceThreshold;
}

/**
 * Set the function state values before each iteration
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
FastSymmetricForcesDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::InitializeIteration()
{
  if ( !this->GetMovingImage() || !this->GetFixedImage() || !m_MovingImageInterpolator )
    {
    itkExceptionMacro(<< "MovingImage, FixedImage and/or Interpolator not set");
    }

  // cache fixed image information
  const PixelType fixedImageSpacing = this->GetFixedImage()->GetSpacing();

  // compute the normalizer
  m_Normalizer      = 0.0;
  for ( unsigned int k = 0; k < ImageDimension; k++ )
    {
    m_Normalizer += fixedImageSpacing[k] * fixedImageSpacing[k];
    }
  m_Normalizer /= static_cast< double >( ImageDimension );

  // setup gradient calculator
  m_FixedImageGradientCalculator->SetInputImage( this->GetFixedImage() );

  m_MovingImageWarper->SetOutputOrigin( this->GetFixedImage()->GetOrigin() );
  m_MovingImageWarper->SetOutputSpacing( this->GetFixedImage()->GetSpacing() );
  m_MovingImageWarper->SetOutputDirection( this->GetFixedImage()->GetDirection() );
  m_MovingImageWarper->SetInput( this->GetMovingImage() );
  m_MovingImageWarper->SetDisplacementField( this->GetDisplacementField() );
  m_MovingImageWarper->Update();
  m_WarpedMovingImageGradientCalculator->SetInputImage( this->m_MovingImageWarper->GetOutput() );

  // setup moving image interpolator for further access
  m_MovingImageInterpolator->SetInputImage( this->GetMovingImage() );

  // initialize metric computation variables
  m_SumOfSquaredDifference  = 0.0;
  m_NumberOfPixelsProcessed = 0L;
  m_SumOfSquaredChange      = 0.0;
}

/**
 * Compute update at a non boundary neighbourhood
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
typename FastSymmetricForcesDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::PixelType
FastSymmetricForcesDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::ComputeUpdate( const NeighborhoodType & it, void *gd,
                 const FloatOffsetType & itkNotUsed(offset) )
{
  GlobalDataStruct *globalData = (GlobalDataStruct *)gd;
  const IndexType   FirstIndex = this->GetFixedImage()->GetLargestPossibleRegion().GetIndex();
  const IndexType   LastIndex = this->GetFixedImage()->GetLargestPossibleRegion().GetIndex()
                                + this->GetFixedImage()->GetLargestPossibleRegion().GetSize();

  const IndexType index = it.GetIndex();

  // Get fixed image related information
  // Note: no need to check the index is within
  // fixed image buffer. This is done by the external filter.
  const double              fixedValue = (double)this->GetFixedImage()->GetPixel(index);
  const CovariantVectorType fixedGradient = m_FixedImageGradientCalculator->EvaluateAtIndex(index);

  // Get moving image related information
  // use warped moving image to get moving value and gradient fast(er).
  const double              movingValue = (double)m_MovingImageWarper->GetOutput()->GetPixel(index);
  const CovariantVectorType movingGradient = m_WarpedMovingImageGradientCalculator->EvaluateAtIndex(index);

  // unfortunately (since it's a little redundant) we still need the mapped
  // center point coordinates
  PointType mappedCenterPoint;

  this->GetFixedImage()->TransformIndexToPhysicalPoint(index, mappedCenterPoint);
  for ( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    mappedCenterPoint[dim] += it.GetCenterPixel()[dim];
    }

  /**
   * Compute Update.
   * In the original equation the denominator is defined as
   *
   *         (g-f)^2 + (moving_grad+fixed_grad)_mag^2
   *
   * However there is a mismatch in units between the two terms.
   * The units for the second term is intensity^2/mm^2 while the
   * units for the first term is intensity^2. This mismatch is particularly
   * problematic when the fixed image does not have unit spacing.
   * In this implementation, we normalize the first term by a factor K,
   * such that denominator = (g-f)^2/K + grad_mag^2
   * where K = mean square spacing to compensate for the mismatch in units.
   */
  double fixedPlusMovingGradientSquaredMagnitude = 0;
  for ( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    fixedPlusMovingGradientSquaredMagnitude += itk::Math::sqr(fixedGradient[dim] + movingGradient[dim]);
    }

  const double speedValue = fixedValue - movingValue;
  const double denominator = itk::Math::sqr(speedValue) / m_Normalizer + fixedPlusMovingGradientSquaredMagnitude;

  PixelType update;
  if ( itk::Math::abs(speedValue) < m_IntensityDifferenceThreshold || denominator < m_DenominatorThreshold )
    {
    update.Fill(0.0);
    }
  else
    {
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      update[j] = 2 * speedValue * ( movingGradient[j] + fixedGradient[j] ) / denominator;
      }
    }

  // update the squared change value
  PointType newMappedCenterPoint;
  bool      IsOutsideRegion = 0;
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    if ( globalData )
      {
      globalData->m_SumOfSquaredChange += itk::Math::sqr(update[j]);
      newMappedCenterPoint[j] = mappedCenterPoint[j] + update[j];
      if ( index[j] < ( FirstIndex[j] + 2 ) || index[j] > ( LastIndex[j] - 3 ) )
        {
        IsOutsideRegion = 1;
        }
      }
    }

  // update the metric with the latest deformable field
  double newMovingValue = 0;
  if ( globalData )
    {
    // do not consider voxel on the border (2 voxels) as there are often
    // artefacts
    // which falsify the metric
    if ( !IsOutsideRegion )
      {
      if ( m_MovingImageInterpolator->IsInsideBuffer(newMappedCenterPoint) )
        {
        newMovingValue = m_MovingImageInterpolator->Evaluate(newMappedCenterPoint);
        }
      else
        {
        newMovingValue = 0;
        }
      globalData->m_SumOfSquaredDifference += itk::Math::sqr(fixedValue - newMovingValue);
      globalData->m_NumberOfPixelsProcessed += 1;
      }
    }
  return update;
}

/**
 * Update the metric and release the per-thread-global data.
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
FastSymmetricForcesDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
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
