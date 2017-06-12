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
#ifndef itkSymmetricForcesDemonsRegistrationFunction_hxx
#define itkSymmetricForcesDemonsRegistrationFunction_hxx

#include "itkSymmetricForcesDemonsRegistrationFunction.h"
#include "itkMacro.h"
#include "itkMath.h"

namespace itk
{
/**
 * Default constructor
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
SymmetricForcesDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::SymmetricForcesDemonsRegistrationFunction()
{
  RadiusType r;

  r.Fill(0);
  this->SetRadius(r);

  m_TimeStep = 1.0;
  m_DenominatorThreshold = 1e-9;
  m_IntensityDifferenceThreshold = 0.001;
  this->SetMovingImage(ITK_NULLPTR);
  this->SetFixedImage(ITK_NULLPTR);
  m_FixedImageSpacing.Fill(1.0);
  m_Normalizer = 0.0;
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
}

/*
 * Standard "PrintSelf" method.
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
SymmetricForcesDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
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
SymmetricForcesDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::SetIntensityDifferenceThreshold(double threshold)
{
  m_IntensityDifferenceThreshold = threshold;
}

/**
 *
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
double
SymmetricForcesDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::GetIntensityDifferenceThreshold() const
{
  return m_IntensityDifferenceThreshold;
}

/**
 * Set the function state values before each iteration
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
SymmetricForcesDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::InitializeIteration()
{
  if ( !this->GetMovingImage() || !this->GetFixedImage() || !m_MovingImageInterpolator )
    {
    itkExceptionMacro(<< "MovingImage, FixedImage and/or Interpolator not set");
    }

  // cache fixed image information
  m_FixedImageSpacing    = this->GetFixedImage()->GetSpacing();

  // compute the normalizer
  m_Normalizer      = 0.0;
  for ( unsigned int k = 0; k < ImageDimension; k++ )
    {
    m_Normalizer += m_FixedImageSpacing[k] * m_FixedImageSpacing[k];
    }
  m_Normalizer /= static_cast< double >( ImageDimension );

  // setup gradient calculator
  m_FixedImageGradientCalculator->SetInputImage( this->GetFixedImage() );

  // setup moving image interpolator
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
typename SymmetricForcesDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::PixelType
SymmetricForcesDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
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
  IndexType                         tmpIndex = index;
  PointType                         mappedNeighPoint;
  CovariantVectorType               movingGradient;
  const DisplacementFieldType *const field = this->GetDisplacementField();

  typedef typename DisplacementFieldType::PixelType DisplacementPixelType;
  PointType mappedCenterPoint;
  this->GetFixedImage()->TransformIndexToPhysicalPoint(index, mappedCenterPoint);
  for ( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    mappedCenterPoint[dim] += it.GetCenterPixel()[dim];
    // bounds checking
    if ( index[dim] < ( FirstIndex[dim] + 1 ) || index[dim] > ( LastIndex[dim] - 2 ) )
      {
      movingGradient[dim] = 0.0;
      }
    else
      {
      tmpIndex[dim] += 1;
      DisplacementPixelType displacement = field->GetPixel(tmpIndex);
      this->GetFixedImage()->TransformIndexToPhysicalPoint(tmpIndex, mappedNeighPoint);
      for ( unsigned int j = 0; j < ImageDimension; j++ )
        {
        mappedNeighPoint[j] += displacement[j];
        }
      if ( m_MovingImageInterpolator->IsInsideBuffer(mappedNeighPoint) )
        {
        movingGradient[dim] = m_MovingImageInterpolator->Evaluate(mappedNeighPoint);
        }
      else
        {
        movingGradient[dim] = 0.0;
        }

      tmpIndex[dim] -= 2;
      displacement = field->GetPixel(tmpIndex);
      this->GetFixedImage()->TransformIndexToPhysicalPoint(tmpIndex, mappedNeighPoint);
      for ( unsigned int j = 0; j < ImageDimension; j++ )
        {
        mappedNeighPoint[j] += displacement[j];
        }
      if ( m_MovingImageInterpolator->IsInsideBuffer(mappedNeighPoint) )
        {
        movingGradient[dim] -= m_MovingImageInterpolator->Evaluate(mappedNeighPoint);
        }

      movingGradient[dim] *= 0.5 / m_FixedImageSpacing[dim];
      tmpIndex[dim] += 1;
      }
    }

  double movingValue = 0.0;
  if ( m_MovingImageInterpolator->IsInsideBuffer(mappedCenterPoint) )
    {
    movingValue = m_MovingImageInterpolator->Evaluate(mappedCenterPoint);
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
      update[j] = 2 * speedValue * ( fixedGradient[j] + movingGradient[j] ) / denominator;
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
  if ( globalData )
    {
    // do not consider voxel on the border (2 voxels) as there are often
    // artefacts
    // which falsify the metric
    if ( !IsOutsideRegion )
      {
      double newMovingValue = 0.0;
      if ( m_MovingImageInterpolator->IsInsideBuffer(newMappedCenterPoint) )
        {
        newMovingValue = m_MovingImageInterpolator->Evaluate(newMappedCenterPoint);
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
SymmetricForcesDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
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
