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
#ifndef itkESMDemonsRegistrationFunction_hxx
#define itkESMDemonsRegistrationFunction_hxx

#include "itkESMDemonsRegistrationFunction.h"
#include "itkExceptionObject.h"
#include "itkMath.h"

namespace itk
{
/**
 * Default constructor
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
ESMDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::ESMDemonsRegistrationFunction()
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
  m_MaximumUpdateStepLength = 0.5;

  this->SetMovingImage(ITK_NULLPTR);
  this->SetFixedImage(ITK_NULLPTR);
  m_FixedImageSpacing.Fill(1.0);
  m_FixedImageOrigin.Fill(0.0);
  m_FixedImageDirection.SetIdentity();
  m_Normalizer = 0.0;
  m_FixedImageGradientCalculator = GradientCalculatorType::New();
  // Gradient orientation will be taken care of explicitely
  m_FixedImageGradientCalculator->UseImageDirectionOff();
  m_MappedMovingImageGradientCalculator = MovingImageGradientCalculatorType::New();
  // Gradient orientation will be taken care of explicitely
  m_MappedMovingImageGradientCalculator->UseImageDirectionOff();

  this->m_UseGradientType = Symmetric;

  typename DefaultInterpolatorType::Pointer interp =
    DefaultInterpolatorType::New();

  m_MovingImageInterpolator = itkDynamicCastInDebugMode< InterpolatorType * >
    ( interp.GetPointer() );

  m_MovingImageWarper = WarperType::New();
  m_MovingImageWarper->SetInterpolator(m_MovingImageInterpolator);
  m_MovingImageWarper->SetEdgePaddingValue( NumericTraits< MovingPixelType >::max() );

  m_MovingImageWarperOutput = ITK_NULLPTR;

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
ESMDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "UseGradientType: ";
  os << m_UseGradientType << std::endl;
  os << indent << "MaximumUpdateStepLength: ";
  os << m_MaximumUpdateStepLength << std::endl;

  os << indent << "MovingImageIterpolator: ";
  os << m_MovingImageInterpolator.GetPointer() << std::endl;
  os << indent << "FixedImageGradientCalculator: ";
  os << m_FixedImageGradientCalculator.GetPointer() << std::endl;
  os << indent << "MappedMovingImageGradientCalculator: ";
  os << m_MappedMovingImageGradientCalculator.GetPointer() << std::endl;
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
ESMDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::SetIntensityDifferenceThreshold(double threshold)
{
  m_IntensityDifferenceThreshold = threshold;
}

/**
 *
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
double
ESMDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::GetIntensityDifferenceThreshold() const
{
  return m_IntensityDifferenceThreshold;
}

/**
 * Set the function state values before each iteration
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
ESMDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::InitializeIteration()
{
  if ( !this->GetMovingImage() || !this->GetFixedImage()
       || !m_MovingImageInterpolator )
    {
    itkExceptionMacro(
      << "MovingImage, FixedImage and/or Interpolator not set");
    }

  // cache fixed image information
  m_FixedImageOrigin  = this->GetFixedImage()->GetOrigin();
  m_FixedImageSpacing = this->GetFixedImage()->GetSpacing();
  m_FixedImageDirection = this->GetFixedImage()->GetDirection();

  // compute the normalizer
  if ( m_MaximumUpdateStepLength > 0.0 )
    {
    m_Normalizer = 0.0;
    for ( unsigned int k = 0; k < ImageDimension; k++ )
      {
      m_Normalizer += m_FixedImageSpacing[k] * m_FixedImageSpacing[k];
      }
    m_Normalizer *= m_MaximumUpdateStepLength * m_MaximumUpdateStepLength
                    / static_cast< double >( ImageDimension );
    }
  else
    {
    // set it to minus one to denote a special case
    // ( unrestricted update length )
    m_Normalizer = -1.0;
    }

  // setup gradient calculator
  m_FixedImageGradientCalculator->SetInputImage( this->GetFixedImage() );
  m_MappedMovingImageGradientCalculator->SetInputImage( this->GetMovingImage() );

  // Compute warped moving image
  m_MovingImageWarper->SetOutputOrigin(this->m_FixedImageOrigin);
  m_MovingImageWarper->SetOutputSpacing(this->m_FixedImageSpacing);
  m_MovingImageWarper->SetOutputDirection(this->m_FixedImageDirection);
  m_MovingImageWarper->SetInput( this->GetMovingImage() );
  m_MovingImageWarper->SetDisplacementField( this->GetDisplacementField() );
  m_MovingImageWarper->GetOutput()->SetRequestedRegion( this->GetDisplacementField()->GetRequestedRegion() );
  m_MovingImageWarper->Update();
  this->m_MovingImageWarperOutput =
    this->m_MovingImageWarper->GetOutput();
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
typename ESMDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::PixelType
ESMDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::ComputeUpdate( const NeighborhoodType & it, void *gd,
                 const FloatOffsetType & itkNotUsed(offset) )
{
  GlobalDataStruct *globalData = (GlobalDataStruct *)gd;
  PixelType         update;
  IndexType         FirstIndex = this->GetFixedImage()->GetLargestPossibleRegion().GetIndex();
  IndexType         LastIndex = this->GetFixedImage()->GetLargestPossibleRegion().GetIndex()
                                + this->GetFixedImage()->GetLargestPossibleRegion().GetSize();

  const IndexType index = it.GetIndex();

  // Get fixed image related information
  // Note: no need to check if the index is within
  // fixed image buffer. This is done by the external filter.
  const double fixedValue = static_cast< double >(
    this->GetFixedImage()->GetPixel(index) );

  // Get moving image related information
  // check if the point was mapped outside of the moving image using
  // the "special value" NumericTraits<MovingPixelType>::max()
  MovingPixelType movingPixValue =
    m_MovingImageWarperOutput->GetPixel(index);

  if ( movingPixValue == NumericTraits< MovingPixelType >::max() )
    {
    update.Fill(0.0);
    return update;
    }

  const double movingValue = static_cast< double >( movingPixValue );

  // We compute the gradient more or less by hand.
  // We first start by ignoring the image orientation and introduce it
  // afterwards
  CovariantVectorType usedOrientFreeGradientTimes2;

  if ( ( this->m_UseGradientType == Symmetric )
       || ( this->m_UseGradientType == WarpedMoving ) )
    {
    // we don't use a CentralDifferenceImageFunction here to be able to
    // check for NumericTraits<MovingPixelType>::max()
    CovariantVectorType warpedMovingGradient;
    IndexType           tmpIndex = index;
    for ( unsigned int dim = 0; dim < ImageDimension; dim++ )
      {
      // bounds checking
      if ( FirstIndex[dim] == LastIndex[dim]
           || index[dim] < FirstIndex[dim]
           || index[dim] >= LastIndex[dim] )
        {
        warpedMovingGradient[dim] = 0.0;
        continue;
        }
      else if ( index[dim] == FirstIndex[dim] )
        {
        // compute derivative
        tmpIndex[dim] += 1;
        movingPixValue = m_MovingImageWarperOutput->GetPixel(tmpIndex);
        if ( movingPixValue == NumericTraits< MovingPixelType >::max() )
          {
          // weird crunched border case
          warpedMovingGradient[dim] = 0.0;
          }
        else
          {
          // forward difference
          warpedMovingGradient[dim] = static_cast< double >( movingPixValue ) - movingValue;
          warpedMovingGradient[dim] /= m_FixedImageSpacing[dim];
          }
        tmpIndex[dim] -= 1;
        continue;
        }
      else if ( index[dim] == ( LastIndex[dim] - 1 ) )
        {
        // compute derivative
        tmpIndex[dim] -= 1;
        movingPixValue = m_MovingImageWarperOutput->GetPixel(tmpIndex);
        if ( movingPixValue == NumericTraits< MovingPixelType >::max() )
          {
          // weird crunched border case
          warpedMovingGradient[dim] = 0.0;
          }
        else
          {
          // backward difference
          warpedMovingGradient[dim] = movingValue - static_cast< double >( movingPixValue );
          warpedMovingGradient[dim] /= m_FixedImageSpacing[dim];
          }
        tmpIndex[dim] += 1;
        continue;
        }

      // compute derivative
      tmpIndex[dim] += 1;
      movingPixValue = m_MovingImageWarperOutput->GetPixel(tmpIndex);
      if ( movingPixValue == NumericTraits
           < MovingPixelType >::max() )
        {
        // backward difference
        warpedMovingGradient[dim] = movingValue;

        tmpIndex[dim] -= 2;
        movingPixValue = m_MovingImageWarperOutput->GetPixel(tmpIndex);
        if ( movingPixValue == NumericTraits< MovingPixelType >::max() )
          {
          // weird crunched border case
          warpedMovingGradient[dim] = 0.0;
          }
        else
          {
          // backward difference
          warpedMovingGradient[dim] -= static_cast< double >(
            m_MovingImageWarperOutput->GetPixel(tmpIndex) );

          warpedMovingGradient[dim] /= m_FixedImageSpacing[dim];
          }
        }
      else
        {
        warpedMovingGradient[dim] = static_cast< double >( movingPixValue );

        tmpIndex[dim] -= 2;
        movingPixValue = m_MovingImageWarperOutput->GetPixel(tmpIndex);
        if ( movingPixValue == NumericTraits< MovingPixelType >::max() )
          {
          // forward difference
          warpedMovingGradient[dim] -= movingValue;
          warpedMovingGradient[dim] /= m_FixedImageSpacing[dim];
          }
        else
          {
          // normal case, central difference
          warpedMovingGradient[dim] -= static_cast< double >( movingPixValue );
          warpedMovingGradient[dim] *= 0.5 / m_FixedImageSpacing[dim];
          }
        }
      tmpIndex[dim] += 1;
      }

    if ( this->m_UseGradientType == Symmetric )
      {
      // Compute orientation-free gradient with calculator
      const CovariantVectorType fixedGradient =
        m_FixedImageGradientCalculator->EvaluateAtIndex(index);

      usedOrientFreeGradientTimes2 = fixedGradient + warpedMovingGradient;
      }
    else if ( this->m_UseGradientType == WarpedMoving )
      {
      usedOrientFreeGradientTimes2 = warpedMovingGradient + warpedMovingGradient;
      }
    else
      {
      itkExceptionMacro(<< "Unknown gradient type");
      }
    }
  else if ( this->m_UseGradientType == Fixed )
    {
    // Compute orientation-free gradient with calculator
    const CovariantVectorType fixedGradient =
      m_FixedImageGradientCalculator->EvaluateAtIndex(index);

    usedOrientFreeGradientTimes2 = fixedGradient + fixedGradient;
    }
  else if ( this->m_UseGradientType == MappedMoving )
    {
    PointType mappedPoint;
    this->GetFixedImage()->TransformIndexToPhysicalPoint(index, mappedPoint);
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      mappedPoint[j] += it.GetCenterPixel()[j];
      }

    const CovariantVectorType mappedMovingGradient =
      m_MappedMovingImageGradientCalculator->Evaluate(mappedPoint);

    usedOrientFreeGradientTimes2 = mappedMovingGradient + mappedMovingGradient;
    }
  else
    {
    itkExceptionMacro(<< "Unknown gradient type");
    }

  CovariantVectorType usedGradientTimes2;
  this->GetFixedImage()->TransformLocalVectorToPhysicalVector(
    usedOrientFreeGradientTimes2, usedGradientTimes2);

  /**
   * Compute Update.
   * We avoid the mismatch in units between the two terms.
   * and avoid large step using a normalization term.
   */

  const double usedGradientTimes2SquaredMagnitude =
    usedGradientTimes2.GetSquaredNorm();

  const double speedValue = fixedValue - movingValue;
  if ( itk::Math::abs(speedValue) < m_IntensityDifferenceThreshold )
    {
    update.Fill(0.0);
    }
  else
    {
    double denom;
    if (  m_Normalizer > 0.0 )
      {
      // "ITK-Thirion" normalization
      denom =  usedGradientTimes2SquaredMagnitude + ( itk::Math::sqr(speedValue) / m_Normalizer );
      }
    else
      {
      // least square solution of the system
      denom =  usedGradientTimes2SquaredMagnitude;
      }

    if ( denom < m_DenominatorThreshold )
      {
      update.Fill(0.0);
      }
    else
      {
      const double factor = 2.0 * speedValue / denom;

      for ( unsigned int j = 0; j < ImageDimension; j++ )
        {
        update[j] = factor * usedGradientTimes2[j];
        }
      }
    }

  // WARNING!! We compute the global data without taking into account the
  // current update step.
  // There are several reasons for that: If an exponential, a smoothing or any
  // other operation
  // is applied on the update field, we cannot compute the newMappedCenterPoint
  // here; and even
  // if we could, this would be an often unnecessary time-consuming task.
  if ( globalData )
    {
    globalData->m_SumOfSquaredDifference += itk::Math::sqr(speedValue);
    globalData->m_NumberOfPixelsProcessed += 1;
    globalData->m_SumOfSquaredChange += update.GetSquaredNorm();
    }

  return update;
}

/**
 * Update the metric and release the per-thread-global data.
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
ESMDemonsRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
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
