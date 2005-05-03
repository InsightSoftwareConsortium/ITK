/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastSymmetricForcesDemonsRegistrationFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFastSymmetricForcesDemonsRegistrationFunction_txx_
#define _itkFastSymmetricForcesDemonsRegistrationFunction_txx_

#include "itkFastSymmetricForcesDemonsRegistrationFunction.h"
#include "itkExceptionObject.h"
#include "vnl/vnl_math.h"

namespace itk {

/*
 * Default constructor
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
FastSymmetricForcesDemonsRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::FastSymmetricForcesDemonsRegistrationFunction()
{

  RadiusType r;
  unsigned int j;
  for( j = 0; j < ImageDimension; j++ )
    {
    r[j] = 0;
    }
  this->SetRadius(r);

  m_TimeStep = 1.0;
  m_DenominatorThreshold = 1e-9;
  m_IntensityDifferenceThreshold = 0.001;
  this->SetMovingImage(NULL);
  this->SetFixedImage(NULL);
  m_FixedImageSpacing.Fill( 1.0 );
  m_FixedImageOrigin.Fill( 0.0 );
  m_Normalizer = 0.0;
  m_FixedImageGradientCalculator = GradientCalculatorType::New();

  typename DefaultInterpolatorType::Pointer interp =
    DefaultInterpolatorType::New();

  m_MovingImageInterpolator = static_cast<InterpolatorType*>(
    interp.GetPointer() );

  m_WarpedMovingImageGradientCalculator = MovingGradientCalculatorType::New();
  m_MovingImageWarper = WarperType::New();
  m_MovingImageWarper->SetInterpolator( m_MovingImageInterpolator );

  m_Metric = NumericTraits<double>::max();
  m_SumOfSquaredDifference = 0.0;
  m_NumberOfPixelsProcessed = 0L;
  m_RMSChange = NumericTraits<double>::max();
  m_SumOfSquaredChange = 0.0;

}


/*
 * Standard "PrintSelf" method.
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
FastSymmetricForcesDemonsRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::PrintSelf(std::ostream& os, Indent indent) const
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
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
FastSymmetricForcesDemonsRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::SetIntensityDifferenceThreshold(double threshold)
{
  m_IntensityDifferenceThreshold = threshold;
}

/**
 *
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
double
FastSymmetricForcesDemonsRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::GetIntensityDifferenceThreshold() const
{
  return m_IntensityDifferenceThreshold;
}

/*
 * Set the function state values before each iteration
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
FastSymmetricForcesDemonsRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::InitializeIteration()
{
  if( !this->GetMovingImage() || !this->GetFixedImage() || !m_MovingImageInterpolator )
    {
    itkExceptionMacro( << "MovingImage, FixedImage and/or Interpolator not set" );
    }

  // cache fixed image information
  m_FixedImageSpacing    = this->GetFixedImage()->GetSpacing();
  m_FixedImageOrigin     = this->GetFixedImage()->GetOrigin();

  // compute the normalizer
  m_Normalizer      = 0.0;
  for( unsigned int k = 0; k < ImageDimension; k++ )
    {
    m_Normalizer += m_FixedImageSpacing[k] * m_FixedImageSpacing[k];
    }
  m_Normalizer /= static_cast<double>( ImageDimension );


  // setup gradient calculator
  m_FixedImageGradientCalculator->SetInputImage( this->GetFixedImage() );

  m_MovingImageWarper->SetInput( this->GetMovingImage() );
  m_MovingImageWarper->SetDeformationField( this->GetDeformationField() );
  m_MovingImageWarper->Update();
  m_WarpedMovingImageGradientCalculator->SetInputImage( this->m_MovingImageWarper->GetOutput() );

  // setup moving image interpolator for further access
  m_MovingImageInterpolator->SetInputImage( this->GetMovingImage() );
  
  // initialize metric computation variables
  m_SumOfSquaredDifference  = 0.0;
  m_NumberOfPixelsProcessed = 0L;
  m_SumOfSquaredChange      = 0.0;

}


/*
 * Compute update at a non boundary neighbourhood
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
typename FastSymmetricForcesDemonsRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::PixelType
FastSymmetricForcesDemonsRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::ComputeUpdate(const NeighborhoodType &it, void * gd,
                const FloatOffsetType& itkNotUsed(offset))
{

  GlobalDataStruct *globalData = (GlobalDataStruct *)gd;
  PixelType update;
  IndexType FirstIndex = this->GetFixedImage()->GetLargestPossibleRegion().GetIndex();
  IndexType LastIndex = this->GetFixedImage()->GetLargestPossibleRegion().GetIndex() + 
                        this->GetFixedImage()->GetLargestPossibleRegion().GetSize();

  IndexType index = it.GetIndex();



  // Get fixed image related information
  double fixedValue;
  CovariantVectorType fixedGradient;

  // Note: no need to check the index is within
  // fixed image buffer. This is done by the external filter.
  fixedValue = (double) this->GetFixedImage()->GetPixel( index );
  fixedGradient = m_FixedImageGradientCalculator->EvaluateAtIndex( index );

  // Get moving image related information
  double movingValue;
  CovariantVectorType movingGradient;

  // use warped moving image to get moving value and gradient fast(er).
  movingValue = (double) m_MovingImageWarper->GetOutput()->GetPixel( index );
  movingGradient = m_WarpedMovingImageGradientCalculator->EvaluateAtIndex( index );

  // unfortunately (since it's a little redundant) we still need the mapped center point coordinates
  PointType mappedCenterPoint;
  for( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    mappedCenterPoint[dim] = double( index[dim] ) * m_FixedImageSpacing[dim] + m_FixedImageOrigin[dim];
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
   * In this implemenation, we normalize the first term by a factor K,
   * such that denominator = (g-f)^2/K + grad_mag^2
   * where K = mean square spacing to compensate for the mismatch in units.
   */

  double fixedPlusMovingGradientSquaredMagnitude = 0;
  for( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    fixedPlusMovingGradientSquaredMagnitude += vnl_math_sqr( fixedGradient[dim] + movingGradient[dim] );
    }

  double speedValue = fixedValue - movingValue;
  double denominator = vnl_math_sqr( speedValue ) / m_Normalizer + fixedPlusMovingGradientSquaredMagnitude;

  if ( vnl_math_abs(speedValue) < m_IntensityDifferenceThreshold || denominator < m_DenominatorThreshold )
    {
    for( unsigned int j = 0; j < ImageDimension; j++ )
      {
      update[j] = 0.0;
      }
    }

  else
    {
    for( unsigned int j = 0; j < ImageDimension; j++ )
      {
      update[j] = 2 * speedValue * (movingGradient[j] + fixedGradient[j]) / denominator;
      }
    }

  // update the squared change value
  PointType newMappedCenterPoint;
  double newMovingValue;
  bool IsOutsideRegion = 0;
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    if ( globalData )
      {
      globalData->m_SumOfSquaredChange += vnl_math_sqr( update[j] );

      newMappedCenterPoint[j] = mappedCenterPoint[j] + update[j];
      if ( index[j] < (FirstIndex[j] + 2) || index[j] > (LastIndex[j] - 3) )
        {
        IsOutsideRegion = 1;
        }
      }
    }

  // update the metric with the latest deformable field
  if ( globalData )
    {
    // do not consider voxel on the border (2 voxels) as there are often artefacts
    // which falsify the metric
    if( !IsOutsideRegion )
      {
      if( m_MovingImageInterpolator->IsInsideBuffer( newMappedCenterPoint ) )
        {
        newMovingValue = m_MovingImageInterpolator->Evaluate( newMappedCenterPoint );
        }
      else
        {
        newMovingValue = 0;
        }
        globalData->m_SumOfSquaredDifference += vnl_math_sqr( fixedValue - newMovingValue );
        globalData->m_NumberOfPixelsProcessed += 1;
      }
    }

  return update;

}


/*
 * Update the metric and release the per-thread-global data.
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
FastSymmetricForcesDemonsRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::ReleaseGlobalDataPointer( void *gd ) const
{
  GlobalDataStruct * globalData = (GlobalDataStruct *) gd;

  m_MetricCalculationLock.Lock();
  m_SumOfSquaredDifference  += globalData->m_SumOfSquaredDifference;
  m_NumberOfPixelsProcessed += globalData->m_NumberOfPixelsProcessed;
  m_SumOfSquaredChange += globalData->m_SumOfSquaredChange;
  if ( m_NumberOfPixelsProcessed )
    {
    m_Metric = m_SumOfSquaredDifference / 
               static_cast<double>( m_NumberOfPixelsProcessed ); 
    m_RMSChange = vcl_sqrt( m_SumOfSquaredChange / 
               static_cast<double>( m_NumberOfPixelsProcessed ) ); 
    }
  m_MetricCalculationLock.Unlock();

  delete globalData;
}

} // end namespace itk

#endif
