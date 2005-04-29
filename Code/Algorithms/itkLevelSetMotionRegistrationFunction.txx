/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetMotionRegistrationFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkLevelSetMotionRegistrationFunction_txx_
#define _itkLevelSetMotionRegistrationFunction_txx_

#include "itkLevelSetMotionRegistrationFunction.h"
#include "itkExceptionObject.h"
#include "vnl/vnl_math.h"

namespace itk {

/*
 * Default constructor
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
LevelSetMotionRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::LevelSetMotionRegistrationFunction()
{

  RadiusType r;
  unsigned int j;
  for( j = 0; j < ImageDimension; j++ )
    {
    r[j] = 0;
    }
  this->SetRadius(r);

  m_Alpha = 0.1;
  m_GradientMagnitudeThreshold = 1e-9;
  m_IntensityDifferenceThreshold = 0.001;
  m_GradientSmoothingStandardDeviations = 1.0;
  this->SetMovingImage(NULL);
  this->SetFixedImage(NULL);
  m_FixedImageSpacing.Fill( 1.0 );
  m_FixedImageOrigin.Fill( 0.0 );

  typename DefaultInterpolatorType::Pointer interp =
    DefaultInterpolatorType::New();

  m_MovingImageInterpolator = static_cast<InterpolatorType*>(
    interp.GetPointer() );

  m_Metric = NumericTraits<double>::max();
  m_SumOfSquaredDifference = 0.0;
  m_NumberOfPixelsProcessed = 0L;
  m_RMSChange = NumericTraits<double>::max();
  m_SumOfSquaredChange = 0.0;

  m_MovingImageSmoothingFilter = MovingImageSmoothingFilterType::New();
  m_MovingImageSmoothingFilter
    ->SetSigma( m_GradientSmoothingStandardDeviations );
  m_MovingImageSmoothingFilter->SetNormalizeAcrossScale(false);

  m_SmoothMovingImageInterpolator
    = static_cast<InterpolatorType *>(
      DefaultInterpolatorType::New().GetPointer());
}


/*
 * Standard "PrintSelf" method.
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
LevelSetMotionRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "MovingImageIterpolator: ";
  os << m_MovingImageInterpolator.GetPointer() << std::endl;
  os << indent << "IntensityDifferenceThreshold: ";
  os << m_IntensityDifferenceThreshold << std::endl;
  os << indent << "GradientMagnitudeThreshold: ";
  os << m_GradientMagnitudeThreshold << std::endl;
  os << indent << "Alpha: ";
  os << m_Alpha << std::endl;

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
LevelSetMotionRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::SetAlpha(double alpha)
{
  m_Alpha = alpha;
}

/**
 *
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
double
LevelSetMotionRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::GetAlpha() const
{
  return m_Alpha;
}

/**
 *
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
LevelSetMotionRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::SetIntensityDifferenceThreshold(double threshold)
{
  m_IntensityDifferenceThreshold = threshold;
}

/**
 *
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
double
LevelSetMotionRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::GetIntensityDifferenceThreshold() const
{
  return m_IntensityDifferenceThreshold;
}


/**
 *
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
LevelSetMotionRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::SetGradientMagnitudeThreshold(double threshold)
{
  m_GradientMagnitudeThreshold = threshold;
}

/**
 *
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
double
LevelSetMotionRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::GetGradientMagnitudeThreshold() const
{
  return m_GradientMagnitudeThreshold;
}


/**
 *
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
LevelSetMotionRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::SetGradientSmoothingStandardDeviations(double sigma)
{
  m_GradientSmoothingStandardDeviations = sigma;
}

/**
 *
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
double
LevelSetMotionRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::GetGradientSmoothingStandardDeviations() const
{
  return m_GradientSmoothingStandardDeviations;
}


/*
 * Set the function state values before each iteration
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
LevelSetMotionRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::InitializeIteration()
{
  if( !this->GetMovingImage() || !this->GetFixedImage() || !m_MovingImageInterpolator )
    {
    itkExceptionMacro( << "MovingImage, FixedImage and/or Interpolator not set" );
    }

  // cache fixed image information
  m_FixedImageSpacing    = this->GetFixedImage()->GetSpacing();
  m_FixedImageOrigin     = this->GetFixedImage()->GetOrigin();

  // create a smoothed version of the moving image for the calculation
  // of gradients.  due to the pipeline structure, this will only be
  // calculated once. InitializeIteration() is called in a single
  // threaded execution model. 
  m_MovingImageSmoothingFilter->SetInput( this->GetMovingImage() );
  m_MovingImageSmoothingFilter
    ->SetSigma( m_GradientSmoothingStandardDeviations );
  m_MovingImageSmoothingFilter->Update();

  m_SmoothMovingImageInterpolator
     ->SetInputImage( m_MovingImageSmoothingFilter->GetOutput() );
  
  // setup moving image interpolator
  m_MovingImageInterpolator->SetInputImage( this->GetMovingImage() );

  // initialize metric computation variables
  m_SumOfSquaredDifference  = 0.0;
  m_NumberOfPixelsProcessed = 0L;
  m_SumOfSquaredChange      = 0.0;

}


/*
 * Compute update at a specify neighbourhood
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
typename LevelSetMotionRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::PixelType
LevelSetMotionRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::ComputeUpdate(const NeighborhoodType &it, void * gd,
                const FloatOffsetType& itkNotUsed(offset))
{
  PixelType update;
  unsigned int j;

  IndexType index = it.GetIndex();

  // Get fixed image related information
  double fixedValue;

  // Note: no need to check the index is within
  // fixed image buffer. This is done by the external filter.
  fixedValue = (double) this->GetFixedImage()->GetPixel( index );

  // Get moving image related information
  double movingValue;
  PointType mappedPoint;

  for( j = 0; j < ImageDimension; j++ )
    {
    mappedPoint[j] = double( index[j] ) * m_FixedImageSpacing[j] + 
      m_FixedImageOrigin[j];
    mappedPoint[j] += it.GetCenterPixel()[j];
    }
  if( m_MovingImageInterpolator->IsInsideBuffer( mappedPoint ) )
    {
    movingValue = m_MovingImageInterpolator->Evaluate( mappedPoint );
    }
  else
    {
    for( j = 0; j < ImageDimension; j++ )
      {
      update[j] = 0.0;
      }
    return update;
    }

  // Calculate the gradient using minmod finite differences
  //
  //
  //
  CovariantVectorType gradient;
  double gradientMagnitude = 0.0;

  double forwardDifferences[ImageDimension];
  double backwardDifferences[ImageDimension];
  double centralValue;
  PointType mPoint( mappedPoint );

  MovingSpacingType mSpacing = this->GetMovingImage()->GetSpacing();

  // first calculate the forward and backward differences on the
  // smooth image. Do we need to structure the gradient calculation to
  // take into account the Jacobian of the deformation field? i.e. in
  // which coordinate frame do we ultimately want the gradient vector?
  centralValue = m_SmoothMovingImageInterpolator->Evaluate( mPoint );
  for (j=0; j < ImageDimension; j++)
    {
    mPoint[j] += mSpacing[j];
    if( m_SmoothMovingImageInterpolator->IsInsideBuffer( mPoint ) )
      {
      forwardDifferences[j] = m_SmoothMovingImageInterpolator->Evaluate(mPoint)
        - centralValue;
      forwardDifferences[j] /= mSpacing[j];
      }
    else
      {
      forwardDifferences[j] = 0.0;
      }

    mPoint[j] -= (2.0 * mSpacing[j]);
    if( m_SmoothMovingImageInterpolator->IsInsideBuffer( mPoint ) )
      {
      backwardDifferences[j] = centralValue
        - m_SmoothMovingImageInterpolator->Evaluate( mPoint );
      backwardDifferences[j] /= mSpacing[j];
      }
    else
      {
      backwardDifferences[j] = 0.0;
      }

    // std::cout << "F(" << j << ") : " << forwardDifferences[j] << std::endl;
    // std::cout << "B(" << j << ") : " << backwardDifferences[j] << std::endl;

    mPoint[j] += mSpacing[j];
    }

  // minmod finite difference
  //
  // m(x,y) = sign(x) min(|x|, |y|)    if xy >  0
  //          0                        if xy <= 0
  //
  // gradient[j] = m(forwardDifferences[j], backwardDifferences[j])
  //
  double gvalue;
  double bvalue;
  for( j = 0; j < ImageDimension; j++ )
    {
    if (forwardDifferences[j] * backwardDifferences[j] > 0.0)
      {
      gvalue = vnl_math_abs(forwardDifferences[j]);
      bvalue = vnl_math_abs(backwardDifferences[j]);
      if (gvalue > bvalue)
        {
        gvalue = bvalue;
        }
      gradient[j] = gvalue * vnl_math_sgn(forwardDifferences[j]);
      }
    else
      {
      gradient[j] = 0.0;
      }

    gradientMagnitude += vnl_math_sqr( gradient[j] );
    }
  gradientMagnitude = vcl_sqrt( gradientMagnitude );
  
  /**
   * Compute Update.
   */
  double speedValue = fixedValue - movingValue;
  
  // update the metric
  GlobalDataStruct *globalData = (GlobalDataStruct *)gd;
  if ( globalData )
    {
    globalData->m_SumOfSquaredDifference += vnl_math_sqr( speedValue );
    globalData->m_NumberOfPixelsProcessed += 1;
    }

  if ( vnl_math_abs(speedValue) < m_IntensityDifferenceThreshold 
       || gradientMagnitude < m_GradientMagnitudeThreshold )
    {
    for( j = 0; j < ImageDimension; j++ )
      {
      update[j] = 0.0;
      }
    return update;
    }

  double L1norm = 0.0;
  for( j = 0; j < ImageDimension; j++ )
    {
    update[j] = speedValue * gradient[j] / (gradientMagnitude + m_Alpha);
    if ( globalData )
      {
      globalData->m_SumOfSquaredChange += vnl_math_sqr( update[j] );

      // build up the L1norm of the update, normalized by the pixel
      // spacing. we will use this to calculate a timestep which
      // converts the update (measured in intensity) to a vector
      // measured in physical units (mm).
      L1norm += (vnl_math_abs(update[j]) / this->GetMovingImage()->GetSpacing()[j]);
      }
    }

  // Store the L1 norm of the update vector if it is the largest
  // update.  This is used in calculating the timestep.
  if (globalData && (L1norm > globalData->m_MaxL1Norm))
    {
    globalData->m_MaxL1Norm = L1norm;
    }
  
  return update;

}

/**
 * Compute the global time step for this iteration.
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
typename LevelSetMotionRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>::TimeStepType
LevelSetMotionRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::ComputeGlobalTimeStep(void *GlobalData) const
{
  TimeStepType dt = 1.0;

  GlobalDataStruct *d = (GlobalDataStruct *)GlobalData;

  if (d->m_MaxL1Norm > 0.0)
    {
    dt = 1.0 / d->m_MaxL1Norm;
    // std::cout << "Computed timestep: " << dt << std::endl;
    }
  else
    {
    // std::cout << "Using default timestep: " << dt << std::endl;
    }

  return dt;
}


/*
 * Update the metric and release the per-thread-global data.
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
LevelSetMotionRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
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
