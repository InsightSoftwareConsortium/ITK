/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanSquareRegistrationFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMeanSquareRegistrationFunction_txx_
#define _itkMeanSquareRegistrationFunction_txx_

#include "itkMeanSquareRegistrationFunction.h"
#include "itkExceptionObject.h"
#include "vnl/vnl_math.h"

namespace itk {

/*
 * Default constructor
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
MeanSquareRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::MeanSquareRegistrationFunction()
{

  RadiusType r;
  unsigned int j;
  for( j = 0; j < ImageDimension; j++ )
    {
    r[j] = 0;
    }
  this->SetRadius(r);

  m_Energy = 0.0;
  m_TimeStep = 1.0;
  m_DenominatorThreshold = 1e-9;
  m_IntensityDifferenceThreshold = 0.001;
  m_MovingImage = NULL;
  m_FixedImage = NULL;
  m_FixedImageSpacing = NULL;
  m_FixedImageOrigin = NULL;
  m_FixedImageGradientCalculator = GradientCalculatorType::New();


  typename DefaultInterpolatorType::Pointer interp =
    DefaultInterpolatorType::New();

  m_MovingImageInterpolator = static_cast<InterpolatorType*>(
    interp.GetPointer() );


}


/*
 * Standard "PrintSelf" method.
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
MeanSquareRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::PrintSelf(std::ostream& os, Indent indent) const
{
/*  Superclass::PrintSelf(os, indent);

  os << indent << "MovingImageIterpolator: ";
  os << m_MovingImageInterpolator.GetPointer() << std::endl;
  os << indent << "FixedImageGradientCalculator: ";
  os << m_FixedImageGradientCalculator.GetPointer() << std::endl;
  os << indent << "DenominatorThreshold: ";
  os << m_DenominatorThreshold << std::endl;
  os << indent << "IntensityDifferenceThreshold: ";
  os << m_IntensityDifferenceThreshold << std::endl;
*/
}


/*
 * Set the function state values before each iteration
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
MeanSquareRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::InitializeIteration()
{
  if( !m_MovingImage || !m_FixedImage || !m_MovingImageInterpolator )
    {
    itkExceptionMacro( << "MovingImage, FixedImage and/or Interpolator not set" );
    throw ExceptionObject(__FILE__,__LINE__);
    }

  // cache fixed image information
  m_FixedImageSpacing    = m_FixedImage->GetSpacing();
  m_FixedImageOrigin     = m_FixedImage->GetOrigin();

  // setup gradient calculator
  m_FixedImageGradientCalculator->SetInputImage( m_FixedImage );

  // setup moving image interpolator
  m_MovingImageInterpolator->SetInputImage( m_MovingImage );

  std::cout << " Energy " << m_Energy << std::endl;
  m_Energy=0.0;
}


/*
 * Compute update at a non boundary neighbourhood
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
typename MeanSquareRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::PixelType
MeanSquareRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::ComputeUpdate(const NeighborhoodType &it, void * globalData,
                const FloatOffsetType& offset) const
{

//std::cout << " Update " << std::endl;
  PixelType update;
  unsigned int j;

  IndexType index = it.GetIndex();

  // Get fixed image related information
  double fixedValue;
  CovariantVectorType fixedGradient;
  double fixedGradientSquaredMagnitude = 0;

  // Note: no need to check the index is within
  // fixed image buffer. This is done by the external filter.
  fixedValue = (double) m_FixedImage->GetPixel( index );
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    fixedGradient[j] = m_FixedImageGradientCalculator->EvaluateAtIndex( index, j );
    fixedGradientSquaredMagnitude += vnl_math_sqr( fixedGradient[j] ) * m_FixedImageSpacing[j];
    } 

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
    movingValue = 0.0;
    }

  // Compute update
  double speedValue = fixedValue - movingValue;
  m_Energy+=speedValue*speedValue;

  bool normalizemetric=m_NormalizeGradient;  
  double denominator = 1.0;
  if (normalizemetric) 
  {  
    denominator = speedValue*speedValue *fixedGradientSquaredMagnitude;
    denominator = sqrt(denominator);
  }
  if (denominator == 0) denominator=1.0;
 
  if ( vnl_math_abs(speedValue) < m_IntensityDifferenceThreshold || 
    denominator < m_DenominatorThreshold )
    {
    for( j = 0; j < ImageDimension; j++ )
      {
      update[j] = 0.0;
      }
    return update;
    }

  for( j = 0; j < ImageDimension; j++ )
    {
    update[j] = speedValue * fixedGradient[j] * vnl_math_sqr(m_FixedImageSpacing[j]) / 
      (denominator*m_GradientStep);
    }

  return update;

}

 



} // end namespace itk

#endif
