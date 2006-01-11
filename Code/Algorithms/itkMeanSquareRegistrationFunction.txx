/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanSquareRegistrationFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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

  this->SetEnergy(0.0);
  m_TimeStep = 1.0;
  m_DenominatorThreshold = 1e-9;
  m_IntensityDifferenceThreshold = 0.001;
  this->SetMovingImage(NULL);
  this->SetFixedImage(NULL);
  m_FixedImageSpacing.Fill( 1.0 );
  m_FixedImageOrigin.Fill( 0.0 );
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
  Superclass::PrintSelf(os, indent);
/*
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
  if( !this->GetMovingImage() || !this->GetFixedImage() || !m_MovingImageInterpolator )
    {
    itkExceptionMacro( << "MovingImage, FixedImage and/or Interpolator not set" );
    }

  // cache fixed image information
  m_FixedImageSpacing    = this->GetFixedImage()->GetSpacing();
  m_FixedImageOrigin     = this->GetFixedImage()->GetOrigin();

  // setup gradient calculator
  m_FixedImageGradientCalculator->SetInputImage( this->GetFixedImage() );

  // setup moving image interpolator
  m_MovingImageInterpolator->SetInputImage( this->GetMovingImage() );
  
  this->SetEnergy(0.0);
}


/*
 * Compute update at a non boundary neighbourhood
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
typename MeanSquareRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::PixelType
MeanSquareRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::ComputeUpdate(const NeighborhoodType &it, void * itkNotUsed(globalData),
                const FloatOffsetType& itkNotUsed(offset)) 
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
  fixedValue = (double) this->GetFixedImage()->GetPixel( index );
  fixedGradient = m_FixedImageGradientCalculator->EvaluateAtIndex( index );
  for( j = 0; j < ImageDimension; j++ )
    {
    fixedGradientSquaredMagnitude += vnl_math_sqr( fixedGradient[j] ) * m_FixedImageSpacing[j];
    } 

  // Get moving image related information
  double movingValue;
  PointType mappedPoint;
  typename Superclass::DeformationFieldType::PixelType itvec=this->GetDeformationField()->GetPixel(index);

  for( j = 0; j < ImageDimension; j++ )
    {
     mappedPoint[j] = double( index[j] ) * m_FixedImageSpacing[j] + 
      m_FixedImageOrigin[j];
//     mappedPoint[j] += it.GetCenterPixel()[j];
      mappedPoint[j] += itvec[j];
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
  this->m_Energy+=speedValue*speedValue;

  bool normalizemetric=this->GetNormalizeGradient();  
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
      denominator*this->m_GradientStep;
    }

  return update;

}

 



} // end namespace itk

#endif
