/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDemonsRegistrationFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkDemonsRegistrationFunction_txx_
#define _itkDemonsRegistrationFunction_txx_

#include "itkDemonsRegistrationFunction.h"
#include "itkExceptionObject.h"
#include "vnl/vnl_math.h"

namespace itk {

/*
 * Default constructor
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
DemonsRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::DemonsRegistrationFunction()
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
DemonsRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
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

}


/*
 * Set the function state values before each iteration
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
void
DemonsRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
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

}


/*
 * Compute update at a non boundary neighbourhood
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
typename DemonsRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::PixelType
DemonsRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::ComputeUpdate(const NeighborhoodType &it, void * itkNotUsed(globalData),
                const FloatOffsetType& itkNotUsed(offset)) const
{

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
  double denominator = vnl_math_sqr( speedValue ) + 
    fixedGradientSquaredMagnitude;

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
      denominator;
    }

  return update;

}



/*
 * Compute update at a boundary neighbourhood
 */
/*
template <class TFixedImage, class TMovingImage, class TDeformationField>
typename DemonsRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::PixelType
DemonsRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::ComputeUpdate(const BoundaryNeighborhoodType &it, void * itkNotUsed(globalData),
                const FloatOffsetType& itkNotUsed(offset)) const
{

  PixelType update;
  unsigned int  j;

  for( j = 0; j < ImageDimension; j++ )
    {
    update[j] = 0.0;
    }

  return update;

}
*/


} // end namespace itk

#endif
