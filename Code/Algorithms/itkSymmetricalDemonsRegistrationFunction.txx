/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSymmetricalDemonsRegistrationFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSymmetricalDemonsRegistrationFunction_txx_
#define _itkSymmetricalDemonsRegistrationFunction_txx_

#include "itkSymmetricalDemonsRegistrationFunction.h"
#include "itkExceptionObject.h"
#include "vnl/vnl_math.h"

namespace itk {

/*
 * Default constructor
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
SymmetricalDemonsRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::SymmetricalDemonsRegistrationFunction()
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
  m_FixedImageSpacing.Fill( 1.0 );
  m_FixedImageOrigin.Fill( 0.0 );
  m_Normalizer = 0.0;
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
SymmetricalDemonsRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
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
SymmetricalDemonsRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::InitializeIteration()
{
  if( !m_MovingImage || !m_FixedImage || !m_MovingImageInterpolator )
    {
    itkExceptionMacro( << "MovingImage, FixedImage and/or Interpolator not set" );
    }

  // cache fixed image information
  m_FixedImageSpacing    = m_FixedImage->GetSpacing();
  m_FixedImageOrigin     = m_FixedImage->GetOrigin();

  // compute the normalizer
  m_Normalizer      = 0.0;
  for( unsigned int k = 0; k < ImageDimension; k++ )
    {
    m_Normalizer += m_FixedImageSpacing[k] * m_FixedImageSpacing[k];
    }
  m_Normalizer /= static_cast<double>( ImageDimension );


  // setup gradient calculator
  m_FixedImageGradientCalculator->SetInputImage( m_FixedImage );

  // setup moving image interpolator
  m_MovingImageInterpolator->SetInputImage( m_MovingImage );

}


/*
 * Compute update at a non boundary neighbourhood
 */
template <class TFixedImage, class TMovingImage, class TDeformationField>
typename SymmetricalDemonsRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::PixelType
SymmetricalDemonsRegistrationFunction<TFixedImage,TMovingImage,TDeformationField>
::ComputeUpdate(const NeighborhoodType &it, void * itkNotUsed(globalData),
                const FloatOffsetType& itkNotUsed(offset))
{

  PixelType update;
  IndexType FirstIndex = m_FixedImage->GetLargestPossibleRegion().GetIndex();
  IndexType LastIndex = m_FixedImage->GetLargestPossibleRegion().GetIndex() + 
                        m_FixedImage->GetLargestPossibleRegion().GetSize();

  IndexType index = it.GetIndex();



  // Get fixed image related information
  double fixedValue;
  CovariantVectorType fixedGradient;

  // Note: no need to check the index is within
  // fixed image buffer. This is done by the external filter.
  fixedValue = (double) m_FixedImage->GetPixel( index );
  fixedGradient = m_FixedImageGradientCalculator->EvaluateAtIndex( index );



  // Get moving image related information
  IndexType tmpIndex = index;
  PointType mappedNeighPoint;
  PointType mappedCenterPoint;
  CovariantVectorType movingGradient;
  double movingValue;

  for( unsigned int dim = 0; dim < ImageDimension; dim++ ){

    // bounds checking
    if( index[dim] < static_cast<long>(FirstIndex[dim]) + 1 || index[dim] > (LastIndex[dim] - 2 )){
      movingGradient[dim] = 0.0;
    }

    else{
      tmpIndex[dim] += 1;
      for( unsigned int j = 0; j < ImageDimension; j++ ){
        mappedNeighPoint[j] = double( tmpIndex[j] ) * m_FixedImageSpacing[j] + m_FixedImageOrigin[j];
        mappedNeighPoint[j] += this->GetDeformationField()->GetPixel(tmpIndex)[j];
      }
      if( m_MovingImageInterpolator->IsInsideBuffer( mappedNeighPoint ) ){
        movingGradient[dim] = m_MovingImageInterpolator->Evaluate( mappedNeighPoint );
      }
      else{
        movingGradient[dim] = 0.0;
      }

      tmpIndex[dim] -= 2;
      for( unsigned int j = 0; j < ImageDimension; j++ ){
        mappedNeighPoint[j] = double( tmpIndex[j] ) * m_FixedImageSpacing[j] + m_FixedImageOrigin[j];
        mappedNeighPoint[j] += this->GetDeformationField()->GetPixel(tmpIndex)[j];
      }
      if( m_MovingImageInterpolator->IsInsideBuffer( mappedNeighPoint ) ){
        movingGradient[dim] -= m_MovingImageInterpolator->Evaluate( mappedNeighPoint );
      }

      movingGradient[dim] *= 0.5 / m_FixedImageSpacing[dim];
      tmpIndex[dim] += 1;
    }

    mappedCenterPoint[dim] = double( index[dim] ) * m_FixedImageSpacing[dim] + m_FixedImageOrigin[dim];
    mappedCenterPoint[dim] += it.GetCenterPixel()[dim];
  }

  if( m_MovingImageInterpolator->IsInsideBuffer( mappedCenterPoint ) ){
    movingValue = m_MovingImageInterpolator->Evaluate( mappedCenterPoint );
  }
  else{
    movingValue = 0.0;
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
  for( unsigned int dim = 0; dim < ImageDimension; dim++ ){
    fixedPlusMovingGradientSquaredMagnitude += vnl_math_sqr( fixedGradient[dim] + movingGradient[dim] );
  }

  double speedValue = fixedValue - movingValue;
  double denominator = vnl_math_sqr( speedValue ) / m_Normalizer + 
    fixedPlusMovingGradientSquaredMagnitude;

  if ( vnl_math_abs(speedValue) < m_IntensityDifferenceThreshold || 
          denominator < m_DenominatorThreshold )
    {
    for( unsigned int j = 0; j < ImageDimension; j++ )
      {
      update[j] = 0.0;
      }
    return update;
    }

  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    update[j] = 2 * speedValue * (fixedGradient[j] + movingGradient[j]) / denominator;
    }


  return update;

}

} // end namespace itk

#endif
