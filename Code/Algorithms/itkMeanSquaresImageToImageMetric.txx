/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanSquaresImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMeanSquaresImageToImageMetric_txx
#define _itkMeanSquaresImageToImageMetric_txx

#include "itkMeanSquaresImageToImageMetric.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

/**
 * Constructor
 */
template <class TFixedImage, class TMovingImage> 
MeanSquaresImageToImageMetric<TFixedImage,TMovingImage>
::MeanSquaresImageToImageMetric()
{
  m_MatchMeasure = NumericTraits<double>::Zero;
  m_MatchMeasureDerivatives = 
     DerivativeType(FixedImageType::ImageDimension);
  m_MatchMeasureDerivatives.Fill( NumericTraits<double>::Zero );
}

/**
 * Get the match Measure
 */
template <class TFixedImage, class TMovingImage> 
MeanSquaresImageToImageMetric<TFixedImage,TMovingImage>::MeasureType
MeanSquaresImageToImageMetric<TFixedImage,TMovingImage>
::GetValue( const TransformParametersType & parameters )
{

  FixedImagePointer fixedImage = this->GetFixedImage();

  if( !fixedImage ) 
    {
    itkExceptionMacro( << "Fixed image has not been assigned" );
    }

  typename FixedImageType::RegionType  fixedRegion = 
                              fixedImage->GetLargestPossibleRegion();

  const unsigned int dimension = FixedImageType::ImageDimension;
  itk::Point<double, dimension> Point;  

  double MovingValue;
  double FixedValue;

  typedef  itk::ImageRegionConstIteratorWithIndex<FixedImageType> FixedIteratorType;


  FixedIteratorType ti( fixedImage, fixedRegion );

  typename FixedImageType::IndexType index;

  m_MatchMeasure = NumericTraits< MeasureType >::Zero;

  m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters( parameters );

  while(!ti.IsAtEnd())
    {

    index = ti.GetIndex();
    
    InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint( index, inputPoint );

    OutputPointType transformedPoint = m_Transform->TransformPoint( inputPoint );

    if( m_Interpolator->IsInsideBuffer( transformedPoint ) )
      {
      MovingValue  = m_Interpolator->Evaluate( transformedPoint );
      FixedValue     = ti.Get();
      m_NumberOfPixelsCounted++;
      const double diff = MovingValue - FixedValue; 
      m_MatchMeasure += diff * diff; 
      }

    ++ti;
    }

  return m_MatchMeasure;

}





/**
 * Get the Derivative Measure
 */
template < class TFixedImage, class TMovingImage> 
MeanSquaresImageToImageMetric<TFixedImage,TMovingImage>::DerivativeType
MeanSquaresImageToImageMetric<TFixedImage,TMovingImage>
::GetDerivative( const TransformParametersType & parameters )
{

  const double delta = 0.00011;
  TransformParametersType testPoint;
  testPoint = parameters;

  const unsigned int dimension = FixedImageType::ImageDimension;
  for( unsigned int i=0; i<dimension; i++) 
    {
    testPoint[i] -= delta;
    const MeasureType valuep0 = this->GetValue( testPoint );
    testPoint[i] += 2*delta;
    const MeasureType valuep1 = this->GetValue( testPoint );
    m_MatchMeasureDerivatives[i] = (valuep1 - valuep0 ) / ( 2 * delta );
    testPoint[i] = parameters[i];
    }

  return m_MatchMeasureDerivatives;

}


/**
 * Get both the match Measure and theDerivative Measure 
 */
template <class TFixedImage, class TMovingImage> 
void
MeanSquaresImageToImageMetric<TFixedImage,TMovingImage>
::GetValueAndDerivative(const TransformParametersType & parameters, 
                        MeasureType & Value, DerivativeType  & Derivative)
{
  Value      = this->GetValue( parameters );
  Derivative = this->GetDerivative( parameters );
}

} // end namespace itk


#endif
