/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanReciprocalSquareDifferenceImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMeanReciprocalSquareDifferenceImageToImageMetric_txx
#define __itkMeanReciprocalSquareDifferenceImageToImageMetric_txx

#include "itkMeanReciprocalSquareDifferenceImageToImageMetric.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{
/**
 * Constructor
 */
template< class TFixedImage, class TMovingImage >
MeanReciprocalSquareDifferenceImageToImageMetric< TFixedImage, TMovingImage >
::MeanReciprocalSquareDifferenceImageToImageMetric()
{
  m_Lambda = 1.0;
  m_Delta  = 0.00011;
}

/**
 * PrintSelf
 */
template< class TFixedImage, class TMovingImage >
void
MeanReciprocalSquareDifferenceImageToImageMetric< TFixedImage, TMovingImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "Lambda factor = " << m_Lambda << std::endl;
  os << "Delta  value  = " << m_Delta  << std::endl;
}

/*
 * Get the match Measure
 */
template< class TFixedImage, class TMovingImage >
typename MeanReciprocalSquareDifferenceImageToImageMetric< TFixedImage, TMovingImage >::MeasureType
MeanReciprocalSquareDifferenceImageToImageMetric< TFixedImage, TMovingImage >
::GetValue(const TransformParametersType & parameters) const
{
  FixedImageConstPointer fixedImage = this->m_FixedImage;

  if ( !fixedImage )
    {
    itkExceptionMacro(<< "Fixed image has not been assigned");
    }

  const unsigned int              dimension = FixedImageType::ImageDimension;
  itk::Point< double, dimension > Point;

  double MovingValue;
  double FixedValue;

  typedef  itk::ImageRegionConstIteratorWithIndex< FixedImageType > FixedIteratorType;

  FixedIteratorType ti( fixedImage, this->GetFixedImageRegion() );

  typename FixedImageType::IndexType index;

  MeasureType measure = NumericTraits< MeasureType >::Zero;

  this->m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters(parameters);

  while ( !ti.IsAtEnd() )
    {
    index = ti.GetIndex();

    InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint(index, inputPoint);

    if ( this->m_FixedImageMask && !this->m_FixedImageMask->IsInside(inputPoint) )
      {
      ++ti;
      continue;
      }

    TransformType const *transform = this->m_Transform;
    OutputPointType      transformedPoint = transform->TransformPoint(inputPoint);

    if ( this->m_MovingImageMask && !this->m_MovingImageMask->IsInside(transformedPoint) )
      {
      ++ti;
      continue;
      }

    if ( this->m_Interpolator->IsInsideBuffer(transformedPoint) )
      {
      MovingValue  = this->m_Interpolator->Evaluate(transformedPoint);
      FixedValue     = ti.Get();
      this->m_NumberOfPixelsCounted++;
      const double diff = MovingValue - FixedValue;
      measure += 1.0f / ( 1.0f + m_Lambda * ( diff * diff ) );
      }

    ++ti;
    }

  return measure;
}

/**
 * Get the Derivative Measure
 */
template< class TFixedImage, class TMovingImage >
void
MeanReciprocalSquareDifferenceImageToImageMetric< TFixedImage, TMovingImage >
::GetDerivative(const TransformParametersType & parameters,
                DerivativeType & derivative) const
{
  TransformParametersType testPoint;

  testPoint = parameters;

  const unsigned int numberOfParameters = this->GetNumberOfParameters();
  derivative = DerivativeType(numberOfParameters);

  for ( unsigned int i = 0; i < numberOfParameters; i++ )
    {
    testPoint[i] -= m_Delta;
    const MeasureType valuep0 = this->GetValue(testPoint);
    testPoint[i] += 2 * m_Delta;
    const MeasureType valuep1 = this->GetValue(testPoint);
    derivative[i] = ( valuep1 - valuep0 ) / ( 2 * m_Delta );
    testPoint[i] = parameters[i];
    }
}

/**
 * Get both the match Measure and theDerivative Measure
 */
template< class TFixedImage, class TMovingImage >
void
MeanReciprocalSquareDifferenceImageToImageMetric< TFixedImage, TMovingImage >
::GetValueAndDerivative(const TransformParametersType & parameters,
                        MeasureType & Value, DerivativeType  & Derivative) const
{
  Value      = this->GetValue(parameters);
  this->GetDerivative(parameters, Derivative);
}
} // end namespace itk

#endif
