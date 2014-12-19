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
#ifndef itkMeanReciprocalSquareDifferenceImageToImageMetric_hxx
#define itkMeanReciprocalSquareDifferenceImageToImageMetric_hxx

#include "itkMeanReciprocalSquareDifferenceImageToImageMetric.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TFixedImage, typename TMovingImage >
MeanReciprocalSquareDifferenceImageToImageMetric< TFixedImage, TMovingImage >
::MeanReciprocalSquareDifferenceImageToImageMetric()
{
  m_Lambda = 1.0;
  m_Delta  = 0.00011;
}

/**
 * PrintSelf
 */
template< typename TFixedImage, typename TMovingImage >
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
template< typename TFixedImage, typename TMovingImage >
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

  MeasureType measure = NumericTraits< MeasureType >::ZeroValue();

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
template< typename TFixedImage, typename TMovingImage >
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
template< typename TFixedImage, typename TMovingImage >
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
