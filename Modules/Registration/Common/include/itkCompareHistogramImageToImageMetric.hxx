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
#ifndef itkCompareHistogramImageToImageMetric_hxx
#define itkCompareHistogramImageToImageMetric_hxx

#include "itkCompareHistogramImageToImageMetric.h"
#include "itkHistogram.h"

namespace itk
{
template< typename TFixedImage, typename TMovingImage >
CompareHistogramImageToImageMetric< TFixedImage, TMovingImage >::CompareHistogramImageToImageMetric()
{
  m_TrainingFixedImage        = ITK_NULLPTR; // has to be provided by the user.
  m_TrainingMovingImage       = ITK_NULLPTR; // has to be provided by the user.
  m_TrainingTransform         = ITK_NULLPTR; // has to be provided by the user.
  m_TrainingInterpolator      = ITK_NULLPTR; // has to be provided by the user.
  m_TrainingHistogram         = ITK_NULLPTR; // either provided by the user or created
}

template< typename TFixedImage, typename TMovingImage >
void
CompareHistogramImageToImageMetric< TFixedImage, TMovingImage >
::Initialize()
{
  Superclass::Initialize();

  if ( !m_TrainingHistogram )
    {
    FormTrainingHistogram();
    }
}

template< typename TFixedImage, typename TMovingImage >
void
CompareHistogramImageToImageMetric< TFixedImage, TMovingImage >
::FormTrainingHistogram()
{
  // Check to make sure everything is set
  if ( !m_TrainingTransform )
    {
    itkExceptionMacro(<< "Training Transform is not present");
    }

  if ( !m_TrainingInterpolator )
    {
    itkExceptionMacro(<< "Training Interpolator is not present");
    }

  if ( !m_TrainingMovingImage )
    {
    itkExceptionMacro(<< "Training MovingImage is not present");
    }

// If the image is provided by a source, update the source.
  if ( m_TrainingMovingImage->GetSource() )
    {
    m_TrainingMovingImage->GetSource()->Update();
    }

  if ( !m_TrainingFixedImage )
    {
    itkExceptionMacro(<< "Training FixedImage is not present");
    }

// If the image is provided by a source, update the source.
  if ( m_TrainingFixedImage->GetSource() )
    {
    m_TrainingFixedImage->GetSource()->Update();
    }

  if ( m_TrainingFixedImageRegion.GetNumberOfPixels() == 0 )
    {
    itkExceptionMacro(<< "TrainingFixedImageRegion is empty");
    }

// Make sure the FixedImageRegion is within the FixedImage buffered region
  if ( !m_TrainingFixedImageRegion.Crop( m_TrainingFixedImage->GetBufferedRegion() ) )
    {
    itkExceptionMacro(<< "TrainingFixedImageRegion does not overlap the training fixed image buffered region");
    }

  this->m_TrainingInterpolator->SetInputImage( GetTrainingMovingImage() );

  // Create the exact histogram structure as the one to be used
  // to evaluate the metric. This code is mostly copied
  // from itkHistogramImageToImageMetric
  this->m_TrainingHistogram = HistogramType::New();
  this->m_TrainingHistogram->SetMeasurementVectorSize(2);
  this->m_TrainingHistogram->Initialize(this->Superclass::m_HistogramSize,
                                        this->Superclass::m_LowerBound,
                                        this->Superclass::m_UpperBound);
  typedef itk::ImageRegionConstIteratorWithIndex< FixedImageType >
  TrainingFixedIteratorType;
  typename FixedImageType::IndexType index;
  typename FixedImageType::RegionType fixedRegion;
  typename HistogramType::IndexType hIndex;

  TrainingFixedIteratorType ti(this->m_TrainingFixedImage,
                               this->m_TrainingFixedImageRegion);

  int NumberOfPixelsCounted = 0;

  ti.GoToBegin();
  while ( !ti.IsAtEnd() )
    {
    index = ti.GetIndex();
    if ( this->m_TrainingFixedImageRegion.IsInside(index)
         && ( !this->Superclass::GetUsePaddingValue()
              || ( this->Superclass::GetUsePaddingValue()
                   && ti.Get() > this->Superclass::GetPaddingValue() ) ) )
      {
      typename Superclass::InputPointType inputPoint;
      this->m_TrainingFixedImage->
      TransformIndexToPhysicalPoint(index, inputPoint);

      typename Superclass::OutputPointType transformedPoint =
        this->m_TrainingTransform->TransformPoint(inputPoint);

      if ( this->m_TrainingInterpolator->IsInsideBuffer(transformedPoint) )
        {
        const RealType TrainingMovingValue =
          this->m_TrainingInterpolator->Evaluate(transformedPoint);
        const RealType TrainingFixedValue = ti.Get();
        NumberOfPixelsCounted++;

        typename HistogramType::MeasurementVectorType sample;
        sample.SetSize(2);
        sample[0] = TrainingFixedValue;
        sample[1] = TrainingMovingValue;

        this->m_TrainingHistogram->GetIndex( sample, hIndex );
        this->m_TrainingHistogram->IncreaseFrequencyOfIndex( hIndex, 1 );
        }
      }

    ++ti;
    }

  if ( NumberOfPixelsCounted == 0 )
    {
    itkExceptionMacro(<< "All the points mapped to outside of the Training moving \
age");
    }
}

template< typename TFixedImage, typename TMovingImage >
void CompareHistogramImageToImageMetric< TFixedImage, TMovingImage >::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "TrainingFixedImage: ";
  if ( m_TrainingFixedImage.IsNull() )
    {
    os << 0 << std::endl;
    }
  else
    {
    os << m_TrainingFixedImage << std::endl;
    }

  os << indent << "TrainingMovingImage: ";
  if ( m_TrainingMovingImage.IsNull() )
    {
    os << 0 << std::endl;
    }
  else
    {
    os << m_TrainingMovingImage << std::endl;
    }
  os << indent << "TrainingTransform: ";
  if ( m_TrainingTransform.IsNull() )
    {
    os << 0 << std::endl;
    }
  else
    {
    os << m_TrainingTransform << std::endl;
    }
  os << indent << "TrainingInterpolator: ";
  if ( m_TrainingInterpolator.IsNull() )
    {
    os << 0 << std::endl;
    }
  else
    {
    os << m_TrainingInterpolator << std::endl;
    }
  os << indent << "TrainingHistogram: ";
  if ( m_TrainingHistogram.IsNull() )
    {
    os << 0 << std::endl;
    }
  else
    {
    os << m_TrainingHistogram << std::endl;
    }
  os << indent << "TrainingFixedImageRegion: " << m_TrainingFixedImageRegion
     << std::endl;
}
} // End namespace itk

#endif // itkCompareHistogramImageToImageMetric_hxx
