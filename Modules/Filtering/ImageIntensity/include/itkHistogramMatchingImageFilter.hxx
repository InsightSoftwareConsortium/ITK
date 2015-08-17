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
#ifndef itkHistogramMatchingImageFilter_hxx
#define itkHistogramMatchingImageFilter_hxx

#include "itkHistogramMatchingImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "itkMath.h"
#include <vector>

namespace itk
{
/**
 *
 */
template< typename TInputImage, typename TOutputImage, typename THistogramMeasurement >
HistogramMatchingImageFilter< TInputImage, TOutputImage, THistogramMeasurement >
::HistogramMatchingImageFilter() :
  m_NumberOfHistogramLevels(256),
  m_NumberOfMatchPoints(1),
  m_ThresholdAtMeanIntensity(true),
  m_SourceIntensityThreshold(NumericTraits<InputPixelType>::ZeroValue()),
  m_ReferenceIntensityThreshold(NumericTraits<InputPixelType>::ZeroValue()),
  m_OutputIntensityThreshold(NumericTraits<OutputPixelType>::ZeroValue()),
  m_SourceMinValue(NumericTraits<THistogramMeasurement>::ZeroValue()),
  m_SourceMaxValue(NumericTraits<THistogramMeasurement>::ZeroValue()),
  m_SourceMeanValue(NumericTraits<THistogramMeasurement>::ZeroValue()),
  m_ReferenceMinValue(NumericTraits<THistogramMeasurement>::ZeroValue()),
  m_ReferenceMaxValue(NumericTraits<THistogramMeasurement>::ZeroValue()),
  m_ReferenceMeanValue(NumericTraits<THistogramMeasurement>::ZeroValue()),
  m_OutputMinValue(NumericTraits<THistogramMeasurement>::ZeroValue()),
  m_OutputMaxValue(NumericTraits<THistogramMeasurement>::ZeroValue()),
  m_OutputMeanValue(NumericTraits<THistogramMeasurement>::ZeroValue()),
  m_SourceHistogram(HistogramType::New()),
  m_ReferenceHistogram(HistogramType::New()),
  m_OutputHistogram(HistogramType::New()),
  m_LowerGradient(0.0),
  m_UpperGradient(0.0)
{
  this->SetNumberOfRequiredInputs(2);

  m_QuantileTable.set_size(3, m_NumberOfMatchPoints + 2);
  m_QuantileTable.fill(0);
  m_Gradients.set_size(m_NumberOfMatchPoints + 1);
  m_Gradients.fill(0);
}

/*
 *
 */
template< typename TInputImage, typename TOutputImage, typename THistogramMeasurement >
void
HistogramMatchingImageFilter< TInputImage, TOutputImage, THistogramMeasurement >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "NumberOfHistogramLevels: ";
  os << m_NumberOfHistogramLevels << std::endl;
  os << indent << "NumberOfMatchPoints: ";
  os << m_NumberOfMatchPoints << std::endl;
  os << indent << "ThresholdAtMeanIntensity: ";
  os << m_ThresholdAtMeanIntensity << std::endl;

  os << indent << "SourceIntensityThreshold: ";
  os << m_SourceIntensityThreshold << std::endl;
  os << indent << "ReferenceIntensityThreshold: ";
  os << m_ReferenceIntensityThreshold << std::endl;
  os << indent << "OutputIntensityThreshold: ";
  os << m_ReferenceIntensityThreshold << std::endl;
  os << indent << "Source histogram: ";
  os << m_SourceHistogram.GetPointer() << std::endl;
  os << indent << "Reference histogram: ";
  os << m_ReferenceHistogram.GetPointer() << std::endl;
  os << indent << "Output histogram: ";
  os << m_OutputHistogram.GetPointer() << std::endl;
  os << indent << "QuantileTable: " << std::endl;
  os << m_QuantileTable << std::endl;
  os << indent << "Gradients: " << std::endl;
  os << m_Gradients << std::endl;
  os << indent << "LowerGradient: ";
  os << m_LowerGradient << std::endl;
  os << indent << "UpperGradient: ";
  os << m_UpperGradient << std::endl;
}

/*
 *
 */
template< typename TInputImage, typename TOutputImage, typename THistogramMeasurement >
void
HistogramMatchingImageFilter< TInputImage, TOutputImage, THistogramMeasurement >
::SetReferenceImage(const InputImageType *reference)
{
  this->ProcessObject::SetNthInput( 1,
                                    const_cast< InputImageType * >( reference ) );
}

/*
 *
 */
template< typename TInputImage, typename TOutputImage, typename THistogramMeasurement >
const typename HistogramMatchingImageFilter< TInputImage, TOutputImage, THistogramMeasurement >
::InputImageType *
HistogramMatchingImageFilter< TInputImage, TOutputImage, THistogramMeasurement >
::GetReferenceImage()
{
  return dynamic_cast< TInputImage * >( this->ProcessObject::GetInput(1) );
}

/*
 * This filter requires all of the input images to be
 * in the buffer.
 */
template< typename TInputImage, typename TOutputImage, typename THistogramMeasurement >
void
HistogramMatchingImageFilter< TInputImage, TOutputImage, THistogramMeasurement >
::GenerateInputRequestedRegion()
{
  this->Superclass::GenerateInputRequestedRegion();

  for ( unsigned int idx = 0; idx < this->GetNumberOfIndexedInputs(); ++idx )
    {
    if ( this->GetInput(idx) )
      {
      InputImagePointer image =
        const_cast< InputImageType * >( this->GetInput(idx) );
      image->SetRequestedRegionToLargestPossibleRegion();
      }
    }
}

/**
 *
 */
template< typename TInputImage, typename TOutputImage, typename THistogramMeasurement >
void
HistogramMatchingImageFilter< TInputImage, TOutputImage, THistogramMeasurement >
::BeforeThreadedGenerateData()
{
  unsigned int j;

  InputImageConstPointer source    = this->GetSourceImage();
  InputImageConstPointer reference = this->GetReferenceImage();

  this->ComputeMinMaxMean(source, m_SourceMinValue,
                          m_SourceMaxValue, m_SourceMeanValue);
  this->ComputeMinMaxMean(reference, m_ReferenceMinValue,
                          m_ReferenceMaxValue, m_ReferenceMeanValue);

  if ( m_ThresholdAtMeanIntensity )
    {
    m_SourceIntensityThreshold    = static_cast< InputPixelType >( m_SourceMeanValue );
    m_ReferenceIntensityThreshold = static_cast< InputPixelType >( m_ReferenceMeanValue );
    }
  else
    {
    m_SourceIntensityThreshold    = static_cast< InputPixelType >( m_SourceMinValue );
    m_ReferenceIntensityThreshold = static_cast< InputPixelType >( m_ReferenceMinValue );
    }

  this->ConstructHistogram(source, m_SourceHistogram,
                           m_SourceIntensityThreshold, m_SourceMaxValue);
  this->ConstructHistogram(reference, m_ReferenceHistogram,
                           m_ReferenceIntensityThreshold,
                           m_ReferenceMaxValue);

  // Fill in the quantile table.
  m_QuantileTable.set_size(3, m_NumberOfMatchPoints + 2);
  m_QuantileTable[0][0] = m_SourceIntensityThreshold;
  m_QuantileTable[1][0] = m_ReferenceIntensityThreshold;

  m_QuantileTable[0][m_NumberOfMatchPoints + 1] = m_SourceMaxValue;
  m_QuantileTable[1][m_NumberOfMatchPoints + 1] = m_ReferenceMaxValue;

  double delta = 1.0 / ( double(m_NumberOfMatchPoints) + 1.0 );

  for ( j = 1; j < m_NumberOfMatchPoints + 1; j++ )
    {
    m_QuantileTable[0][j] = m_SourceHistogram->Quantile(
      0, double(j) * delta);
    m_QuantileTable[1][j] = m_ReferenceHistogram->Quantile(
      0, double(j) * delta);
    }

  // Fill in the gradient array.
  m_Gradients.set_size(m_NumberOfMatchPoints + 1);
  double denominator;
  for ( j = 0; j < m_NumberOfMatchPoints + 1; j++ )
    {
    denominator = m_QuantileTable[0][j + 1]
                  - m_QuantileTable[0][j];
    if ( Math::NotAlmostEquals( denominator, 0.0) )
      {
      m_Gradients[j] = m_QuantileTable[1][j + 1]
                       - m_QuantileTable[1][j];
      m_Gradients[j] /= denominator;
      }
    else
      {
      m_Gradients[j] = 0.0;
      }
    }

  denominator = m_QuantileTable[0][0] - m_SourceMinValue;
  if ( Math::NotAlmostEquals( denominator, 0.0 ) )
    {
    m_LowerGradient = m_QuantileTable[1][0] - m_ReferenceMinValue;
    m_LowerGradient /= denominator;
    }
  else
    {
    m_LowerGradient = 0.0;
    }

  denominator = m_QuantileTable[0][m_NumberOfMatchPoints + 1]
                - m_SourceMaxValue;
  if ( Math::NotAlmostEquals( denominator, 0.0 ) )
    {
    m_UpperGradient = m_QuantileTable[1][m_NumberOfMatchPoints + 1]
                      - m_ReferenceMaxValue;
    m_UpperGradient /= denominator;
    }
  else
    {
    m_UpperGradient = 0.0;
    }
}

/**
 *
 */
template< typename TInputImage, typename TOutputImage, typename THistogramMeasurement >
void
HistogramMatchingImageFilter< TInputImage, TOutputImage, THistogramMeasurement >
::AfterThreadedGenerateData()
{
  OutputImagePointer output    = this->GetOutput();

  this->ComputeMinMaxMean(output, m_OutputMinValue,
                          m_OutputMaxValue, m_OutputMeanValue);

  if ( m_ThresholdAtMeanIntensity )
    {
    m_OutputIntensityThreshold    = static_cast< OutputPixelType >( m_OutputMeanValue );
    }
  else
    {
    m_OutputIntensityThreshold    = static_cast< OutputPixelType >( m_OutputMinValue );
    }

  this->ConstructHistogram(output, m_OutputHistogram,
                           m_OutputIntensityThreshold, m_OutputMaxValue);

  // Fill in the quantile table.
  m_QuantileTable[2][0] = m_OutputIntensityThreshold;

  m_QuantileTable[2][m_NumberOfMatchPoints + 1] = m_OutputMaxValue;

  double delta = 1.0 / ( double(m_NumberOfMatchPoints) + 1.0 );

  for ( unsigned int j = 1; j < m_NumberOfMatchPoints + 1; j++ )
    {
    m_QuantileTable[2][j] = m_OutputHistogram->Quantile(
      0, double(j) * delta);
    }
}

/**
 *
 */
template< typename TInputImage, typename TOutputImage, typename THistogramMeasurement >
void
HistogramMatchingImageFilter< TInputImage, TOutputImage, THistogramMeasurement >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  int          i;
  unsigned int j;

  // Get the input and output pointers;
  InputImageConstPointer input  = this->GetInput();
  OutputImagePointer     output = this->GetOutput();

  // Transform the source image and write to output.
  typedef ImageRegionConstIterator< InputImageType > InputConstIterator;
  typedef ImageRegionIterator< OutputImageType >     OutputIterator;

  InputConstIterator inIter(input, outputRegionForThread);
  OutputIterator     outIter(output, outputRegionForThread);

  // support progress methods/callbacks
  SizeValueType updateVisits = 0;
  SizeValueType totalPixels = 0;
  if ( threadId == 0 )
    {
    totalPixels = outputRegionForThread.GetNumberOfPixels();
    updateVisits = totalPixels / 10;
    if ( updateVisits < 1 ) { updateVisits = 1; }
    }

  double srcValue, mappedValue;

  for ( i = 0; !outIter.IsAtEnd(); ++inIter, ++outIter, i++ )
    {
    if ( threadId == 0 && !( i % updateVisits ) )
      {
      this->UpdateProgress( (float)i / (float)totalPixels );
      }

    srcValue = static_cast< double >( inIter.Get() );

    for ( j = 0; j < m_NumberOfMatchPoints + 2; j++ )
      {
      if ( srcValue < m_QuantileTable[0][j] )
        {
        break;
        }
      }

    if ( j == 0 )
      {
      // Linear interpolate from min to point[0]
      mappedValue = m_ReferenceMinValue
                    + ( srcValue - m_SourceMinValue ) * m_LowerGradient;
      }
    else if ( j == m_NumberOfMatchPoints + 2 )
      {
      // Linear interpolate from point[m_NumberOfMatchPoints+1] to max
      mappedValue = m_ReferenceMaxValue
                    + ( srcValue - m_SourceMaxValue ) * m_UpperGradient;
      }
    else
      {
      // Linear interpolate from point[j] and point[j+1].
      mappedValue = m_QuantileTable[1][j - 1]
                    + ( srcValue - m_QuantileTable[0][j - 1] ) * m_Gradients[j - 1];
      }

    outIter.Set( static_cast< OutputPixelType >( mappedValue ) );
    }
}

/**
 * Compute min, max and mean of an image.
 */
template< typename TInputImage, typename TOutputImage, typename THistogramMeasurement >
void
HistogramMatchingImageFilter< TInputImage, TOutputImage, THistogramMeasurement >
::ComputeMinMaxMean(
  const InputImageType *image,
  THistogramMeasurement & minValue,
  THistogramMeasurement & maxValue,
  THistogramMeasurement & meanValue)
{
  typedef ImageRegionConstIterator< InputImageType > ConstIterator;
  ConstIterator iter( image, image->GetBufferedRegion() );

  double        sum = 0.0;
  SizeValueType count = 0;

  minValue = static_cast< THistogramMeasurement >( iter.Get() );
  maxValue = minValue;

  while ( !iter.IsAtEnd() )
    {
    const THistogramMeasurement value = static_cast< THistogramMeasurement >( iter.Get() );
    sum += static_cast< double >( value );

    if ( value < minValue ) { minValue = value; }
    if ( value > maxValue ) { maxValue = value; }

    ++iter;
    ++count;
    }

  meanValue = static_cast< THistogramMeasurement >( sum / static_cast< double >( count ) );
}

/**
 * Construct a histogram from an image.
 */
template< typename TInputImage, typename TOutputImage, typename THistogramMeasurement >
void
HistogramMatchingImageFilter< TInputImage, TOutputImage, THistogramMeasurement >
::ConstructHistogram(
  const InputImageType *image,
  HistogramType  *histogram,
  const THistogramMeasurement minValue,
  const THistogramMeasurement maxValue)
{
  {
    // allocate memory for the histogram
    typename HistogramType::SizeType size;
    typename HistogramType::MeasurementVectorType lowerBound;
    typename HistogramType::MeasurementVectorType upperBound;

    size.SetSize(1);
    lowerBound.SetSize(1);
    upperBound.SetSize(1);
    histogram->SetMeasurementVectorSize(1);

    size[0] = m_NumberOfHistogramLevels;
    lowerBound.Fill(minValue);
    upperBound.Fill(maxValue);

    //Initialize with equally spaced bins.
    histogram->Initialize(size, lowerBound, upperBound);
    histogram->SetToZero();
  }

  typename HistogramType::IndexType index(1);
  typename HistogramType::MeasurementVectorType measurement(1);
  typedef typename HistogramType::MeasurementType MeasurementType;
  measurement[0] = NumericTraits< MeasurementType >::ZeroValue();

    {

    // put each image pixel into the histogram
    typedef ImageRegionConstIterator< InputImageType > ConstIterator;
    ConstIterator iter( image, image->GetBufferedRegion() );

    iter.GoToBegin();
    while ( !iter.IsAtEnd() )
      {
      InputPixelType value = iter.Get();

      if ( static_cast< double >( value ) >= minValue
           && static_cast< double >( value ) <= maxValue )
        {
        // add sample to histogram
        measurement[0] = value;
        histogram->GetIndex( measurement, index );
        histogram->IncreaseFrequencyOfIndex( index, 1 );
        }
      ++iter;
      }
    }
}
} // end namespace itk

#endif
