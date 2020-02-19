/*=========================================================================
 *
 *  Copyright NumFOCUS
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
template <typename TInputImage, typename TOutputImage, typename THistogramMeasurement>
HistogramMatchingImageFilter<TInputImage, TOutputImage, THistogramMeasurement>::HistogramMatchingImageFilter()
  : m_SourceMinValue(NumericTraits<THistogramMeasurement>::ZeroValue())
  , m_SourceMaxValue(NumericTraits<THistogramMeasurement>::ZeroValue())
  , m_ReferenceMinValue(NumericTraits<THistogramMeasurement>::ZeroValue())
  , m_ReferenceMaxValue(NumericTraits<THistogramMeasurement>::ZeroValue())
  , m_SourceHistogram(HistogramType::New())
  , m_OutputHistogram(HistogramType::New())

{
  this->SetNumberOfRequiredInputs(1);
  Self::SetPrimaryInputName("SourceImage");
  Self::AddOptionalInputName("ReferenceImage", 1);
  Self::AddOptionalInputName("ReferenceHistogram", 2);

  m_QuantileTable.set_size(3, m_NumberOfMatchPoints + 2);
  m_QuantileTable.fill(0);
  m_Gradients.set_size(m_NumberOfMatchPoints + 1);
  m_Gradients.fill(0);
  this->DynamicMultiThreadingOn();
}


template <typename TInputImage, typename TOutputImage, typename THistogramMeasurement>
void
HistogramMatchingImageFilter<TInputImage, TOutputImage, THistogramMeasurement>::PrintSelf(std::ostream & os,
                                                                                          Indent         indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "NumberOfHistogramLevels: ";
  os << m_NumberOfHistogramLevels << std::endl;
  os << indent << "NumberOfMatchPoints: ";
  os << m_NumberOfMatchPoints << std::endl;
  os << indent << "ThresholdAtMeanIntensity: ";
  os << m_ThresholdAtMeanIntensity << std::endl;

  os << indent << "Source histogram: ";
  os << m_SourceHistogram.GetPointer() << std::endl;
  os << indent << "Reference histogram: ";
  os << this->GetReferenceHistogram() << std::endl;
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
  os << indent << "GenerateReferenceHistogramFromImage:";
  os << m_GenerateReferenceHistogramFromImage << std::endl;
}

/*
 * This filter requires all of the input images to be
 * in the buffer.
 */
template <typename TInputImage, typename TOutputImage, typename THistogramMeasurement>
void
HistogramMatchingImageFilter<TInputImage, TOutputImage, THistogramMeasurement>::GenerateInputRequestedRegion()
{
  this->Superclass::GenerateInputRequestedRegion();
  {
    auto * source_image = const_cast<InputImageType *>(this->GetSourceImage());
    if (source_image)
    {
      source_image->SetRequestedRegionToLargestPossibleRegion();
    }
  }

  if (this->m_GenerateReferenceHistogramFromImage)
  {
    auto * reference_image = const_cast<InputImageType *>(this->GetReferenceImage());
    if (reference_image)
    {
      reference_image->SetRequestedRegionToLargestPossibleRegion();
    }
  }
}


template <typename TInputImage, typename TOutputImage, typename THistogramMeasurement>
void
HistogramMatchingImageFilter<TInputImage, TOutputImage, THistogramMeasurement>::VerifyPreconditions() ITKv5_CONST
{
  Superclass::VerifyPreconditions();

  if (m_GenerateReferenceHistogramFromImage)
  {
    if (this->GetReferenceImage() == nullptr)
    {
      itkExceptionMacro(<< "ReferenceImage required when GenerateReferenceHistogramFromImage is true.")
    }
  }
  else
  {
    if (this->GetReferenceHistogram() == nullptr)
    {
      itkExceptionMacro(<< "ReferenceHistogram required when GenerateReferenceHistogramFromImage is false.")
    }
  }
}


template <typename TInputImage, typename TOutputImage, typename THistogramMeasurement>
void
HistogramMatchingImageFilter<TInputImage, TOutputImage, THistogramMeasurement>::BeforeThreadedGenerateData()
{
  THistogramMeasurement sourceMeanValue;
  THistogramMeasurement referenceMeanValue;

  InputPixelType sourceIntensityThreshold;
  InputPixelType referenceIntensityThreshold;

  if (m_GenerateReferenceHistogramFromImage)
  {
    InputImageConstPointer reference = this->GetReferenceImage();
    if (reference.IsNull())
    {
      itkExceptionMacro(<< "ERROR: ReferenceImage required when GenerateReferenceHistogramFromImage is true.\n")
    }
    this->ComputeMinMaxMean(reference, m_ReferenceMinValue, m_ReferenceMaxValue, referenceMeanValue);
    if (m_ThresholdAtMeanIntensity)
    {
      referenceIntensityThreshold = static_cast<InputPixelType>(referenceMeanValue);
    }
    else
    {
      referenceIntensityThreshold = static_cast<InputPixelType>(m_ReferenceMinValue);
    }
    {
      HistogramPointer tempHistptr = HistogramType::New();
      this->ConstructHistogramFromIntensityRange(reference,
                                                 tempHistptr,
                                                 referenceIntensityThreshold,
                                                 m_ReferenceMaxValue,
                                                 m_ReferenceMinValue,
                                                 m_ReferenceMaxValue);
      this->SetReferenceHistogram(tempHistptr);
    }
  }
  else
  {
    const HistogramType * const referenceHistogram = this->GetReferenceHistogram();
    if (referenceHistogram == nullptr)
    {
      itkExceptionMacro(<< "ERROR: ReferenceHistogram required when GenerateReferenceHistogramFromImage is false.\n")
    }

    // If the reference histogram is provided, then extract summary statistics
    // directly from the histogram.
    const auto & allReferenceMinsByDimension = referenceHistogram->GetMins();        // Array of dimensions
    const auto & allReferenceMinsFirstDimension = allReferenceMinsByDimension.at(0); // Mins for dimension 0
    m_ReferenceMinValue = allReferenceMinsFirstDimension.at(0);                      // First element of mins
    const auto & allReferenceMaxsByDimension = referenceHistogram->GetMaxs();        // Array of dimensions
    const auto & allReferenceMaxsFirstDimension = allReferenceMaxsByDimension.at(0); // Maxes for dimension 0
    m_ReferenceMaxValue =
      allReferenceMaxsFirstDimension.at(allReferenceMaxsFirstDimension.size() - 1); // last element of Maxes

    if (m_ThresholdAtMeanIntensity)
    {
      referenceIntensityThreshold = allReferenceMinsFirstDimension.at(0); // First element of mins array in histogram
    }
    else
    {
      referenceIntensityThreshold = static_cast<InputPixelType>(m_ReferenceMinValue);
    }
  }

  InputImageConstPointer source = this->GetSourceImage();

  this->ComputeMinMaxMean(source, m_SourceMinValue, m_SourceMaxValue, sourceMeanValue);

  if (m_ThresholdAtMeanIntensity)
  {
    sourceIntensityThreshold = static_cast<InputPixelType>(sourceMeanValue);
  }
  else
  {
    sourceIntensityThreshold = static_cast<InputPixelType>(m_SourceMinValue);
  }
  this->ConstructHistogramFromIntensityRange(
    source, m_SourceHistogram, sourceIntensityThreshold, m_SourceMaxValue, m_SourceMinValue, m_SourceMaxValue);

  // Fill in the quantile table.
  m_QuantileTable.set_size(3, m_NumberOfMatchPoints + 2);
  m_QuantileTable[0][0] = sourceIntensityThreshold;
  m_QuantileTable[1][0] = referenceIntensityThreshold;

  m_QuantileTable[0][m_NumberOfMatchPoints + 1] = m_SourceMaxValue;
  m_QuantileTable[1][m_NumberOfMatchPoints + 1] = m_ReferenceMaxValue;

  {
    const double                delta = 1.0 / (static_cast<double>(m_NumberOfMatchPoints) + 1.0);
    const HistogramType * const referenceHistogram = this->GetReferenceHistogram();
    for (SizeValueType j = 1; j < m_NumberOfMatchPoints + 1; j++)
    {
      m_QuantileTable[0][j] = m_SourceHistogram->Quantile(0, static_cast<double>(j) * delta);
      m_QuantileTable[1][j] = referenceHistogram->Quantile(0, static_cast<double>(j) * delta);
    }
  }

  // Fill in the gradient array.
  m_Gradients.set_size(m_NumberOfMatchPoints + 1);
  for (SizeValueType j = 0; j < m_NumberOfMatchPoints + 1; j++)
  {
    const double denominator = m_QuantileTable[0][j + 1] - m_QuantileTable[0][j];
    if (Math::NotAlmostEquals(denominator, 0.0))
    {
      m_Gradients[j] = m_QuantileTable[1][j + 1] - m_QuantileTable[1][j];
      m_Gradients[j] /= denominator;
    }
    else
    {
      m_Gradients[j] = 0.0;
    }
  }
  {
    const double denominator = m_QuantileTable[0][0] - m_SourceMinValue;
    if (Math::NotAlmostEquals(denominator, 0.0))
    {
      m_LowerGradient = m_QuantileTable[1][0] - m_ReferenceMinValue;
      m_LowerGradient /= denominator;
    }
    else
    {
      m_LowerGradient = 0.0;
    }
  }
  {
    const double denominator = m_QuantileTable[0][m_NumberOfMatchPoints + 1] - m_SourceMaxValue;
    if (Math::NotAlmostEquals(denominator, 0.0))
    {
      m_UpperGradient = m_QuantileTable[1][m_NumberOfMatchPoints + 1] - m_ReferenceMaxValue;
      m_UpperGradient /= denominator;
    }
    else
    {
      m_UpperGradient = 0.0;
    }
  }
}


template <typename TInputImage, typename TOutputImage, typename THistogramMeasurement>
void
HistogramMatchingImageFilter<TInputImage, TOutputImage, THistogramMeasurement>::AfterThreadedGenerateData()
{
  THistogramMeasurement outputMeanValue;
  THistogramMeasurement outputMinValue;
  THistogramMeasurement outputMaxValue;

  OutputPixelType outputIntensityThreshold;

  OutputImagePointer output = this->GetOutput();

  this->ComputeMinMaxMean(output, outputMinValue, outputMaxValue, outputMeanValue);

  if (m_ThresholdAtMeanIntensity)
  {
    outputIntensityThreshold = static_cast<OutputPixelType>(outputMeanValue);
  }
  else
  {
    outputIntensityThreshold = static_cast<OutputPixelType>(outputMinValue);
  }

  this->ConstructHistogramFromIntensityRange(
    output, m_OutputHistogram, outputIntensityThreshold, outputMaxValue, outputMinValue, outputMaxValue);

  // Fill in the quantile table.
  m_QuantileTable[2][0] = outputIntensityThreshold;

  m_QuantileTable[2][m_NumberOfMatchPoints + 1] = outputMaxValue;

  const double delta = 1.0 / (static_cast<double>(m_NumberOfMatchPoints) + 1.0);

  for (SizeValueType j = 1; j < m_NumberOfMatchPoints + 1; j++)
  {
    m_QuantileTable[2][j] = m_OutputHistogram->Quantile(0, static_cast<double>(j) * delta);
  }
}


template <typename TInputImage, typename TOutputImage, typename THistogramMeasurement>
void
HistogramMatchingImageFilter<TInputImage, TOutputImage, THistogramMeasurement>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  InputImageConstPointer input = this->GetSourceImage();
  OutputImagePointer     output = this->GetOutput();

  // Transform the source image and write to output.
  using InputConstIterator = ImageRegionConstIterator<InputImageType>;
  using OutputIterator = ImageRegionIterator<OutputImageType>;

  InputConstIterator inIter(input, outputRegionForThread);
  OutputIterator     outIter(output, outputRegionForThread);

  for (SizeValueType i = 0; !outIter.IsAtEnd(); ++inIter, ++outIter, i++)
  {
    const auto    srcValue = static_cast<double>(inIter.Get());
    SizeValueType j = 0;
    for (; j < m_NumberOfMatchPoints + 2; j++)
    {
      if (srcValue < m_QuantileTable[0][j])
      {
        break;
      }
    }

    double mappedValue;
    if (j == 0)
    {
      // Linear interpolate from min to point[0]
      mappedValue = m_ReferenceMinValue + (srcValue - m_SourceMinValue) * m_LowerGradient;
    }
    else if (j == m_NumberOfMatchPoints + 2)
    {
      // Linear interpolate from point[m_NumberOfMatchPoints+1] to max
      mappedValue = m_ReferenceMaxValue + (srcValue - m_SourceMaxValue) * m_UpperGradient;
    }
    else
    {
      // Linear interpolate from point[j] and point[j+1].
      mappedValue = m_QuantileTable[1][j - 1] + (srcValue - m_QuantileTable[0][j - 1]) * m_Gradients[j - 1];
    }

    outIter.Set(static_cast<OutputPixelType>(mappedValue));
  }
}

/**
 * Compute min, max and mean of an image.
 */
template <typename TInputImage, typename TOutputImage, typename THistogramMeasurement>
void
HistogramMatchingImageFilter<TInputImage, TOutputImage, THistogramMeasurement>::ComputeMinMaxMean(
  const InputImageType *  image,
  THistogramMeasurement & minValue,
  THistogramMeasurement & maxValue,
  THistogramMeasurement & meanValue)
{
  using ConstIterator = ImageRegionConstIterator<InputImageType>;
  ConstIterator iter(image, image->GetBufferedRegion());

  double        sum = 0.0;
  SizeValueType count = 0;

  minValue = static_cast<THistogramMeasurement>(iter.Get());
  maxValue = minValue;

  while (!iter.IsAtEnd())
  {
    const auto value = static_cast<THistogramMeasurement>(iter.Get());
    sum += static_cast<double>(value);

    if (value < minValue)
    {
      minValue = value;
    }
    if (value > maxValue)
    {
      maxValue = value;
    }

    ++iter;
    ++count;
  }

  meanValue = static_cast<THistogramMeasurement>(sum / static_cast<double>(count));
}

template <typename TInputImage, typename TOutputImage, typename THistogramMeasurement>
void
HistogramMatchingImageFilter<TInputImage, TOutputImage, THistogramMeasurement>::ConstructHistogramFromIntensityRange(
  const InputImageType *      image,
  HistogramType *             histogram,
  const THistogramMeasurement minHistogramValidValue,
  const THistogramMeasurement maxHistogramValidValue,
  const THistogramMeasurement imageTrueMinValue,
  const THistogramMeasurement imageTrueMaxValue)
{
  {
    // allocate memory for the histogram
    typename HistogramType::SizeType              size;
    typename HistogramType::MeasurementVectorType lowerBound;
    typename HistogramType::MeasurementVectorType upperBound;

    size.SetSize(1);
    lowerBound.SetSize(1);
    upperBound.SetSize(1);
    histogram->SetMeasurementVectorSize(1);

    size[0] = m_NumberOfHistogramLevels;
    lowerBound.Fill(minHistogramValidValue);
    upperBound.Fill(maxHistogramValidValue);

    // Initialize with equally spaced bins withing the valid region.
    histogram->Initialize(size, lowerBound, upperBound);

    // Now expand the first and last bin to represent the true reference image range
    histogram->SetBinMin(0, 0, imageTrueMinValue);
    histogram->SetBinMax(0, m_NumberOfHistogramLevels - 1, imageTrueMaxValue);
    histogram->SetToZero();
  }

  typename HistogramType::IndexType             index(1);
  typename HistogramType::MeasurementVectorType measurement(1);
  using MeasurementType = typename HistogramType::MeasurementType;
  measurement[0] = NumericTraits<MeasurementType>::ZeroValue();

  {
    // put each image pixel into the histogram
    using ConstIterator = ImageRegionConstIterator<InputImageType>;
    ConstIterator iter(image, image->GetBufferedRegion());

    iter.GoToBegin();
    while (!iter.IsAtEnd())
    {
      const InputPixelType & value = iter.Value();

      if (static_cast<double>(value) >= minHistogramValidValue && static_cast<double>(value) <= maxHistogramValidValue)
      {
        // add sample to histogram
        measurement[0] = value;
        const bool is_inside_histogram = histogram->GetIndex(measurement, index);
        if (is_inside_histogram)
        {
          histogram->IncreaseFrequencyOfIndex(index, 1);
        }
      }
      ++iter;
    }
  }
}
} // end namespace itk

#endif
