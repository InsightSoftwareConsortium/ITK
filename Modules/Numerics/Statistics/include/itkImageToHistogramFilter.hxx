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
#ifndef itkImageToHistogramFilter_hxx
#define itkImageToHistogramFilter_hxx

#include "itkImageToHistogramFilter.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{
namespace Statistics
{
template <typename TImage>
ImageToHistogramFilter<TImage>::ImageToHistogramFilter()
{
  this->SetNumberOfRequiredInputs(1);
  this->SetNumberOfRequiredOutputs(1);

  this->ProcessObject::SetNthOutput(0, this->MakeOutput(0));

  // same default values as in the HistogramGenerator

  this->Self::SetMarginalScale(100);

  if (typeid(ValueType) == typeid(signed char) || typeid(ValueType) == typeid(unsigned char))
  {
    this->Self::SetAutoMinimumMaximum(false);
  }
  else
  {
    this->Self::SetAutoMinimumMaximum(true);
  }
}

template <typename TImage>
DataObject::Pointer
ImageToHistogramFilter<TImage>::MakeOutput(DataObjectPointerArraySizeType itkNotUsed(idx))
{
  return HistogramType::New().GetPointer();
}

template <typename TImage>
const typename ImageToHistogramFilter<TImage>::HistogramType *
ImageToHistogramFilter<TImage>::GetOutput() const
{
  auto * output = itkDynamicCastInDebugMode<const HistogramType *>(this->ProcessObject::GetPrimaryOutput());

  return output;
}

template <typename TImage>
typename ImageToHistogramFilter<TImage>::HistogramType *
ImageToHistogramFilter<TImage>::GetOutput()
{

  auto * output = itkDynamicCastInDebugMode<HistogramType *>(this->ProcessObject::GetPrimaryOutput());

  return output;
}


template <typename TImage>
void
ImageToHistogramFilter<TImage>::GraftOutput(DataObject * graft)
{
  DataObject * output = const_cast<HistogramType *>(this->GetOutput());

  // Call Histogram to copy meta-information, and the container
  output->Graft(graft);
}


template <typename TImage>
unsigned int
ImageToHistogramFilter<TImage>::GetNumberOfInputRequestedRegions()
{
  // If we need to compute the minimum and maximum we don't stream
  if (this->GetAutoMinimumMaximumInput() && this->GetAutoMinimumMaximum())
  {
    return 1;
  }

  return Superclass::GetNumberOfInputRequestedRegions();
}

template <typename TImage>
void
ImageToHistogramFilter<TImage>::StreamedGenerateData(unsigned int inputRequestedRegionNumber)
{
  if (inputRequestedRegionNumber == 0)
  {
    this->InitializeOutputHistogram();
  }

  Superclass::StreamedGenerateData(inputRequestedRegionNumber);
}


template <typename TImage>
void
ImageToHistogramFilter<TImage>::InitializeOutputHistogram()
{
  const unsigned int nbOfComponents = this->GetInput()->GetNumberOfComponentsPerPixel();
  m_Minimum = HistogramMeasurementVectorType(nbOfComponents);
  m_Maximum = HistogramMeasurementVectorType(nbOfComponents);

  m_Minimum.Fill(NumericTraits<ValueType>::max());
  m_Maximum.Fill(NumericTraits<ValueType>::NonpositiveMin());

  m_MergeHistogram = nullptr;

  HistogramType * outputHistogram = this->GetOutput();
  outputHistogram->SetClipBinsAtEnds(true);

  // the parameter needed to initialize the histogram
  HistogramSizeType size(nbOfComponents);
  if (this->GetHistogramSizeInput())
  {
    // user provided value
    size = this->GetHistogramSize();
  }
  else
  {
    // use a default value, which must be computed at run time for the VectorImage
    size.Fill(256);
  }

  if (this->GetAutoMinimumMaximumInput() && this->GetAutoMinimumMaximum())
  {
    if (this->GetInput()->GetBufferedRegion() != this->GetInput()->GetLargestPossibleRegion())
    {
      itkExceptionMacro(<< "AutoMinimumMaximumInput is not supported with streaming.")
    }

    // we have to compute the minimum and maximum values
    this->GetMultiThreader()->template ParallelizeImageRegion<ImageType::ImageDimension>(
      this->GetInput()->GetBufferedRegion(),
      [this](const RegionType & inputRegionForThread) { this->ThreadedComputeMinimumAndMaximum(inputRegionForThread); },
      this);


    this->ApplyMarginalScale(m_Minimum, m_Maximum, size);
  }
  else
  {
    if (this->GetHistogramBinMinimumInput())
    {
      m_Minimum = this->GetHistogramBinMinimum();
    }
    else
    {
      m_Minimum.Fill(NumericTraits<ValueType>::NonpositiveMin() - 0.5);
    }
    if (this->GetHistogramBinMaximumInput())
    {
      m_Maximum = this->GetHistogramBinMaximum();
    }
    else
    {
      m_Maximum.Fill(NumericTraits<ValueType>::max() + 0.5);
    }
    // No marginal scaling is applied in this case
  }

  outputHistogram->SetMeasurementVectorSize(nbOfComponents);
  outputHistogram->Initialize(size, m_Minimum, m_Maximum);
}


template <typename TImage>
void
ImageToHistogramFilter<TImage>::AfterStreamedGenerateData()
{
  Superclass::AfterStreamedGenerateData();

  HistogramType * outputHistogram = this->GetOutput();
  outputHistogram->Graft(m_MergeHistogram);
  m_MergeHistogram = nullptr;
}


template <typename TImage>
void
ImageToHistogramFilter<TImage>::ThreadedComputeMinimumAndMaximum(const RegionType & inputRegionForThread)
{
  const unsigned int             nbOfComponents = this->GetInput()->GetNumberOfComponentsPerPixel();
  HistogramMeasurementVectorType min(nbOfComponents);
  HistogramMeasurementVectorType max(nbOfComponents);

  ImageRegionConstIterator<TImage> inputIt(this->GetInput(), inputRegionForThread);
  inputIt.GoToBegin();
  HistogramMeasurementVectorType m(nbOfComponents);

  min.Fill(NumericTraits<ValueType>::max());
  max.Fill(NumericTraits<ValueType>::NonpositiveMin());
  while (!inputIt.IsAtEnd())
  {
    const PixelType & p = inputIt.Get();
    NumericTraits<PixelType>::AssignToArray(p, m);
    for (unsigned int i = 0; i < nbOfComponents; i++)
    {
      min[i] = std::min(m[i], min[i]);
      max[i] = std::max(m[i], max[i]);
    }
    ++inputIt;
  }

  std::lock_guard<std::mutex> mutexHolder(m_Mutex);
  for (unsigned int i = 0; i < nbOfComponents; i++)
  {
    m_Minimum[i] = std::min(m_Minimum[i], min[i]);
    m_Maximum[i] = std::max(m_Maximum[i], max[i]);
  }
}

template <typename TImage>
void
ImageToHistogramFilter<TImage>::ThreadedStreamedGenerateData(const RegionType & inputRegionForThread)
{
  const unsigned int    nbOfComponents = this->GetInput()->GetNumberOfComponentsPerPixel();
  const HistogramType * outputHistogram = this->GetOutput();

  HistogramPointer histogram = HistogramType::New();
  histogram->SetClipBinsAtEnds(outputHistogram->GetClipBinsAtEnds());
  histogram->SetMeasurementVectorSize(nbOfComponents);
  histogram->Initialize(outputHistogram->GetSize(), m_Minimum, m_Maximum);


  ImageRegionConstIterator<TImage> inputIt(this->GetInput(), inputRegionForThread);
  inputIt.GoToBegin();
  HistogramMeasurementVectorType m(nbOfComponents);

  typename HistogramType::IndexType index;
  while (!inputIt.IsAtEnd())
  {
    const PixelType & p = inputIt.Get();
    NumericTraits<PixelType>::AssignToArray(p, m);
    histogram->GetIndex(m, index);
    histogram->IncreaseFrequencyOfIndex(index, 1);
    ++inputIt;
  }

  this->ThreadedMergeHistogram(std::move(histogram));
}

template <typename TImage>
void
ImageToHistogramFilter<TImage>::ThreadedMergeHistogram(HistogramPointer && histogram)
{
  while (true)
  {

    std::unique_lock<std::mutex> lock(m_Mutex);

    if (m_MergeHistogram.IsNull())
    {
      m_MergeHistogram = std::move(histogram);
      return;
    }
    else
    {

      // merge/reduce the local results with current values in m_MergeHistogram

      // take ownership locally
      HistogramPointer tomergeHistogram;
      swap(m_MergeHistogram, tomergeHistogram);

      // allow other threads to merge data
      lock.unlock();

      using HistogramIterator = typename HistogramType::ConstIterator;

      HistogramIterator hit = tomergeHistogram->Begin();
      HistogramIterator end = tomergeHistogram->End();

      typename HistogramType::IndexType index;

      while (hit != end)
      {
        histogram->GetIndex(hit.GetMeasurementVector(), index);
        histogram->IncreaseFrequencyOfIndex(index, hit.GetFrequency());
        ++hit;
      }
    }
  }
}

template <typename TImage>
void
ImageToHistogramFilter<TImage>::ApplyMarginalScale(HistogramMeasurementVectorType & min,
                                                   HistogramMeasurementVectorType & max,
                                                   HistogramSizeType &              size)
{
  const unsigned int nbOfComponents = this->GetInput()->GetNumberOfComponentsPerPixel();
  bool               clipHistograms = true;
  for (unsigned int i = 0; i < nbOfComponents; i++)
  {
    if (!NumericTraits<HistogramMeasurementType>::is_integer)
    {
      HistogramMeasurementType marginalScale = this->GetMarginalScale();
      const double             margin =
        (static_cast<HistogramMeasurementType>(max[i] - min[i]) / static_cast<HistogramMeasurementType>(size[i])) /
        static_cast<HistogramMeasurementType>(marginalScale);

      // Now we check if the max[i] value can be increased by
      // the margin value without saturating the capacity of the
      // HistogramMeasurementType
      if ((NumericTraits<HistogramMeasurementType>::max() - max[i]) > margin)
      {
        max[i] = static_cast<HistogramMeasurementType>(max[i] + margin);
      }
      else
      {
        // an overflow would occur if we add 'margin' to the max
        // therefore we just compromise in setting max = max.
        // Histogram measurement type would force the clipping the max
        // value.
        // Therefore we must call the following to include the max value:
        clipHistograms = false;
        // The above function is okay since here we are within the
        // autoMinMax
        // computation and clearly the user intended to include min and max.
      }
    }
    else
    {
      // max[i] = SafeAssign(max[i] + NumericTraits<MeasurementType>::OneValue());
      // if ( max[i] <= max[i] )
      if (max[i] < (static_cast<ValueType>(NumericTraits<HistogramMeasurementType>::max()) -
                    NumericTraits<ValueType>::OneValue()))
      {
        max[i] = static_cast<HistogramMeasurementType>(max[i] + NumericTraits<ValueType>::OneValue());
      }
      else
      {
        // an overflow would have occurred, therefore set max to max
        // Histogram measurement type would force the clipping the max
        // value.
        // Therefore we must call the following to include the max value:
        clipHistograms = false;
        // The above function is okay since here we are within the
        // autoMinMax
        // computation and clearly the user intended to include min and max.
      }
    }
  }
  if (clipHistograms == false)
  {
    this->GetOutput()->SetClipBinsAtEnds(false);
  }
}

template <typename TImage>
void
ImageToHistogramFilter<TImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  if (this->GetHistogramBinMinimumInput())
  {
    os << indent << "HistogramBinMinimum: " << this->GetHistogramBinMinimum() << std::endl;
  }
  if (this->GetHistogramBinMaximumInput())
  {
    os << indent << "HistogramBinMaximum: " << this->GetHistogramBinMaximum() << std::endl;
  }
  os << indent << "MarginalScale: " << this->GetMarginalScale() << std::endl;
  os << indent << "AutoMinimumMaximum: " << this->GetAutoMinimumMaximum() << std::endl;
  if (this->GetHistogramSizeInput())
  {
    os << indent << "HistogramSize: " << this->GetHistogramSize() << std::endl;
  }
}
} // end of namespace Statistics
} // end of namespace itk

#endif
