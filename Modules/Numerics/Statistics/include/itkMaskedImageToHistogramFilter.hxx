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
#ifndef itkMaskedImageToHistogramFilter_hxx
#define itkMaskedImageToHistogramFilter_hxx

#include "itkMaskedImageToHistogramFilter.h"
#include "itkProgressReporter.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{
namespace Statistics
{
template <typename TImage, typename TMaskImage>
MaskedImageToHistogramFilter<TImage, TMaskImage>::MaskedImageToHistogramFilter()
{
  Self::AddRequiredInputName("MaskImage");
  this->SetMaskValue(NumericTraits<MaskPixelType>::max());
}

template <typename TImage, typename TMaskImage>
void
MaskedImageToHistogramFilter<TImage, TMaskImage>::ThreadedComputeMinimumAndMaximum(
  const RegionType & inputRegionForThread)
{
  unsigned int                   nbOfComponents = this->GetInput()->GetNumberOfComponentsPerPixel();
  HistogramMeasurementVectorType min(nbOfComponents);
  HistogramMeasurementVectorType max(nbOfComponents);

  MaskPixelType maskValue = this->GetMaskValue();

  ImageRegionConstIterator<TImage>     inputIt(this->GetInput(), inputRegionForThread);
  ImageRegionConstIterator<TMaskImage> maskIt(this->GetMaskImage(), inputRegionForThread);
  inputIt.GoToBegin();
  maskIt.GoToBegin();
  HistogramMeasurementVectorType m(nbOfComponents);

  min.Fill(NumericTraits<ValueType>::max());
  max.Fill(NumericTraits<ValueType>::NonpositiveMin());
  while (!inputIt.IsAtEnd())
  {
    if (maskIt.Get() == maskValue)
    {
      const PixelType & p = inputIt.Get();
      NumericTraits<PixelType>::AssignToArray(p, m);
      for (unsigned int i = 0; i < nbOfComponents; i++)
      {
        min[i] = std::min(m[i], min[i]);
        max[i] = std::max(m[i], max[i]);
      }
    }
    ++inputIt;
    ++maskIt;
  }
  std::lock_guard<std::mutex> mutexHolder(this->m_Mutex);
  for (unsigned int i = 0; i < nbOfComponents; i++)
  {
    this->m_Minimum[i] = std::min(this->m_Minimum[i], min[i]);
    this->m_Maximum[i] = std::max(this->m_Maximum[i], max[i]);
  }
}

template <typename TImage, typename TMaskImage>
void
MaskedImageToHistogramFilter<TImage, TMaskImage>::ThreadedStreamedGenerateData(const RegionType & inputRegionForThread)
{
  const unsigned int    nbOfComponents = this->GetInput()->GetNumberOfComponentsPerPixel();
  const HistogramType * outputHistogram = this->GetOutput();

  HistogramPointer histogram = HistogramType::New();
  histogram->SetClipBinsAtEnds(outputHistogram->GetClipBinsAtEnds());
  histogram->SetMeasurementVectorSize(nbOfComponents);
  histogram->Initialize(outputHistogram->GetSize(), this->m_Minimum, this->m_Maximum);

  ImageRegionConstIterator<TImage>     inputIt(this->GetInput(), inputRegionForThread);
  ImageRegionConstIterator<TMaskImage> maskIt(this->GetMaskImage(), inputRegionForThread);
  inputIt.GoToBegin();
  maskIt.GoToBegin();
  HistogramMeasurementVectorType m(nbOfComponents);
  MaskPixelType                  maskValue = this->GetMaskValue();

  typename HistogramType::IndexType index;
  while (!inputIt.IsAtEnd())
  {
    if (maskIt.Get() == maskValue)
    {
      const PixelType & p = inputIt.Get();
      NumericTraits<PixelType>::AssignToArray(p, m);
      histogram->GetIndex(m, index);
      histogram->IncreaseFrequencyOfIndex(index, 1);
    }
    ++inputIt;
    ++maskIt;
  }


  this->ThreadedMergeHistogram(std::move(histogram));
}

} // end of namespace Statistics
} // end of namespace itk

#endif
