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
#ifndef itkShiftScaleImageFilter_hxx
#define itkShiftScaleImageFilter_hxx
#include "itkShiftScaleImageFilter.h"

#include "itkImageScanlineIterator.h"
#include "itkNumericTraits.h"
#include "itkTotalProgressReporter.h"

namespace itk
{

template <typename TInputImage, typename TOutputImage>
void
ShiftScaleImageFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  //  reset output variables
  m_UnderflowCount = 0;
  m_OverflowCount = 0;
}


template <typename TInputImage, typename TOutputImage>
void
ShiftScaleImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegion)
{

  const TInputImage * inputPtr = this->GetInput();
  TOutputImage *      outputPtr = this->GetOutput();

  SizeValueType underflow = 0;
  SizeValueType overflow = 0;

  ImageScanlineIterator<TOutputImage>     ot(outputPtr, outputRegion);
  ImageScanlineConstIterator<TInputImage> it(inputPtr, outputRegion);

  // support progress methods/callbacks

  TotalProgressReporter progress(this, outputPtr->GetRequestedRegion().GetNumberOfPixels());

  it.GoToBegin();
  ot.GoToBegin();
  // do the work
  while (!it.IsAtEnd())
  {
    while (!it.IsAtEndOfLine())
    {
      // shift and scale the input pixels
      const RealType value = (static_cast<RealType>(it.Get()) + m_Shift) * m_Scale;
      if (value < NumericTraits<OutputImagePixelType>::NonpositiveMin())
      {
        ot.Set(NumericTraits<OutputImagePixelType>::NonpositiveMin());
        ++underflow;
      }
      else if (value > static_cast<RealType>(NumericTraits<OutputImagePixelType>::max()))
      {
        ot.Set(NumericTraits<OutputImagePixelType>::max());
        ++overflow;
      }
      else
      {
        ot.Set(static_cast<OutputImagePixelType>(value));
      }
      ++it;
      ++ot;
    }
    it.NextLine();
    ot.NextLine();
    progress.Completed(outputRegion.GetSize()[0]);
  }

  std::lock_guard<std::mutex> mutexHolder(m_Mutex);
  m_OverflowCount += overflow;
  m_UnderflowCount += underflow;
}

template <typename TInputImage, typename TOutputImage>
void
ShiftScaleImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Shift: " << m_Shift << std::endl;
  os << indent << "Scale: " << m_Scale << std::endl;
  os << indent << "Computed values follow:" << std::endl;
  os << indent << "UnderflowCount: " << m_UnderflowCount << std::endl;
  os << indent << "OverflowCount: " << m_OverflowCount << std::endl;
}
} // end namespace itk
#endif
