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

#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
ShiftScaleImageFilter<TInputImage, TOutputImage>::ShiftScaleImageFilter()
{
  m_Shift = NumericTraits<RealType>::ZeroValue();
  m_Scale = NumericTraits<RealType>::OneValue();
  m_UnderflowCount = 0;
  m_OverflowCount = 0;
  m_ThreadUnderflow.SetSize(1);
  m_ThreadOverflow.SetSize(1);
  m_InputImage = nullptr;
  m_OutputImage = nullptr;
  this->DynamicMultiThreadingOff();
}

template <typename TInputImage, typename TOutputImage>
void
ShiftScaleImageFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  ThreadIdType numberOfThreads = this->GetNumberOfWorkUnits();

  //  Allocate and initialize the thread temporaries
  m_ThreadUnderflow.SetSize(numberOfThreads);
  m_ThreadUnderflow.Fill(0);
  m_ThreadOverflow.SetSize(numberOfThreads);
  m_ThreadOverflow.Fill(0);
  m_InputImage = this->GetInput();
  m_OutputImage = this->GetOutput();
}

template <typename TInputImage, typename TOutputImage>
void
ShiftScaleImageFilter<TInputImage, TOutputImage>::AfterThreadedGenerateData()
{
  ThreadIdType numberOfThreads = this->GetNumberOfWorkUnits();

  m_UnderflowCount = 0;
  m_OverflowCount = 0;

  // Accumulate counts for each thread
  for (ThreadIdType i = 0; i < numberOfThreads; i++)
  {
    m_UnderflowCount += m_ThreadUnderflow[i];
    m_OverflowCount += m_ThreadOverflow[i];
  }
}

template <typename TInputImage, typename TOutputImage>
void
ShiftScaleImageFilter<TInputImage, TOutputImage>::ThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread,
  ThreadIdType                  threadId)
{

  ImageRegionConstIterator<TInputImage> it(this->m_InputImage, outputRegionForThread);
  ImageRegionIterator<TOutputImage>     ot(this->m_OutputImage, outputRegionForThread);

  // support progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());

  // shift and scale the input pixels
  while (!it.IsAtEnd())
  {
    const RealType value = (static_cast<RealType>(it.Get()) + m_Shift) * m_Scale;
    if (value < NumericTraits<OutputImagePixelType>::NonpositiveMin())
    {
      ot.Set(NumericTraits<OutputImagePixelType>::NonpositiveMin());
      m_ThreadUnderflow[threadId]++;
    }
    else if (value > static_cast<RealType>(NumericTraits<OutputImagePixelType>::max()))
    {
      ot.Set(NumericTraits<OutputImagePixelType>::max());
      m_ThreadOverflow[threadId]++;
    }
    else
    {
      ot.Set(static_cast<OutputImagePixelType>(value));
    }
    ++it;
    ++ot;

    progress.CompletedPixel();
  }
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
