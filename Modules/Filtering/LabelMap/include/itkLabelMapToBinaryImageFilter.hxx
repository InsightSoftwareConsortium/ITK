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
#ifndef itkLabelMapToBinaryImageFilter_hxx
#define itkLabelMapToBinaryImageFilter_hxx

#include "itkLabelMapToBinaryImageFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkImageRegionIterator.h"
#include "itkProgressTransformer.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
LabelMapToBinaryImageFilter<TInputImage, TOutputImage>::LabelMapToBinaryImageFilter()
{
  this->m_BackgroundValue = NumericTraits<OutputImagePixelType>::NonpositiveMin();
  this->m_ForegroundValue = NumericTraits<OutputImagePixelType>::max();
  this->DynamicMultiThreadingOn();
}

template <typename TInputImage, typename TOutputImage>
void
LabelMapToBinaryImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());

  if (input)
  {
    input->SetRequestedRegion(input->GetLargestPossibleRegion());
  }
}

template <typename TInputImage, typename TOutputImage>
void
LabelMapToBinaryImageFilter<TInputImage, TOutputImage>::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()->SetRequestedRegion(this->GetOutput()->GetLargestPossibleRegion());
}

template <typename TInputImage, typename TOutputImage>
void
LabelMapToBinaryImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  this->UpdateProgress(0.0f);
  this->AllocateOutputs();
  this->BeforeThreadedGenerateData();
  this->UpdateProgress(0.05f);

  ProgressTransformer pt(0.05f, 0.5f, this);
  this->GetMultiThreader()->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
  this->GetMultiThreader()->template ParallelizeImageRegion<OutputImageDimension>(
    this->GetOutput()->GetRequestedRegion(),
    [this](const OutputImageRegionType & outputRegionForThread) {
      this->DynamicThreadedGenerateData(outputRegionForThread);
    },
    pt.GetProcessObject());

  ProgressTransformer pt2(0.5f, 0.99f, this);
  // delegate to the superclass implementation to use the thread support for the label objects
  this->GetMultiThreader()->template ParallelizeImageRegion<OutputImageDimension>(
    this->GetOutput()->GetRequestedRegion(),
    [this](const OutputImageRegionType & outputRegionForThread) { this->SuperclassDynamicTGD(outputRegionForThread); },
    pt2.GetProcessObject());

  this->AfterThreadedGenerateData();
  this->UpdateProgress(1.0f);
}


template <typename TInputImage, typename TOutputImage>
void
LabelMapToBinaryImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  OutputImageType * output = this->GetOutput();

  // fill the output with background value - they will be overridden with the
  // foreground value later, if there is some objects
  if (this->GetNumberOfIndexedInputs() == 2)
  {
    // fill the background with the background values from the background image
    ImageRegionConstIterator<OutputImageType> bgIt(this->GetBackgroundImage(), outputRegionForThread);
    ImageRegionIterator<OutputImageType>      oIt(output, outputRegionForThread);

    bgIt.GoToBegin();
    oIt.GoToBegin();

    while (!oIt.IsAtEnd())
    {
      const OutputImagePixelType & bg = bgIt.Get();
      if (bg != this->m_ForegroundValue)
      {
        oIt.Set(bg);
      }
      else
      {
        oIt.Set(this->m_BackgroundValue);
      }
      ++oIt;
      ++bgIt;
    }
  }
  else
  {
    // fill the background with the background value
    ImageRegionIterator<OutputImageType> oIt(output, outputRegionForThread);
    oIt.GoToBegin();

    while (!oIt.IsAtEnd())
    {
      oIt.Set(this->m_BackgroundValue);
      ++oIt;
    }
  }
}

template <typename TInputImage, typename TOutputImage>
void
LabelMapToBinaryImageFilter<TInputImage, TOutputImage>::ThreadedProcessLabelObject(LabelObjectType * labelObject)
{
  OutputImageType *                            output = this->GetOutput();
  typename LabelObjectType::ConstIndexIterator it(labelObject);
  while (!it.IsAtEnd())
  {
    output->SetPixel(it.GetIndex(), this->m_ForegroundValue);
    ++it;
  }
}

template <typename TInputImage, typename TOutputImage>
void
LabelMapToBinaryImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);

  os << indent << "ForegroundValue: "
     << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(this->m_ForegroundValue) << std::endl;
  os << indent << "BackgroundValue: "
     << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(this->m_BackgroundValue) << std::endl;
}
} // end namespace itk

#endif
