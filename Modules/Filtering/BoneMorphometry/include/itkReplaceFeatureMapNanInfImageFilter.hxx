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
#ifndef itkReplaceFeatureMapNanInfImageFilter_hxx
#define itkReplaceFeatureMapNanInfImageFilter_hxx

#include "itkReplaceFeatureMapNanInfImageFilter.h"

namespace itk
{
template <typename TImage>

ReplaceFeatureMapNanInfImageFilter<TImage>::ReplaceFeatureMapNanInfImageFilter()
{
  m_IndexSelectionFiter = IndexSelectionFiterType::New();
  m_IndexSelectionFiter->SetInput(this->GetInput());
  m_IndexSelectionFiter->SetIndex(0);
}

template <typename TImage>
void
ReplaceFeatureMapNanInfImageFilter<TImage>::GenerateData()
{
  m_IndexSelectionFiter->SetInput(this->GetInput());

  for (unsigned int i = 0; i < 5; i++)
  {
    m_IndexSelectionFiter->SetInput(this->GetInput());
    m_IndexSelectionFiter->SetIndex(i);
    m_IndexSelectionFiter->Update();

    InterIteratorType interIt(m_IndexSelectionFiter->GetOutput(),
                              m_IndexSelectionFiter->GetOutput()->GetLargestPossibleRegion());
    interIt.GoToBegin();
    RealType min = interIt.Get();
    RealType max = interIt.Get();

    while (!interIt.IsAtEnd())
    {
      if (!Math::isnan(interIt.Get()) && !Math::isinf(interIt.Get()))
      {
        if (interIt.Get() < min)
        {
          min = interIt.Get();
        }
        if (interIt.Get() > max)
        {
          max = interIt.Get();
        }
      }
      ++interIt;
    }

    TImage * outputPtr = this->GetOutput();
    outputPtr->SetRegions(this->GetInput()->GetLargestPossibleRegion());
    outputPtr->Allocate();
    using IteratorType = ImageRegionIterator<TImage>;
    IteratorType outputIt(outputPtr, outputPtr->GetLargestPossibleRegion());
    outputIt.GoToBegin();
    interIt.GoToBegin();
    PixelType pixel;

    while (!interIt.IsAtEnd())
    {
      pixel = outputIt.Get();
      if (Math::isnan(interIt.Get()))
      {
        if (i == 4)
        {
          pixel[i] = max;
        }
        else
        {
          pixel[i] = min;
        }
      }
      else if (Math::isinf(interIt.Get()))
      {
        if (i == 4)
        {
          pixel[i] = min;
        }
        else
        {
          pixel[i] = max;
        }
      }
      else
      {
        pixel[i] = interIt.Get();
      }
      outputIt.Set(pixel);
      ++interIt;
      ++outputIt;
    }
  }
  this->GraftOutput(this->GetOutput());
}

template <typename TImage>
void
ReplaceFeatureMapNanInfImageFilter<TImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk

#endif // itkReplaceFeatureMapNanInfImageFilter_hxx
