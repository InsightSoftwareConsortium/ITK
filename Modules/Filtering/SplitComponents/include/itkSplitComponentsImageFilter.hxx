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
#ifndef __itkSplitComponentsImageFilter_hxx
#define __itkSplitComponentsImageFilter_hxx

#include "itkSplitComponentsImageFilter.h"

#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"

namespace itk
{

template <class TInputImage, class TOutputImage, unsigned int TComponents>
SplitComponentsImageFilter<TInputImage, TOutputImage, TComponents>::SplitComponentsImageFilter()
{
  this->SetNumberOfIndexedOutputs(Components);

  // ImageSource only does this for the first output.
  for (unsigned int i = 1; i < Components; i++)
  {
    this->SetNthOutput(i, this->MakeOutput(i));
  }
}

template <class TInputImage, class TOutputImage, unsigned int TComponents>
void
SplitComponentsImageFilter<TInputImage, TOutputImage, TComponents>::ThreadedGenerateData(
  const OutputRegionType & outputRegion,
  ThreadIdType             itkNotUsed(threadId))
{
  typename InputImageType::ConstPointer input = this->GetInput();

  ProcessObject::DataObjectPointerArray outputs = this->GetOutputs();

  typedef ImageRegionIterator<OutputImageType> OutputIteratorType;
  ImageRegionConstIterator<InputImageType>     inIt(input, outputRegion);
  std::vector<OutputIteratorType>              outIts;
  unsigned int                                 i;
  for (i = 0; i < Components; i++)
  {
    OutputIteratorType outIt(dynamic_cast<OutputImageType *>(outputs[i].GetPointer()), outputRegion);
    outIt.GoToBegin();
    outIts.push_back(outIt);
  }
  InputPixelType inputPixel;
  for (inIt.GoToBegin(); !inIt.IsAtEnd(); ++inIt)
  {
    inputPixel = inIt.Get();
    for (i = 0; i < Components; i++)
    {
      outIts[i].Set(static_cast<OutputPixelType>(inputPixel[i]));
      ++(outIts[i]);
    }
  }
}

} // end namespace itk

#endif
