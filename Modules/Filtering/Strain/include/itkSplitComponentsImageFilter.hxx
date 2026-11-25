/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkSplitComponentsImageFilter_hxx
#define itkSplitComponentsImageFilter_hxx


#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"

namespace itk
{

template <typename TInputImage, typename TOutputImage, unsigned int TComponents>
SplitComponentsImageFilter<TInputImage, TOutputImage, TComponents>::SplitComponentsImageFilter()
{
  this->m_ComponentsMask.Fill(true);

  this->SetNumberOfIndexedOutputs(Components);

  // ImageSource only does this for the first output.
  for (unsigned int i = 1; i < Components; i++)
  {
    this->SetNthOutput(i, this->MakeOutput(i));
  }

  this->DynamicMultiThreadingOn();
}


template <typename TInputImage, typename TOutputImage, unsigned int TComponents>
void
SplitComponentsImageFilter<TInputImage, TOutputImage, TComponents>::AllocateOutputs()
{
  using ImageBaseType = ImageBase<TOutputImage::ImageDimension>;
  typename ImageBaseType::Pointer outputPtr;

  // Allocate the output memory as with ImageSource
  unsigned int ii = 0;
  for (OutputDataObjectIterator it(this); !it.IsAtEnd(); ++it, ++ii)
  {
    // Check whether the output is an image of the appropriate
    // dimension (use ProcessObject's version of the GetInput()
    // method since it returns the input as a pointer to a
    // DataObject as opposed to the subclass version which
    // static_casts the input to an TInputImage).
    outputPtr = dynamic_cast<ImageBaseType *>(it.GetOutput());

    if (outputPtr && this->m_ComponentsMask[ii])
    {
      outputPtr->SetBufferedRegion(outputPtr->GetRequestedRegion());
      outputPtr->Allocate();
    }
  }
}

template <typename TInputImage, typename TOutputImage, unsigned int TComponents>
void
SplitComponentsImageFilter<TInputImage, TOutputImage, TComponents>::DynamicThreadedGenerateData(
  const OutputRegionType & outputRegion)
{
  typename InputImageType::ConstPointer input = this->GetInput();
  ProcessObject::DataObjectPointerArray outputs = this->GetOutputs();
  const ComponentsMaskType              componentsMask = this->m_ComponentsMask;

  using OutputIteratorType = ImageRegionIterator<OutputImageType>;
  ImageRegionConstIterator<InputImageType> inIt(input, outputRegion);
  std::vector<OutputIteratorType>          outIts(Components);
  for (unsigned int ii = 0; ii < Components; ++ii)
  {
    if (componentsMask[ii])
    {
      OutputIteratorType outIt(dynamic_cast<OutputImageType *>(outputs[ii].GetPointer()), outputRegion);
      outIt.GoToBegin();
      outIts[ii] = outIt;
    }
  }
  InputPixelType inputPixel;
  for (inIt.GoToBegin(); !inIt.IsAtEnd(); ++inIt)
  {
    inputPixel = inIt.Get();
    for (unsigned int ii = 0; ii < Components; ++ii)
    {
      if (componentsMask[ii])
      {
        outIts[ii].Set(static_cast<OutputPixelType>(inputPixel[ii]));
        ++(outIts[ii]);
      }
    }
  }
}

template <typename TInputImage, typename TOutputImage, unsigned int TComponents>
void
SplitComponentsImageFilter<TInputImage, TOutputImage, TComponents>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent
     << "ComponentsMask: " << static_cast<typename NumericTraits<ComponentsMaskType>::PrintType>(m_ComponentsMask)
     << std::endl;
}
} // end namespace itk

#endif
