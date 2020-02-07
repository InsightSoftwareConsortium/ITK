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
#ifndef itkNaryFunctorImageFilter_hxx
#define itkNaryFunctorImageFilter_hxx

#include "itkNaryFunctorImageFilter.h"
#include "itkImageScanlineIterator.h"
#include "itkProgressReporter.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage, typename TFunction>
NaryFunctorImageFilter<TInputImage, TOutputImage, TFunction>::NaryFunctorImageFilter()
{
  // This number will be incremented each time an image is added.
  this->SetNumberOfRequiredInputs(1);
  this->InPlaceOff();
  this->DynamicMultiThreadingOn();
}


template <typename TInputImage, typename TOutputImage, typename TFunction>
void
NaryFunctorImageFilter<TInputImage, TOutputImage, TFunction>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  const SizeValueType size0 = outputRegionForThread.GetSize(0);
  if (size0 == 0)
  {
    return;
  }
  const auto numberOfInputImages = static_cast<unsigned int>(this->GetNumberOfIndexedInputs());

  using ImageScanlineConstIteratorType = ImageScanlineConstIterator<TInputImage>;
  std::vector<ImageScanlineConstIteratorType *> inputItrVector;
  inputItrVector.reserve(numberOfInputImages);

  // count the number of inputs that are non-null
  for (unsigned int i = 0; i < numberOfInputImages; ++i)
  {
    InputImagePointer inputPtr = dynamic_cast<TInputImage *>(ProcessObject::GetInput(i));

    if (inputPtr)
    {
      inputItrVector.push_back(new ImageScanlineConstIteratorType(inputPtr, outputRegionForThread));
    }
  }

  const auto numberOfValidInputImages = static_cast<const unsigned int>(inputItrVector.size());

  if (numberOfValidInputImages == 0)
  {
    // No valid regions in the thread
    //(and no region iterators to delete)
    return;
  }

  NaryArrayType naryInputArray(numberOfValidInputImages);

  OutputImagePointer                  outputPtr = this->GetOutput(0);
  ImageScanlineIterator<TOutputImage> outputIt(outputPtr, outputRegionForThread);

  typename std::vector<ImageScanlineConstIteratorType *>::iterator             regionIterators;
  const typename std::vector<ImageScanlineConstIteratorType *>::const_iterator regionItEnd = inputItrVector.end();

  typename NaryArrayType::iterator arrayIt;

  while (!outputIt.IsAtEnd())
  {
    while (!outputIt.IsAtEndOfLine())
    {
      arrayIt = naryInputArray.begin();
      regionIterators = inputItrVector.begin();
      while (regionIterators != regionItEnd)
      {
        *arrayIt++ = (*regionIterators)->Get();
        ++(*(*regionIterators));
        ++regionIterators;
      }
      outputIt.Set(m_Functor(naryInputArray));
      ++outputIt;
    }

    regionIterators = inputItrVector.begin();
    while (regionIterators != regionItEnd)
    {
      (*regionIterators)->NextLine();
      ++regionIterators;
    }
    outputIt.NextLine();
  }

  // Free memory
  regionIterators = inputItrVector.begin();
  while (regionIterators != regionItEnd)
  {
    delete (*regionIterators++);
  }
}
} // end namespace itk

#endif
