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
#ifndef itkUnaryFrequencyDomainFilter_hxx
#define itkUnaryFrequencyDomainFilter_hxx

#include <itkUnaryFrequencyDomainFilter.h>

#include <itkMath.h>
#include <itkImageAlgorithm.h>

namespace itk
{
template <typename TImageType, typename TFrequencyIterator>
UnaryFrequencyDomainFilter<TImageType, TFrequencyIterator>::UnaryFrequencyDomainFilter()

{
  this->InPlaceOff();
  this->DynamicMultiThreadingOn();
  this->SetFunctor([](FrequencyIteratorType &) {}); // no-op functor
}

template <typename TImageType, typename TFrequencyIterator>
void
UnaryFrequencyDomainFilter<TImageType, TFrequencyIterator>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ActualXDimensionIsOdd? " << (this->m_ActualXDimensionIsOdd ? "Yes" : "No ") << std::endl;
}

template <typename TImageType, typename TFrequencyIterator>
void
UnaryFrequencyDomainFilter<TImageType, TFrequencyIterator>::DynamicThreadedGenerateData(
  const ImageRegionType & outputRegionForThread)
{
  m_DynamicThreadedGenerateDataFunction(outputRegionForThread);
}

template <typename TInputImage, typename TFrequencyIterator>
template <typename TFunctor>
void
UnaryFrequencyDomainFilter<TInputImage, TFrequencyIterator>::DynamicThreadedGenerateDataWithFunctor(
  const TFunctor &        functor,
  const ImageRegionType & outputRegionForThread)
{
  const ImageType * inputPtr = this->GetInput();
  ImageType *       outputPtr = this->GetOutput();

  // Define the portion of the input to walk for this thread
  ImageRegionType inputRegionForThread;
  this->CallCopyOutputRegionToInputRegion(inputRegionForThread, outputRegionForThread);
  if (!this->GetRunningInPlace())
  {
    // copy the input pixel to the output
    ImageAlgorithm::Copy(inputPtr, outputPtr, inputRegionForThread, outputRegionForThread);
  }

  FrequencyIteratorType freqIt(outputPtr, outputRegionForThread);
  // Only for HalfHermitian Iterator this option changes frequency spacing
  freqIt.SetActualXDimensionIsOdd(this->GetActualXDimensionIsOdd());
  freqIt.GoToBegin();
  while (!freqIt.IsAtEnd())
  {
    functor(freqIt);
    ++freqIt;
  }
}

} // end namespace itk

#endif // itkUnaryFrequencyDomainFilter_hxx
