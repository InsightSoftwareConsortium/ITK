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
#ifndef itkComplexToComplex1DFFTImageFilter_hxx
#define itkComplexToComplex1DFFTImageFilter_hxx


#include "itkMetaDataDictionary.h"
#include "itkMetaDataObject.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
ComplexToComplex1DFFTImageFilter<TInputImage, TOutputImage>::ComplexToComplex1DFFTImageFilter() = default;


template <typename TInputImage, typename TOutputImage>
void
ComplexToComplex1DFFTImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the inputs
  typename InputImageType::Pointer  inputPtr = const_cast<InputImageType *>(this->GetInput());
  typename OutputImageType::Pointer outputPtr = this->GetOutput();

  if (!inputPtr || !outputPtr)
  {
    return;
  }

  // we need to compute the input requested region (size and start index)
  using OutputSizeType = const typename OutputImageType::SizeType &;
  OutputSizeType outputRequestedRegionSize = outputPtr->GetRequestedRegion().GetSize();
  using OutputIndexType = const typename OutputImageType::IndexType &;
  OutputIndexType outputRequestedRegionStartIndex = outputPtr->GetRequestedRegion().GetIndex();

  //// the regions other than the fft direction are fine
  typename InputImageType::SizeType  inputRequestedRegionSize = outputRequestedRegionSize;
  typename InputImageType::IndexType inputRequestedRegionStartIndex = outputRequestedRegionStartIndex;

  // we but need all of the input in the fft direction
  const unsigned int                        direction = this->m_Direction;
  const typename InputImageType::SizeType & inputLargeSize = inputPtr->GetLargestPossibleRegion().GetSize();
  inputRequestedRegionSize[direction] = inputLargeSize[direction];
  const typename InputImageType::IndexType & inputLargeIndex = inputPtr->GetLargestPossibleRegion().GetIndex();
  inputRequestedRegionStartIndex[direction] = inputLargeIndex[direction];

  const typename InputImageType::RegionType inputRequestedRegion(inputRequestedRegionStartIndex,
                                                                 inputRequestedRegionSize);

  inputPtr->SetRequestedRegion(inputRequestedRegion);
}


template <typename TInputImage, typename TOutputImage>
void
ComplexToComplex1DFFTImageFilter<TInputImage, TOutputImage>::EnlargeOutputRequestedRegion(DataObject * output)
{
  OutputImageType * outputPtr = dynamic_cast<OutputImageType *>(output);

  // we need to enlarge the region in the fft direction to the
  // largest possible in that direction
  using ConstOutputSizeType = const typename OutputImageType::SizeType &;
  ConstOutputSizeType requestedSize = outputPtr->GetRequestedRegion().GetSize();
  ConstOutputSizeType outputLargeSize = outputPtr->GetLargestPossibleRegion().GetSize();
  using ConstOutputIndexType = const typename OutputImageType::IndexType &;
  ConstOutputIndexType requestedIndex = outputPtr->GetRequestedRegion().GetIndex();
  ConstOutputIndexType outputLargeIndex = outputPtr->GetLargestPossibleRegion().GetIndex();

  typename OutputImageType::SizeType  enlargedSize = requestedSize;
  typename OutputImageType::IndexType enlargedIndex = requestedIndex;
  enlargedSize[this->m_Direction] = outputLargeSize[this->m_Direction];
  enlargedIndex[this->m_Direction] = outputLargeIndex[this->m_Direction];

  const typename OutputImageType::RegionType enlargedRegion(enlargedIndex, enlargedSize);
  outputPtr->SetRequestedRegion(enlargedRegion);
}


template <typename TInputImage, typename TOutputImage>
void
ComplexToComplex1DFFTImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Direction: " << m_Direction << std::endl;
  os << indent << "TransformDirection: " << m_TransformDirection << std::endl;
}


} // end namespace itk

#endif // itkComplexToComplex1DFFTImageFilter_hxx
