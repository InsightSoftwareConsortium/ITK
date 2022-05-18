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
#ifndef itkForwardFFTImageFilter_hxx
#define itkForwardFFTImageFilter_hxx

#include "itkMetaDataObject.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
void
ForwardFFTImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  // Call the superclass implementation of this method.
  Superclass::GenerateInputRequestedRegion();

  // Get pointer to the input.
  typename InputImageType::Pointer input = const_cast<InputImageType *>(this->GetInput());

  if (!input)
  {
    return;
  }

  input->SetRequestedRegionToLargestPossibleRegion();
}

template <typename TInputImage, typename TOutputImage>
void
ForwardFFTImageFilter<TInputImage, TOutputImage>::EnlargeOutputRequestedRegion(DataObject * output)
{
  Superclass::EnlargeOutputRequestedRegion(output);
  output->SetRequestedRegionToLargestPossibleRegion();
}

template <typename TInputImage, typename TOutputImage>
SizeValueType
ForwardFFTImageFilter<TInputImage, TOutputImage>::GetSizeGreatestPrimeFactor() const
{
  return 2;
}

} // namespace itk
#endif
