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
#ifndef itkMaskedAssignImageFilter_hxx
#define itkMaskedAssignImageFilter_hxx

#include "itkMaskedAssignImageFilter.h"

namespace itk
{
template <typename TInputImage, typename TMaskImage, typename TOutputImage>
MaskedAssignImageFilter<TInputImage, TMaskImage, TOutputImage>::MaskedAssignImageFilter()
{
  this->SetPrimaryInputName("InputImage");
  this->AddRequiredInputName("MaskImage", 1);
  this->AddRequiredInputName("AssignImage", 2);

  auto func = [](const typename InputImageType::PixelType &  input,
                 const typename MaskImageType::PixelType &   mask,
                 const typename AssignImageType::PixelType & assign) -> OutputPixelType {
    if (mask != NumericTraits<typename MaskImageType::PixelType>::Zero)
    {
      return assign;
    }
    else
    {
      return static_cast<OutputPixelType>(input);
    }
  };
  this->SetFunctor(func);
}

} // end namespace itk

#endif
