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
#ifndef itkAnchorErodeImageFilter_h
#define itkAnchorErodeImageFilter_h

#include "itkAnchorErodeDilateImageFilter.h"

namespace itk
{
template <typename TImage, typename TKernel>
class AnchorErodeImageFilter
  : public AnchorErodeDilateImageFilter<TImage, TKernel, std::less<typename TImage::PixelType>>

{
public:
  ITK_DISALLOW_COPY_AND_MOVE(AnchorErodeImageFilter);

  using Self = AnchorErodeImageFilter;
  using Superclass = AnchorErodeDilateImageFilter<TImage, TKernel, std::less<typename TImage::PixelType>>;

  /** Runtime information support. */
  itkTypeMacro(AnchorErodeImageFilter, AnchorErodeDilateImageFilter);

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using PixelType = typename TImage::PixelType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

protected:
  AnchorErodeImageFilter() { this->m_Boundary = NumericTraits<PixelType>::max(); }
  ~AnchorErodeImageFilter() override = default;

private:
};
} // namespace itk

#endif
