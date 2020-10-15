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
#ifndef itkAnchorOpenImageFilter_h
#define itkAnchorOpenImageFilter_h

#include "itkAnchorOpenCloseImageFilter.h"

namespace itk
{
template <typename TImage, typename TKernel>
class AnchorOpenImageFilter
  : public AnchorOpenCloseImageFilter<TImage,
                                      TKernel,
                                      std::less<typename TImage::PixelType>,
                                      std::greater<typename TImage::PixelType>>

{
public:
  ITK_DISALLOW_COPY_AND_MOVE(AnchorOpenImageFilter);

  using Self = AnchorOpenImageFilter;
  using Superclass = AnchorOpenCloseImageFilter<TImage,
                                                TKernel,
                                                std::less<typename TImage::PixelType>,
                                                std::greater<typename TImage::PixelType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

protected:
  AnchorOpenImageFilter()
  {
    this->m_Boundary1 = NumericTraits<typename TImage::PixelType>::max();
    this->m_Boundary2 = NumericTraits<typename TImage::PixelType>::NonpositiveMin();
  }
  ~AnchorOpenImageFilter() override = default;

private:
};
} // namespace itk

#endif
