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
#ifndef itkTestingGetBufferPointerCountingImage_h
#define itkTestingGetBufferPointerCountingImage_h

#include "itkImage.h"

namespace itk
{
namespace Testing
{
/** \class GetBufferPointerCountingImage
 * \brief Image that counts calls to its non-const GetBufferPointer(), like CudaImage relies on.
 * \ingroup ITKTestKernel
 */
template <typename TPixel, unsigned int VImageDimension = 2>
class GetBufferPointerCountingImage : public Image<TPixel, VImageDimension>
{
public:
  using Self = GetBufferPointerCountingImage;
  using Superclass = Image<TPixel, VImageDimension>;
  using Pointer = SmartPointer<Self>;
  using ConstWeakPointer = WeakPointer<const Self>;

  itkNewMacro(Self);

  using Superclass::GetBufferPointer;

  TPixel *
  GetBufferPointer() override
  {
    ++m_NumberOfNonConstGetBufferPointerCalls;
    return Superclass::GetBufferPointer();
  }

  [[nodiscard]] size_t
  GetNumberOfNonConstGetBufferPointerCalls() const
  {
    return m_NumberOfNonConstGetBufferPointerCalls;
  }

private:
  size_t m_NumberOfNonConstGetBufferPointerCalls{};
};


/** Tells whether `TIterator(argsBeforeImage..., image, region)` calls the non-const GetBufferPointer() of the image.
 * `TIterator::ImageType` must be a GetBufferPointerCountingImage. */
template <typename TIterator, typename... TArgs>
bool
IteratorConstructorCallsNonConstGetBufferPointer(const TArgs &... argsBeforeImage)
{
  using ImageType = typename TIterator::ImageType;

  const auto image = ImageType::New();
  image->SetRegions(ImageType::SizeType::Filled(4));
  image->Allocate();

  const size_t                     numberOfCallsBefore = image->GetNumberOfNonConstGetBufferPointerCalls();
  [[maybe_unused]] const TIterator iterator(argsBeforeImage..., image, image->GetBufferedRegion());
  return image->GetNumberOfNonConstGetBufferPointerCalls() > numberOfCallsBefore;
}
} // end namespace Testing
} // end namespace itk

#endif // itkTestingGetBufferPointerCountingImage_h
