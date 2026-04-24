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
#ifndef itkLineIterator_h
#define itkLineIterator_h

#include "itkLineConstIterator.h"
#include <type_traits>

namespace itk
{
/**
 * \class LineIteratorBase
 * \brief Templated base for LineIterator generalized over const-ness.
 *
 * SMOKE-TEST SPIKE: VIsConst template parameter gates write access via
 * SFINAE on Set(), eliminating the const_cast used by the legacy
 * LineIterator. The non-const variant (VIsConst == false) is the legacy
 * LineIterator; a const alias is provided for symmetry with the
 * ShapedNeighborhoodIterator / NeighborhoodIteratorBase pattern.
 *
 * Note: the underlying m_Image is inherited from LineConstIterator and
 * stored as ConstWeakPointer regardless of VIsConst. Writes go through
 * an explicit const_cast localized to this base; the value of the
 * VIsConst parameter is that client code cannot call Set() on a
 * VIsConst=true instantiation, making the const-correctness visible at
 * the type level instead of only at the call site.
 *
 * \ingroup ITKCommon
 */
template <typename TImage, bool VIsConst>
class ITK_TEMPLATE_EXPORT LineIteratorBase : public LineConstIterator<TImage>
{
public:
  using Self = LineIteratorBase;
  using Superclass = LineConstIterator<TImage>;

  static constexpr unsigned int ImageIteratorDimension = TImage::ImageDimension;
  static constexpr bool         IsConst = VIsConst;

  using typename Superclass::IndexType;
  using typename Superclass::OffsetType;
  using typename Superclass::SizeType;
  using typename Superclass::RegionType;
  using typename Superclass::ImageType;
  using typename Superclass::PixelContainer;
  using typename Superclass::PixelContainerPointer;
  using typename Superclass::InternalPixelType;
  using typename Superclass::PixelType;
  using typename Superclass::AccessorType;

  itkOverrideGetNameOfClassMacro(LineIteratorBase);

  /** Set the pixel value. Only available for the non-const instantiation. */
  template <bool VDep = VIsConst, typename = std::enable_if_t<!VDep>>
  void
  Set(const PixelType & value)
  {
    const_cast<ImageType *>(this->m_Image.GetPointer())->SetPixel(this->m_CurrentImageIndex, value);
  }

  /** Return a reference to the pixel. */
  const PixelType &
  Value()
  {
    return this->m_Image->GetPixel(this->m_CurrentImageIndex);
  }

  Self &
  operator=(const Self & it)
  {
    if (this != &it)
    {
      this->Superclass::operator=(it);
    }
    return *this;
  }

  LineIteratorBase(ImageType * imagePtr, const IndexType & firstIndex, const IndexType & lastIndex)
    : Superclass(imagePtr, firstIndex, lastIndex)
  {}

  ~LineIteratorBase() override = default;
};

/** Legacy writable LineIterator — non-const instantiation. */
template <typename TImage>
using LineIterator = LineIteratorBase<TImage, false>;

/** Optional const alias (symmetric with ShapedNeighborhoodIterator pattern).
 *  Future work: unify with LineConstIterator. For now this is a distinct
 *  type that also provides the LineConstIterator interface via inheritance
 *  but with the VIsConst template tag visible in generic code. */
template <typename TImage>
using ConstLineIterator = LineIteratorBase<TImage, true>;

} // end namespace itk

#endif
