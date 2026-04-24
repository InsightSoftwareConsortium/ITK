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
#ifndef itkImageSliceIteratorWithIndexBase_h
#define itkImageSliceIteratorWithIndexBase_h

#include "itkImageIteratorWithIndex.h"
#include "itkImageConstIteratorWithIndex.h"
#include <type_traits>

namespace itk
{

/** \class ImageSliceIteratorWithIndexBase
 * \brief Unified template implementation for ImageSliceConstIteratorWithIndex
 * (VIsConst=true) and ImageSliceIteratorWithIndex (VIsConst=false).
 *
 * This class template is a SMOKE-TEST scaffold for Unit 7 of the iterator
 * modernization series (remove const_cast from iterator hierarchy). It
 * follows the VIsConst pattern used by itkNeighborhoodIteratorBase.h and
 * mirrors the prior spikes in Units 3/4/6.
 *
 * Ripple: Inherits from ImageIteratorWithIndex (Unit 2 base). The Unit 2
 * template must expose the same VIsConst parameter so that this class
 * selects the correct parent via std::conditional_t.
 *
 * \ingroup ImageIterators
 * \ingroup ITKCommon
 */
template <typename TImage, bool VIsConst>
class ITK_TEMPLATE_EXPORT ImageSliceIteratorWithIndexBase
  : public std::conditional_t<VIsConst, ImageConstIteratorWithIndex<TImage>, ImageIteratorWithIndex<TImage>>
{
public:
  using Self = ImageSliceIteratorWithIndexBase;
  using Superclass = std::conditional_t<VIsConst, ImageConstIteratorWithIndex<TImage>, ImageIteratorWithIndex<TImage>>;

  using typename Superclass::IndexType;
  using typename Superclass::SizeType;
  using typename Superclass::OffsetType;
  using typename Superclass::RegionType;
  using typename Superclass::ImageType;
  using typename Superclass::PixelContainer;
  using typename Superclass::PixelContainerPointer;
  using typename Superclass::InternalPixelType;
  using typename Superclass::PixelType;
  using typename Superclass::AccessorType;

  ImageSliceIteratorWithIndexBase() = default;

  ImageSliceIteratorWithIndexBase(std::conditional_t<VIsConst, const TImage *, TImage *> ptr, const RegionType & region)
    : Superclass(ptr, region)
  {}

  /** Non-const Set, SFINAE-gated to the mutable (non-const) instantiation. */
  template <bool VIsConst_ = VIsConst, typename = std::enable_if_t<!VIsConst_>>
  void
  Set(const PixelType & value) const
  {
    // Direct write — no const_cast needed because VIsConst=false implies the
    // underlying m_Position is already non-const InternalPixelType *.
    this->m_PixelAccessorFunctor.Set(*(this->m_Position), value);
  }

  /** Non-const Value(), SFINAE-gated. */
  template <bool VIsConst_ = VIsConst, typename = std::enable_if_t<!VIsConst_>>
  PixelType &
  Value()
  {
    return *(this->m_Position);
  }
};

// Alias templates for the two specializations (smoke-scaffold — not yet wired).
template <typename TImage>
using ImageSliceConstIteratorWithIndexV2 = ImageSliceIteratorWithIndexBase<TImage, /*VIsConst=*/true>;

template <typename TImage>
using ImageSliceIteratorWithIndexV2 = ImageSliceIteratorWithIndexBase<TImage, /*VIsConst=*/false>;

} // end namespace itk

#endif
