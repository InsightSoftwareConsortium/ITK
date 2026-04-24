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
#ifndef itkImageRandomNonRepeatingIteratorWithIndexBase_h
#define itkImageRandomNonRepeatingIteratorWithIndexBase_h

// -----------------------------------------------------------------------------
// SMOKE-TEST Unit 9 structural spike.
//
// ImageRandomNonRepeatingIteratorWithIndexBase<TImage, VIsConst> is a unified
// base template that will ultimately replace the sibling pair
//   * ImageRandomNonRepeatingConstIteratorWithIndex<TImage>    (VIsConst=true)
//   * ImageRandomNonRepeatingIteratorWithIndex<TImage>         (VIsConst=false)
// with a single implementation parameterized on a compile-time `bool VIsConst`
// using std::conditional_t -- eliminating the two const_cast<InternalPixelType*>
// sites at itkImageRandomNonRepeatingIteratorWithIndex.h:117 and :126.
//
// This file is a *structural* proof only (compile-only). It does NOT yet port
// the permutation / priority-image behavior; see the sibling legacy headers
// which remain authoritative until a subsequent PR wires bodies over. Pattern
// mirrors itkNeighborhoodIteratorBase.h.
//
// RIPPLE: This base inherits from ImageIteratorWithIndex (Unit 2). When
// Unit 2 introduces ImageIteratorWithIndexBase<TImage, VIsConst>, the
// Superclass alias below will switch to that unified base and the VIsConst
// parameter will propagate. Until then the base header pair is used directly:
// const path -> ImageConstIteratorWithIndex, mutable path ->
// ImageIteratorWithIndex.
// -----------------------------------------------------------------------------

#include "itkImageConstIteratorWithIndex.h"
#include "itkImageIteratorWithIndex.h"
#include <type_traits>

namespace itk
{

template <typename TImage, bool VIsConst>
class ITK_TEMPLATE_EXPORT ImageRandomNonRepeatingIteratorWithIndexBase
  : public std::conditional_t<VIsConst, ImageConstIteratorWithIndex<TImage>, ImageIteratorWithIndex<TImage>>
{
public:
  static constexpr bool IsConst = VIsConst;

  using Self = ImageRandomNonRepeatingIteratorWithIndexBase;
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

  /** Image-pointer type flips with VIsConst; no const_cast needed. */
  using ImagePointer = std::conditional_t<VIsConst, const TImage *, TImage *>;

  /** Default ctor. */
  ImageRandomNonRepeatingIteratorWithIndexBase() = default;

  /** Region ctor -- input image pointer constness follows VIsConst. */
  ImageRandomNonRepeatingIteratorWithIndexBase(ImagePointer ptr, const RegionType & region)
    : Superclass(ptr, region)
  {}

  /** Write-access methods are SFINAE-disabled on the const instantiation.
   * On the mutable instantiation, m_Position is already InternalPixelType*
   * (inherited from ImageIteratorWithIndex), so no const_cast is required. */
  template <bool VDep = VIsConst, std::enable_if_t<!VDep, int> = 0>
  void
  Set(const PixelType & value) const
  {
    this->m_PixelAccessorFunctor.Set(*(this->m_Position), value);
  }

  template <bool VDep = VIsConst, std::enable_if_t<!VDep, int> = 0>
  PixelType &
  Value()
  {
    return *(this->m_Position);
  }
};

/** Alias templates preserve the legacy class names during migration. */
template <typename TImage>
using ImageRandomNonRepeatingConstIteratorWithIndexN1 = ImageRandomNonRepeatingIteratorWithIndexBase<TImage, true>;

template <typename TImage>
using ImageRandomNonRepeatingIteratorWithIndexN1 = ImageRandomNonRepeatingIteratorWithIndexBase<TImage, false>;

} // end namespace itk

#endif
