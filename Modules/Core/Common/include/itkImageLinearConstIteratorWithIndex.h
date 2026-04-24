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
#ifndef itkImageLinearConstIteratorWithIndex_h
#define itkImageLinearConstIteratorWithIndex_h

#include "itkImageConstIteratorWithIndex.h"
#include "itkImageIteratorWithIndex.h"
#include <type_traits> // For conditional_t, enable_if_t, remove_const_t.

namespace itk
{
/** \class ImageLinearIteratorWithIndexBase
 * \brief Unified base for linear (scan-line) image iterators.
 *
 * SMOKE TEST (unit 6): parameterized on `VIsConst` to collapse the
 * ImageLinearConstIteratorWithIndex / ImageLinearIteratorWithIndex pair.
 * When `VIsConst` is false, the base inherits from
 * ImageIteratorWithIndex (mutable) and exposes `Set()` / non-const
 * `Value()` via SFINAE, eliminating the `const_cast<InternalPixelType *>`
 * previously used inside the mutable leaf class.
 *
 * \ingroup ITKCommon
 */
template <typename TImage, bool VIsConst>
class ITK_TEMPLATE_EXPORT ImageLinearIteratorWithIndexBase
  : public std::conditional_t<VIsConst, ImageConstIteratorWithIndex<TImage>, ImageIteratorWithIndex<TImage>>
{
public:
  using Self = ImageLinearIteratorWithIndexBase;
  using Superclass = std::conditional_t<VIsConst, ImageConstIteratorWithIndex<TImage>, ImageIteratorWithIndex<TImage>>;

  using IndexType = typename TImage::IndexType;
  using RegionType = typename TImage::RegionType;
  using ImageType = TImage;
  using PixelContainer = typename TImage::PixelContainer;
  using PixelContainerPointer = typename PixelContainer::Pointer;
  using typename Superclass::InternalPixelType;
  using typename Superclass::PixelType;

  ImageLinearIteratorWithIndexBase() = default;

  ImageLinearIteratorWithIndexBase(std::conditional_t<VIsConst, const TImage *, TImage *> ptr,
                                   const RegionType &                                     region)
    : Superclass(ptr, region)
  {}

  ImageLinearIteratorWithIndexBase(const ImageConstIteratorWithIndex<TImage> & it) { Superclass::operator=(it); }

  /** Write-access Set() — SFINAE-disabled in the const specialization. */
  template <bool V = VIsConst, std::enable_if_t<!V, int> = 0>
  void
  Set(const PixelType & value) const
  {
    // No const_cast: non-const base holds m_Position as InternalPixelType*.
    this->m_PixelAccessorFunctor.Set(*(this->m_Position), value);
  }

  /** Non-const Value() — SFINAE-disabled in the const specialization. */
  template <bool V = VIsConst, std::enable_if_t<!V, int> = 0>
  PixelType &
  Value()
  {
    return *(this->m_Position);
  }

  inline void
  NextLine();

  inline void
  PreviousLine();

  void
  GoToBeginOfLine();
  void
  GoToReverseBeginOfLine();
  void
  GoToEndOfLine();

  [[nodiscard]] inline bool
  IsAtEndOfLine() const
  {
    return this->m_PositionIndex[m_Direction] >= this->m_EndIndex[m_Direction];
  }

  [[nodiscard]] inline bool
  IsAtReverseEndOfLine() const
  {
    return this->m_PositionIndex[m_Direction] < this->m_BeginIndex[m_Direction];
  }

  inline void
  SetDirection(unsigned int direction)
  {
    if (direction >= TImage::ImageDimension)
    {
      itkGenericExceptionMacro("In image of dimension " << TImage::ImageDimension << " Direction " << direction
                                                        << " was selected");
    }
    m_Direction = direction;
    m_Jump = this->m_OffsetTable[m_Direction];
  }

  unsigned int
  GetDirection()
  {
    return m_Direction;
  }

  inline Self &
  operator++()
  {
    this->m_PositionIndex[m_Direction]++;
    this->m_Position += m_Jump;
    return *this;
  }

  inline Self &
  operator--()
  {
    this->m_PositionIndex[m_Direction]--;
    this->m_Position -= m_Jump;
    return *this;
  }

private:
  OffsetValueType m_Jump{ 0 };
  unsigned int    m_Direction{ 0 };
};

/** Const alias — preserves the historical public name. */
template <typename TImage>
using ImageLinearConstIteratorWithIndex = ImageLinearIteratorWithIndexBase<TImage, /*VIsConst=*/true>;

// Deduction guide (CTAD).
template <typename TImage>
ImageLinearIteratorWithIndexBase(SmartPointer<TImage>, const typename TImage::RegionType &)
  -> ImageLinearIteratorWithIndexBase<std::remove_const_t<TImage>, true>;


template <typename TImage, bool VIsConst>
inline void
ImageLinearIteratorWithIndexBase<TImage, VIsConst>::NextLine()
{
  this->m_Position -=
    this->m_OffsetTable[m_Direction] * (this->m_PositionIndex[m_Direction] - this->m_BeginIndex[m_Direction]);

  this->m_PositionIndex[m_Direction] = this->m_BeginIndex[m_Direction];

  for (unsigned int n = 0; n < TImage::ImageDimension; ++n)
  {
    this->m_Remaining = false;

    if (n == m_Direction)
    {
      continue;
    }

    this->m_PositionIndex[n]++;
    if (this->m_PositionIndex[n] < this->m_EndIndex[n])
    {
      this->m_Position += this->m_OffsetTable[n];
      this->m_Remaining = true;
      break;
    }

    this->m_Position -= this->m_OffsetTable[n] * (this->m_Region.GetSize()[n] - 1);
    this->m_PositionIndex[n] = this->m_BeginIndex[n];
  }
}

template <typename TImage, bool VIsConst>
inline void
ImageLinearIteratorWithIndexBase<TImage, VIsConst>::PreviousLine()
{
  this->m_Position +=
    this->m_OffsetTable[m_Direction] * (this->m_EndIndex[m_Direction] - 1 - this->m_PositionIndex[m_Direction]);

  this->m_PositionIndex[m_Direction] = this->m_EndIndex[m_Direction] - 1;

  for (unsigned int n = 0; n < TImage::ImageDimension; ++n)
  {
    this->m_Remaining = false;

    if (n == m_Direction)
    {
      continue;
    }

    this->m_PositionIndex[n]--;
    if (this->m_PositionIndex[n] >= this->m_BeginIndex[n])
    {
      this->m_Position -= this->m_OffsetTable[n];
      this->m_Remaining = true;
      break;
    }

    this->m_Position += this->m_OffsetTable[n] * (this->m_Region.GetSize()[n] - 1);
    this->m_PositionIndex[n] = this->m_EndIndex[n] - 1;
  }
}
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageLinearConstIteratorWithIndex.hxx"
#endif

#endif
