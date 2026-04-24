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
#ifndef itkImageRegionConstIterator_h
#define itkImageRegionConstIterator_h

#include "itkImageIterator.h"
#include <type_traits> // For remove_const_t, conditional_t, enable_if_t.

namespace itk
{
/** \class ImageRegionIteratorBase
 * \brief SMOKE-TEST template: unifies ImageRegionIterator and ImageRegionConstIterator
 * into a single class templated on a const-ness flag.
 *
 * VIsConst == true  -> behaves like ImageRegionConstIterator
 * VIsConst == false -> behaves like ImageRegionIterator (Set / mutable Value enabled via SFINAE)
 *
 * NOTE (smoke-test limitation): the parent class ImageConstIterator<TImage> is NOT
 * templated on VIsConst in this unit. As a result m_Buffer / m_Image in the parent
 * remain const-qualified. A real refactor requires templating the entire iterator
 * hierarchy (see unit 1 — ImageIterator).
 *
 * \ingroup ImageIterators
 * \ingroup ITKCommon
 */
template <typename TImage, bool VIsConst = true>
class ITK_TEMPLATE_EXPORT ImageRegionIteratorBase : public ImageConstIterator<TImage>
{
public:
  using Self = ImageRegionIteratorBase;
  using Superclass = ImageConstIterator<TImage>;

  static constexpr unsigned int ImageIteratorDimension = Superclass::ImageIteratorDimension;

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

  /** Conditional pointer type — core of the const-template pattern.
   * For the non-const specialization this is a writable pointer; for the
   * const specialization it is a pointer-to-const. */
  using BufferPointerType = std::conditional_t<VIsConst, const InternalPixelType *, InternalPixelType *>;
  using ImagePointerType = std::conditional_t<VIsConst, const TImage *, TImage *>;

  itkOverrideGetNameOfClassMacro(ImageRegionIteratorBase);

  ImageRegionIteratorBase()
    : ImageConstIterator<TImage>()
  {}

  ImageRegionIteratorBase(ImagePointerType ptr, const RegionType & region)
    : ImageConstIterator<TImage>(ptr, region)
    , m_SpanBeginOffset(this->m_BeginOffset)
    , m_SpanEndOffset(this->m_BeginOffset + static_cast<OffsetValueType>(this->m_Region.GetSize()[0]))
  {}

  ImageRegionIteratorBase(const ImageIterator<TImage> & it)
  {
    this->ImageConstIterator<TImage>::operator=(it);
    IndexType ind = this->GetIndex();
    m_SpanEndOffset = this->m_Offset + static_cast<OffsetValueType>(this->m_Region.GetSize()[0]) -
                      (ind[0] - this->m_Region.GetIndex()[0]);
    m_SpanBeginOffset = m_SpanEndOffset - static_cast<OffsetValueType>(this->m_Region.GetSize()[0]);
  }

  ImageRegionIteratorBase(const ImageConstIterator<TImage> & it)
  {
    this->ImageConstIterator<TImage>::operator=(it);
    IndexType ind = this->GetIndex();
    m_SpanEndOffset = this->m_Offset + static_cast<OffsetValueType>(this->m_Region.GetSize()[0]) -
                      (ind[0] - this->m_Region.GetIndex()[0]);
    m_SpanBeginOffset = m_SpanEndOffset - static_cast<OffsetValueType>(this->m_Region.GetSize()[0]);
  }

  void
  GoToBegin()
  {
    Superclass::GoToBegin();
    m_SpanBeginOffset = this->m_BeginOffset;
    m_SpanEndOffset = this->m_BeginOffset + static_cast<OffsetValueType>(this->m_Region.GetSize()[0]);
  }

  void
  GoToEnd()
  {
    Superclass::GoToEnd();
    m_SpanEndOffset = this->m_EndOffset;
    m_SpanBeginOffset = m_SpanEndOffset - static_cast<OffsetValueType>(this->m_Region.GetSize()[0]);
  }

  void
  SetIndex(const IndexType & ind) override
  {
    Superclass::SetIndex(ind);
    m_SpanEndOffset = this->m_Offset + static_cast<OffsetValueType>(this->m_Region.GetSize()[0]) -
                      (ind[0] - this->m_Region.GetIndex()[0]);
    m_SpanBeginOffset = m_SpanEndOffset - static_cast<OffsetValueType>(this->m_Region.GetSize()[0]);
  }

  Self &
  operator++()
  {
    if (++this->m_Offset >= m_SpanEndOffset)
    {
      this->Increment();
    }
    return *this;
  }

  Self &
  operator--()
  {
    if (--this->m_Offset < m_SpanBeginOffset)
    {
      this->Decrement();
    }
    return *this;
  }

  /** Write access — only enabled when VIsConst == false.
   *
   * SMOKE NOTE: the underlying m_Buffer in the parent ImageConstIterator is
   * const InternalPixelType*. Removing the last const_cast requires unit 1
   * (ImageIterator) to also template on VIsConst so the storage type can be
   * non-const for the writable specialization. Here we perform the cast inside
   * the gated method; a real refactor eliminates it entirely. */
  template <bool B = VIsConst, typename = std::enable_if_t<!B>>
  void
  Set(const PixelType & value) const
  {
    this->m_PixelAccessorFunctor.Set(*(const_cast<InternalPixelType *>(this->m_Buffer + this->m_Offset)),
                                     value); // SMOKE: parent-class ripple
  }

  template <bool B = VIsConst, typename = std::enable_if_t<!B>>
  PixelType &
  Value()
  {
    return *(const_cast<InternalPixelType *>(this->m_Buffer + this->m_Offset)); // SMOKE: parent-class ripple
  }

protected:
  OffsetValueType m_SpanBeginOffset{};
  OffsetValueType m_SpanEndOffset{};

private:
  void
  Increment();

  void
  Decrement();
};

/** Legacy alias — read-only iterator. */
template <typename TImage>
using ImageRegionConstIterator = ImageRegionIteratorBase<TImage, /*VIsConst=*/true>;

// Deduction guide for class template argument deduction (CTAD).
template <typename TImage>
ImageRegionIteratorBase(SmartPointer<TImage>, const typename TImage::RegionType &)
  -> ImageRegionIteratorBase<std::remove_const_t<TImage>, std::is_const_v<TImage>>;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageRegionConstIterator.hxx"
#endif

#endif
