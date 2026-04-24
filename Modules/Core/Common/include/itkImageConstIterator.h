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
#ifndef itkImageConstIterator_h
#define itkImageConstIterator_h

#include "itkImage.h"
#include "itkIndex.h"
#include "itkNumericTraits.h"
#include <type_traits> // For remove_const_t, conditional_t, enable_if_t.

namespace itk
{
/** \class ImageIteratorBase
 * \brief Shared implementation of ImageConstIterator and ImageIterator,
 * templated on a compile-time `bool VIsConst` that selects const vs. mutable
 * pointer/reference flavors of the pixel buffer and image pointer. This
 * removes the need for `const_cast` in the non-const flavor.
 *
 * The legacy class names `ImageConstIterator<TImage>` and
 * `ImageIterator<TImage>` are exposed as alias templates at the end of this
 * header (and at the end of `itkImageIterator.h` respectively). Public API
 * is preserved.
 *
 * \tparam TImage   The image type being iterated.
 * \tparam VIsConst `true` for the const flavor (read-only `Value()`/`Get()`,
 *                  no `Set()`), `false` for the mutable flavor.
 *
 * \ingroup ImageIterators
 * \ingroup ITKCommon
 */
template <typename TImage, bool VIsConst = true>
class ITK_TEMPLATE_EXPORT ImageIteratorBase
{
public:
  static constexpr bool IsConst = VIsConst;

  /** Standard class type aliases. */
  using Self = ImageIteratorBase;

  /** Dimension of the image the iterator walks. */
  static constexpr unsigned int ImageIteratorDimension = TImage::ImageDimension;

  /** \see LightObject::GetNameOfClass() */
  itkVirtualGetNameOfClassMacro(ImageIteratorBase);

  using IndexType = typename TImage::IndexType;
  using SizeType = typename TImage::SizeType;
  using OffsetType = typename TImage::OffsetType;
  using RegionType = typename TImage::RegionType;
  using ImageType = TImage;

  using PixelContainer = typename TImage::PixelContainer;
  using PixelContainerPointer = typename PixelContainer::Pointer;

  using InternalPixelType = typename TImage::InternalPixelType;
  using PixelType = typename TImage::PixelType;

  using AccessorType = typename TImage::AccessorType;
  using AccessorFunctorType = typename TImage::AccessorFunctorType;

  /** Pointer / reference types flip based on VIsConst. No const_cast anywhere. */
  using InternalPixelPointer = std::conditional_t<VIsConst, const InternalPixelType *, InternalPixelType *>;
  using ImagePointer = std::conditional_t<VIsConst, const ImageType *, ImageType *>;
  using ImageWeakPointerType = std::conditional_t<VIsConst, typename TImage::ConstWeakPointer, WeakPointer<ImageType>>;

  /** Default Constructor. */
  ImageIteratorBase()
    : m_Image(nullptr)
    , m_Region()
    , m_Buffer(nullptr)
    , m_PixelAccessor()
    , m_PixelAccessorFunctor()
  {
    m_PixelAccessorFunctor.SetBegin(m_Buffer);
  }

  /** Default Destructor. */
  virtual ~ImageIteratorBase() = default;

  /** Copy Constructor. */
  ImageIteratorBase(const Self & it)
    : m_Image(it.m_Image)
    , m_Region(it.m_Region)
    , m_Offset(it.m_Offset)
    , m_BeginOffset(it.m_BeginOffset)
    , m_EndOffset(it.m_EndOffset)
    , m_Buffer(it.m_Buffer)
    , m_PixelAccessor(it.m_PixelAccessor)
    , m_PixelAccessorFunctor(it.m_PixelAccessorFunctor)
  {
    m_PixelAccessorFunctor.SetBegin(m_Buffer);
  }

  /** Constructor establishes an iterator to walk a particular image and
   * region. Accepts a pointer whose const-ness matches VIsConst. */
  ImageIteratorBase(ImagePointer ptr, const RegionType & region)
    : m_Image(ptr)
    , m_Buffer(m_Image->GetBufferPointer())
    , m_PixelAccessor(ptr->GetPixelAccessor())
  {
    SetRegion(region);
    m_PixelAccessorFunctor.SetPixelAccessor(m_PixelAccessor);
    m_PixelAccessorFunctor.SetBegin(m_Buffer);
  }

  /** Converting constructor: non-const -> const. Only enabled for the
   * const flavor when copying from a mutable iterator. */
  template <bool VOtherIsConst, std::enable_if_t<VIsConst && !VOtherIsConst, int> = 0>
  ImageIteratorBase(const ImageIteratorBase<TImage, VOtherIsConst> & it)
    : m_Image(it.GetImage())
    , m_Region(it.GetRegion())
    , m_Buffer(it.GetBufferPointerForCopy())
    , m_PixelAccessor()
    , m_PixelAccessorFunctor()
  {
    m_PixelAccessorFunctor.SetBegin(m_Buffer);
  }

  /** Assignment. */
  Self &
  operator=(const Self & it)
  {
    if (this != &it)
    {
      m_Image = it.m_Image;
      m_Region = it.m_Region;
      m_Buffer = it.m_Buffer;
      m_Offset = it.m_Offset;
      m_BeginOffset = it.m_BeginOffset;
      m_EndOffset = it.m_EndOffset;
      m_PixelAccessor = it.m_PixelAccessor;
      m_PixelAccessorFunctor = it.m_PixelAccessorFunctor;
      m_PixelAccessorFunctor.SetBegin(m_Buffer);
    }
    return *this;
  }

  /** Set the region of the image to iterate over. */
  virtual void
  SetRegion(const RegionType & region)
  {
    m_Region = region;

    if (region.GetNumberOfPixels() > 0)
    {
      const RegionType & bufferedRegion = m_Image->GetBufferedRegion();
      itkAssertOrThrowMacro((bufferedRegion.IsInside(m_Region)),
                            "Region " << m_Region << " is outside of buffered region " << bufferedRegion);
    }

    m_Offset = m_Image->ComputeOffset(m_Region.GetIndex());
    m_BeginOffset = m_Offset;

    IndexType ind(m_Region.GetIndex());
    SizeType  size(m_Region.GetSize());
    if (m_Region.GetNumberOfPixels() == 0)
    {
      m_EndOffset = m_BeginOffset;
    }
    else
    {
      for (unsigned int i = 0; i < TImage::ImageDimension; ++i)
      {
        ind[i] += (static_cast<IndexValueType>(size[i]) - 1);
      }
      m_EndOffset = m_Image->ComputeOffset(ind);
      ++m_EndOffset;
    }
  }

  static unsigned int
  GetImageIteratorDimension()
  {
    return TImage::ImageDimension;
  }

  bool
  operator==(const Self & it) const
  {
    return (m_Buffer + m_Offset) == (it.m_Buffer + it.m_Offset);
  }
  ITK_UNEQUAL_OPERATOR_MEMBER_FUNCTION(Self);

  bool
  operator<=(const Self & it) const
  {
    return (m_Buffer + m_Offset) <= (it.m_Buffer + it.m_Offset);
  }
  bool
  operator<(const Self & it) const
  {
    return (m_Buffer + m_Offset) < (it.m_Buffer + it.m_Offset);
  }
  bool
  operator>=(const Self & it) const
  {
    return (m_Buffer + m_Offset) >= (it.m_Buffer + it.m_Offset);
  }
  bool
  operator>(const Self & it) const
  {
    return (m_Buffer + m_Offset) > (it.m_Buffer + it.m_Offset);
  }

  [[nodiscard]] IndexType
  ComputeIndex() const
  {
    return m_Image->ComputeIndex(m_Offset);
  }

#ifndef ITK_FUTURE_LEGACY_REMOVE
  ITK_FUTURE_DEPRECATED(
    "Please use `ComputeIndex()` instead, or use an iterator with index, like `ImageIteratorWithIndex`!")
  [[nodiscard]] IndexType
  GetIndex() const
  {
    return this->ComputeIndex();
  }
#endif

  virtual void
  SetIndex(const IndexType & ind)
  {
    m_Offset = m_Image->ComputeOffset(ind);
  }

  [[nodiscard]] const RegionType &
  GetRegion() const
  {
    return m_Region;
  }

  /** Get the image that this iterator walks. Return type tracks VIsConst. */
  [[nodiscard]] ImagePointer
  GetImage() const
  {
    // For VIsConst=false, m_Image is a non-const WeakPointer<ImageType>
    // whose GetPointer() yields ImageType*. For VIsConst=true, it is a
    // ConstWeakPointer whose GetPointer() yields const ImageType*. No
    // const_cast needed.
    return m_Image.GetPointer();
  }

  [[nodiscard]] PixelType
  Get() const
  {
    return m_PixelAccessorFunctor.Get(*(m_Buffer + m_Offset));
  }

  /** Return a reference to the pixel. const-ness tracks VIsConst. */
  [[nodiscard]] std::conditional_t<VIsConst, const PixelType &, PixelType &>
  Value() const
  {
    // Cast away m_Buffer's const-ness only within the const flavor's return
    // path is not needed: on VIsConst=false, m_Buffer is InternalPixelType*,
    // so *(m_Buffer + m_Offset) yields PixelType& directly.
    return *(m_Buffer + m_Offset);
  }

  /** Set the pixel value. Only available when VIsConst=false. */
  template <bool VCopy = VIsConst, std::enable_if_t<!VCopy, int> = 0>
  void
  Set(const PixelType & value) const
  {
    this->m_PixelAccessorFunctor.Set(*(this->m_Buffer + this->m_Offset), value);
  }

  void
  GoToBegin()
  {
    m_Offset = m_BeginOffset;
  }

  void
  GoToEnd()
  {
    m_Offset = m_EndOffset;
  }

  [[nodiscard]] bool
  IsAtBegin() const
  {
    return m_Offset == m_BeginOffset;
  }

  [[nodiscard]] bool
  IsAtEnd() const
  {
    return m_Offset == m_EndOffset;
  }

  /** Internal accessor used by the converting ctor only. */
  InternalPixelPointer
  GetBufferPointerForCopy() const
  {
    return m_Buffer;
  }

protected: // made protected so other iterators can access
  ImageWeakPointerType m_Image{};

  RegionType m_Region{}; // region to iterate over

  OffsetValueType m_Offset{};
  OffsetValueType m_BeginOffset{};
  OffsetValueType m_EndOffset{};

  InternalPixelPointer m_Buffer{};

  AccessorType        m_PixelAccessor{};
  AccessorFunctorType m_PixelAccessorFunctor{};
};

/** Legacy alias: the read-only flavor. */
template <typename TImage>
using ImageConstIterator = ImageIteratorBase<TImage, /*VIsConst=*/true>;

// Deduction guide for class template argument deduction (CTAD).
template <typename TImage>
ImageIteratorBase(SmartPointer<TImage>, const typename TImage::RegionType &)
  -> ImageIteratorBase<std::remove_const_t<TImage>, true>;

} // end namespace itk

#endif
