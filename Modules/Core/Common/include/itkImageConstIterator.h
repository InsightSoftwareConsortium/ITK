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
#include "itkWeakPointer.h"
#include <type_traits> // For conditional_t, enable_if_t, remove_const_t.

namespace itk
{
// Forward declaration so that the ImageIteratorBase converting
// constructor can refer to the non-const specialization.
template <typename TImage, bool VIsConst>
class ImageIteratorBase;

/** \class ImageIteratorBase
 * \brief Unified templated base for ImageIterator and ImageConstIterator.
 *
 * ImageIteratorBase collapses the legacy pair of classes
 * ImageConstIterator<TImage> and ImageIterator<TImage> into a single class
 * template parameterized on a compile-time `bool VIsConst` flag. Pointer-like
 * members (m_Image, m_Buffer) flip their element-const qualifier via
 * std::conditional_t so that the non-const specialization (VIsConst=false)
 * no longer needs const_cast to implement Set() / non-const Value().
 *
 * The legacy names `ImageConstIterator` and `ImageIterator` are preserved as
 * alias templates (see the bottom of this header and itkImageIterator.h),
 * so existing consumers compile unchanged.
 *
 * Non-const -> const implicit conversion is provided via a converting
 * constructor; the reverse is disabled by construction (no const_cast
 * escape hatch at this layer).
 *
 * \par MORE INFORMATION
 * For a complete description of the ITK Image Iterators and their API, please
 * see the Iterators chapter in the ITK Software Guide.  The ITK Software Guide
 * is available in print and as a free .pdf download from https://www.itk.org.
 *
 * \ingroup ImageIterators
 *
 * \sa ImageConstIterator \sa ImageIterator
 * \ingroup ITKCommon
 */
template <typename TImage, bool VIsConst>
class ITK_TEMPLATE_EXPORT ImageIteratorBase
{
public:
  /** Compile-time constness flag. */
  static constexpr bool IsConst = VIsConst;

  /** Standard class type aliases. */
  using Self = ImageIteratorBase;

  /** Dimension of the image the iterator walks. */
  static constexpr unsigned int ImageIteratorDimension = TImage::ImageDimension;

  /** \see LightObject::GetNameOfClass() */
  itkVirtualGetNameOfClassMacro(ImageIteratorBase);

  /** Index type alias support */
  using IndexType = typename TImage::IndexType;

  /** Size type alias support */
  using SizeType = typename TImage::SizeType;

  /** Offset type alias support */
  using OffsetType = typename TImage::OffsetType;

  /** Region type alias support */
  using RegionType = typename TImage::RegionType;

  /** Image type alias support */
  using ImageType = TImage;

  /** PixelContainer type alias support. */
  using PixelContainer = typename TImage::PixelContainer;
  using PixelContainerPointer = typename PixelContainer::Pointer;

  /** Internal Pixel Type */
  using InternalPixelType = typename TImage::InternalPixelType;

  /** External Pixel Type */
  using PixelType = typename TImage::PixelType;

  /**  Accessor type that convert data between internal and external
   *  representations. */
  using AccessorType = typename TImage::AccessorType;
  using AccessorFunctorType = typename TImage::AccessorFunctorType;

  /** Pointer-to-internal-pixel type that flips const based on VIsConst.
   * The non-const specialization carries a mutable pointer; no const_cast
   * is required to implement Set() / non-const Value(). */
  using InternalPixelPointer = std::conditional_t<VIsConst, const InternalPixelType *, InternalPixelType *>;

  /** Weak pointer to the image. The const specialization uses
   * ConstWeakPointer (WeakPointer<const TImage>) while the non-const
   * specialization uses WeakPointer<TImage> so that GetImage() can
   * return a non-const pointer without any const_cast. */
  using ImageWeakPointer = std::conditional_t<VIsConst, typename TImage::ConstWeakPointer, WeakPointer<TImage>>;

  /** Raw image pointer type returned by GetImage(). */
  using ImagePointer = std::conditional_t<VIsConst, const TImage *, TImage *>;

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

  /** Converting constructor: non-const -> const.
   * Only enabled when VIsConst==true and the source is the non-const
   * sibling specialization. The reverse (const -> non-const) is not
   * provided, so it is a compile error by construction. */
  template <bool VOtherConst, typename = std::enable_if_t<VIsConst && !VOtherConst>>
  ImageIteratorBase(const ImageIteratorBase<TImage, VOtherConst> & it)
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

  /** Constructor establishes an iterator to walk a particular image and a particular region of that image. Initializes
   * the iterator at the begin of the region. The pointer parameter flips
   * const based on VIsConst. */
  ImageIteratorBase(ImagePointer ptr, const RegionType & region)
    : m_Image(ptr)
    , m_Buffer(ptr->GetBufferPointer())
    , m_PixelAccessor(ptr->GetPixelAccessor())
  {
    SetRegion(region);

    m_PixelAccessorFunctor.SetPixelAccessor(m_PixelAccessor);
    m_PixelAccessorFunctor.SetBegin(m_Buffer);
  }

  /** operator= is provided to make sure the handle to the image is properly
   * reference counted. */
  Self &
  operator=(const Self & it)
  {
    if (this != &it)
    {
      m_Image = it.m_Image; // copy the smart pointer
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

    if (region.GetNumberOfPixels() > 0) // If region is non-empty
    {
      const RegionType & bufferedRegion = m_Image->GetBufferedRegion();
      itkAssertOrThrowMacro((bufferedRegion.IsInside(m_Region)),
                            "Region " << m_Region << " is outside of buffered region " << bufferedRegion);
    }

    // Compute the start offset
    m_Offset = m_Image->ComputeOffset(m_Region.GetIndex());
    m_BeginOffset = m_Offset;

    // Compute the end offset.
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

  /** Get the dimension (size) of the index. */
  static unsigned int
  GetImageIteratorDimension()
  {
    return TImage::ImageDimension;
  }

  /** Comparison operators. */
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

  /** Computes the index. Internally calls ImageBase::ComputeIndex. */
  [[nodiscard]] IndexType
  ComputeIndex() const
  {
    return m_Image->ComputeIndex(m_Offset);
  }

#ifndef ITK_FUTURE_LEGACY_REMOVE
  /** Computes and returns the index. Deprecated. */
  ITK_FUTURE_DEPRECATED(
    "Please use `ComputeIndex()` instead, or use an iterator with index, like `ImageIteratorWithIndex`!")
  [[nodiscard]] IndexType
  GetIndex() const
  {
    return this->ComputeIndex();
  }
#endif

  /** Set the index. No bounds checking is performed. */
  virtual void
  SetIndex(const IndexType & ind)
  {
    m_Offset = m_Image->ComputeOffset(ind);
  }

  /** Get the region that this iterator walks. */
  [[nodiscard]] const RegionType &
  GetRegion() const
  {
    return m_Region;
  }

  /** Get the image that this iterator walks.
   * Always callable; returns a pointer-to-const for the const
   * specialization and a non-const pointer for the mutating
   * specialization. */
  [[nodiscard]] ImagePointer
  GetImage() const
  {
    return m_Image.GetPointer();
  }

  /** Get the pixel value. */
  [[nodiscard]] PixelType
  Get() const
  {
    return m_PixelAccessorFunctor.Get(*(m_Buffer + m_Offset));
  }

  /** Return a const reference to the pixel. Available in both
   * specializations. */
  [[nodiscard]] const PixelType &
  Value() const
  {
    return *(m_Buffer + m_Offset);
  }

  /** Return a mutable reference to the pixel. SFINAE-gated on
   * !VIsConst. No const_cast is needed because m_Buffer itself
   * is a non-const pointer in the non-const specialization. */
  template <bool VCopy = VIsConst, std::enable_if_t<!VCopy, int> = 0>
  PixelType &
  Value()
  {
    return *(m_Buffer + m_Offset);
  }

  /** Set the pixel value. SFINAE-gated on !VIsConst. */
  template <bool VCopy = VIsConst, std::enable_if_t<!VCopy, int> = 0>
  void
  Set(const PixelType & value) const
  {
    this->m_PixelAccessorFunctor.Set(*(m_Buffer + m_Offset), value);
  }

  /** Move an iterator to the beginning of the region. */
  void
  GoToBegin()
  {
    m_Offset = m_BeginOffset;
  }

  /** Move an iterator to the end of the region. */
  void
  GoToEnd()
  {
    m_Offset = m_EndOffset;
  }

  /** Is the iterator at the beginning of the region? */
  [[nodiscard]] bool
  IsAtBegin() const
  {
    return m_Offset == m_BeginOffset;
  }

  /** Is the iterator at the end of the region? */
  [[nodiscard]] bool
  IsAtEnd() const
  {
    return m_Offset == m_EndOffset;
  }

  // Grant the sibling specialization access to private/protected members
  // so the non-const -> const converting constructor can initialize from
  // the non-const source.
  template <typename, bool>
  friend class ImageIteratorBase;

protected: // made protected so other iterators can access
  ImageWeakPointer m_Image{};

  RegionType m_Region{}; // region to iterate over

  OffsetValueType m_Offset{};
  OffsetValueType m_BeginOffset{}; // offset to first pixel in region
  OffsetValueType m_EndOffset{};   // offset to one pixel past last pixel in region

  InternalPixelPointer m_Buffer{};

  AccessorType        m_PixelAccessor{};
  AccessorFunctorType m_PixelAccessorFunctor{};
};

/** \class ImageConstIterator
 * \brief A multi-dimensional image iterator templated over image type.
 *
 * ImageConstIterator is preserved as an alias template over
 * ImageIteratorBase<TImage, /\*VIsConst=*\/true> so existing consumers
 * compile unchanged.
 *
 * \ingroup ImageIterators
 * \ingroup ITKCommon
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ImageConstIterator : public ImageIteratorBase<TImage, /*VIsConst=*/true>
{
public:
  using Superclass = ImageIteratorBase<TImage, /*VIsConst=*/true>;
  using Superclass::Superclass;
};

template <typename TImage>
ImageConstIterator(SmartPointer<TImage>, const typename TImage::RegionType &)
  -> ImageConstIterator<std::remove_const_t<TImage>>;

template <typename TImage>
ImageConstIterator(TImage *, const typename TImage::RegionType &) -> ImageConstIterator<TImage>;

template <typename TImage>
ImageConstIterator(const TImage *, const typename TImage::RegionType &) -> ImageConstIterator<TImage>;

// Deduction guide for class template argument deduction (CTAD).
// When the user writes `ImageConstIterator(ptr, region)` or
// `ImageIterator(ptr, region)`, alias-template CTAD (P1814,
// supported by Clang as a C++17 extension) synthesizes a guide for
// each alias by adapting this class-template guide; adaptations
// whose returned target does not match the alias's VIsConst pin
// are silently discarded. Because `std::is_const_v<TImage>` in the
// returned type is a *dependent* expression (not a fixed bool), the
// synthesis step leaves it as deducible and both alias specializations
// can adapt it successfully (ImageConstIterator pins true,
// ImageIterator pins false).
template <typename TImage, bool VIsConst = std::is_const_v<TImage>>
ImageIteratorBase(SmartPointer<TImage>, const typename TImage::RegionType &)
  -> ImageIteratorBase<std::remove_const_t<TImage>, VIsConst>;

} // end namespace itk

#endif
