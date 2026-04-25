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
#ifndef itkImageReverseConstIterator_h
#define itkImageReverseConstIterator_h

#include "itkSize.h"
#include "itkImageConstIterator.h"
#include "itkWeakPointer.h"
#include <memory>
#include <type_traits> // For conditional_t, enable_if_t, remove_const_t.

namespace itk
{
/** \class ImageReverseConstIterator
 * \brief Multi-dimensional image iterator.
 *
 * ImageReverseConstIterator is a templated class to represent a
 * multi-dimensional iterator. ImageReverseConstIterator is templated over the
 * dimension of the image and the data type of the image.
 *
 * ImageReverseConstIterator is a base class for all the reverse image
 * iterators. It provides the basic construction and comparison
 * operations.  However, it does not provide mechanisms for moving the
 * iterator.  A subclass of ImageReverseConstIterator must be used to move
 * the iterator.
 *
 * ImageReverseConstIterator is a multi-dimensional iterator, requiring
 * more information be specified before the iterator can be used than
 * conventional iterators.  Whereas the std::vector::iterator from the
 * STL only needs to be passed a pointer to establish the iterator,
 * the multi-dimensional image iterator needs a pointer, the size of
 * the buffer, the size of the region, the start index of the buffer,
 * and the start index of the region. To gain access to this
 * information, ImageReverseConstIterator holds a reference to the image
 * over which it is traversing.
 *
 * ImageReverseConstIterator assumes a particular layout of the image
 * data. In particular, the data is arranged in a 1D array as if it
 * were [][][][slice][row][col] with Index[0] = col, Index[1] = row,
 * Index[2] = slice, etc.
 *
 * \par MORE INFORMATION
 * For a complete description of the ITK Image Iterators and their API, please
 * see the Iterators chapter in the ITK Software Guide.  The ITK Software Guide
 * is available in print and as a free .pdf download from https://www.itk.org.
 *
 * \ingroup ImageIterators
 *
 * \sa ImageConstIterator \sa ConditionalConstIterator
 * \sa ConstNeighborhoodIterator \sa ConstShapedNeighborhoodIterator
 * \sa ConstSliceIterator  \sa CorrespondenceDataStructureIterator
 * \sa FloodFilledFunctionConditionalConstIterator
 * \sa FloodFilledImageFunctionConditionalConstIterator
 * \sa FloodFilledImageFunctionConditionalIterator
 * \sa FloodFilledSpatialFunctionConditionalConstIterator
 * \sa FloodFilledSpatialFunctionConditionalIterator
 * \sa ImageConstIterator \sa ImageConstIteratorWithIndex
 * \sa ImageIterator \sa ImageIteratorWithIndex
 * \sa ImageLinearConstIteratorWithIndex  \sa ImageLinearIteratorWithIndex
 * \sa ImageRandomConstIteratorWithIndex  \sa ImageRandomIteratorWithIndex
 * \sa ImageRegionConstIterator \sa ImageRegionConstIteratorWithIndex
 * \sa ImageRegionExclusionConstIteratorWithIndex
 * \sa ImageRegionExclusionIteratorWithIndex
 * \sa ImageRegionIterator  \sa ImageRegionIteratorWithIndex
 * \sa ImageRegionReverseConstIterator  \sa ImageRegionReverseIterator
 * \sa ImageReverseConstIterator  \sa ImageReverseIterator
 * \sa ImageSliceConstIteratorWithIndex  \sa ImageSliceIteratorWithIndex
 * \sa NeighborhoodIterator \sa PathConstIterator  \sa PathIterator
 * \sa ShapedNeighborhoodIterator  \sa SliceIterator
 * \sa ImageConstIteratorWithIndex
 * \ingroup ITKCommon
 */
template <typename TImage, bool VIsConst>
class ITK_TEMPLATE_EXPORT ImageReverseIteratorBase
{
public:
  /** Standard class type aliases. */
  using Self = ImageReverseIteratorBase;

  /** Dimension of the image the iterator walks.  This constant is needed so
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  static constexpr unsigned int ImageIteratorDimension = TImage::ImageDimension;

  /** \see LightObject::GetNameOfClass() */
  itkVirtualGetNameOfClassMacro(ImageReverseIteratorBase);

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

  /** PixelContainer type alias support Used to refer to the container for
   * the pixel data. While this was already typedef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc. */
  using PixelContainer = typename TImage::PixelContainer;
  using PixelContainerPointer = typename PixelContainer::Pointer;

  /** Internal Pixel Type */
  using InternalPixelType = typename TImage::InternalPixelType;

  /** External Pixel Type */
  using PixelType = typename TImage::PixelType;

  /**  Accessor type that convert data between internal and external
   *  representations. */
  using AccessorType = typename TImage::AccessorType;

  /** Functor to choose the appropriate accessor. (for Image vs VectorImage) */
  using AccessorFunctorType = typename TImage::AccessorFunctorType;

  using InternalPixelPointer = std::conditional_t<VIsConst, const InternalPixelType *, InternalPixelType *>;
  using ImageWeakPointer = std::conditional_t<VIsConst, typename TImage::ConstWeakPointer, WeakPointer<TImage>>;
  using ImagePointer = std::conditional_t<VIsConst, const TImage *, TImage *>;

  /** Default Constructor. Need to provide a default constructor since we
   * provide a copy constructor. */
  ImageReverseIteratorBase()
    : m_Buffer(nullptr)
    , m_PixelAccessor()
    , m_PixelAccessorFunctor()
  {
    m_PixelAccessorFunctor.SetBegin(m_Buffer);
  }

  /** Default Destructor. */
  virtual ~ImageReverseIteratorBase() = default;

  /** Copy Constructor. The copy constructor is provided to make sure the
   * handle to the image is properly reference counted. */
  ImageReverseIteratorBase(const Self & it)
    : m_Image(it.m_Image)
    , m_Region(it.m_Region)
    , m_Offset(it.m_Offset)
    , m_BeginOffset(it.m_BeginOffset)
    , m_EndOffset(it.m_EndOffset)
    , m_Buffer(it.m_Buffer)
    , m_PixelAccessor(it.m_PixelAccessor)
    , m_PixelAccessorFunctor(it.m_PixelAccessorFunctor)
  {
    // copy the smart pointer


    m_PixelAccessorFunctor.SetBegin(m_Buffer);
  }

  /** Converting constructor: non-const -> const, disabled the other way. */
  template <bool VOtherConst, typename = std::enable_if_t<VIsConst && !VOtherConst>>
  ImageReverseIteratorBase(const ImageReverseIteratorBase<TImage, VOtherConst> & it)
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
   * the iterator at the begin of the region. */
  ImageReverseIteratorBase(ImagePointer ptr, const RegionType & region)
    : m_Image(ptr)
    , m_Region(region)
    , m_Buffer(m_Image->GetBufferPointer())
  {


    // Compute the end offset, one pixel before the first pixel
    SizeValueType offset = m_Image->ComputeOffset(m_Region.GetIndex());
    m_EndOffset = offset - 1;

    // Compute the begin offset, the last pixel in the region
    IndexType ind(m_Region.GetIndex());
    SizeType  size(m_Region.GetSize());
    for (unsigned int i = 0; i < TImage::ImageDimension; ++i)
    {
      ind[i] += (size[i] - 1);
    }
    m_BeginOffset = m_Image->ComputeOffset(ind);
    m_Offset = m_BeginOffset;

    m_PixelAccessor = ptr->GetPixelAccessor();
    m_PixelAccessorFunctor.SetPixelAccessor(m_PixelAccessor);
    m_PixelAccessorFunctor.SetBegin(m_Buffer);
  }

  /** Constructor that can be used to cast from an ImageConstIterator to an
   * ImageRegionReverseIterator. Many routines return an ImageConstIterator
   * but for a particular task, you may want an
   * ImageRegionReverseIterator.  Rather than provide overloaded APIs
   * that return different types of Iterators, itk returns
   * ImageConstIterators and uses constructors to cast from an
   * ImageConstIterator to a ImageRegionReverseIterator. */
  ImageReverseIteratorBase(const ImageIteratorBase<TImage, VIsConst> & it)
    : m_Image(it.GetImage())
    , m_Region(it.GetRegion())
    , m_EndOffset(m_Image->ComputeOffset(m_Region.GetIndex()) - 1)
    , m_Buffer(m_Image->GetBufferPointer())
  {


    const IndexType ind = it.ComputeIndex();

    m_Offset = m_Image->ComputeOffset(ind);

    // Compute the end offset, one pixel before the first pixel


    // Compute the begin offset, the last pixel in the region
    IndexType regInd(m_Region.GetIndex());
    SizeType  regSize(m_Region.GetSize());
    for (unsigned int i = 0; i < TImage::ImageDimension; ++i)
    {
      regInd[i] += (regSize[i] - 1);
    }
    m_BeginOffset = m_Image->ComputeOffset(regInd);

    m_PixelAccessor = m_Image->GetPixelAccessor();
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
      m_PixelAccessorFunctor.SetPixelAccessor(m_PixelAccessor);
      m_PixelAccessorFunctor.SetBegin(m_Buffer);
    }
    return *this;
  }

  /** operator= is provided to make sure the handle to the image is properly
   * reference counted. */
  Self &
  operator=(const ImageIteratorBase<TImage, VIsConst> & it)
  {
    m_Image = it.GetImage();
    m_Region = it.GetRegion();
    m_Buffer = m_Image->GetBufferPointer();

    IndexType ind = it.GetIndex();

    m_Offset = m_Image->ComputeOffset(ind);

    // Compute the end offset, one pixel before the first pixel
    m_EndOffset = m_Image->ComputeOffset(m_Region.GetIndex()) - 1;

    // Compute the begin offset, the last pixel in the region
    IndexType regInd(m_Region.GetIndex());
    SizeType  regSize(m_Region.GetSize());
    for (unsigned int i = 0; i < TImage::ImageDimension; ++i)
    {
      regInd[i] += (regSize[i] - 1);
    }
    m_BeginOffset = m_Image->ComputeOffset(regInd);

    m_PixelAccessor = m_Image->GetPixelAccessor();
    m_PixelAccessorFunctor.SetPixelAccessor(m_PixelAccessor);
    m_PixelAccessorFunctor.SetBegin(m_Buffer);
    return *this;
  }

  template <typename, bool>
  friend class ImageReverseIteratorBase;

  /** Get the dimension (size) of the index. */
  static unsigned int
  GetImageIteratorDimension()
  {
    return TImage::ImageDimension;
  }

  /** Comparison operator. Two iterators are the same if they "point to" the
   * same memory location */
  bool
  operator==(const Self & it) const
  {
    // two iterators are the same if they "point to" the same memory location
    return (m_Buffer + m_Offset) == (it.m_Buffer + it.m_Offset);
  }

  ITK_UNEQUAL_OPERATOR_MEMBER_FUNCTION(Self);


  /** Get the index. This provides a read only reference to the index.
   * This causes the index to be calculated from pointer arithmetic and is
   * therefore an expensive operation.
   * \sa SetIndex */
  const IndexType
  GetIndex()
  {
    return m_Image->ComputeIndex(m_Offset);
  }

  /** Set the index. No bounds checking is performed.
   * \sa GetIndex */
  virtual void
  SetIndex(const IndexType & ind)
  {
    m_Offset = m_Image->ComputeOffset(ind);
  }

  /** Get the region that this iterator walks. ImageReverseConstIterators know the
   * beginning and the end of the region of the image to iterate over. */
  [[nodiscard]] const RegionType &
  GetRegion() const
  {
    return m_Region;
  }

  /** Get the pixel value */
  [[nodiscard]] const PixelType
  Get() const
  {
    return m_PixelAccessorFunctor.Get(*(m_Buffer + m_Offset));
  }

  /** Return a const reference to the pixel
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  [[nodiscard]] const PixelType &
  Value() const
  {
    return *(m_Buffer + m_Offset);
  }

  /** Return a mutable reference to the pixel. SFINAE-gated on !VIsConst. */
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

  /** Move an iterator to the beginning of the region. "Begin" for a reverse
   * iterator is the last pixel in the region. */
  void
  GoToBegin()
  {
    m_Offset = m_BeginOffset;
  }

  /** Move an iterator to the end of the region. "End" for a reverse iterator
   * is defined as one pixel before the first pixel in the region. */
  void
  GoToEnd()
  {
    m_Offset = m_EndOffset;
  }

  /** Is the iterator at the beginning of the (reverse) region? "Begin" for
   * a reverse iterator is the last pixel in the region. */
  [[nodiscard]] bool
  IsAtBegin() const
  {
    return m_Offset == m_BeginOffset;
  }

  /** Is the iterator at the end of the (reverse) region? "End" for a reverse
   * iterator is one pixel before the first pixel in the region. */
  [[nodiscard]] bool
  IsAtEnd() const
  {
    return m_Offset == m_EndOffset;
  }

protected: // made protected so other iterators can access
  ImageWeakPointer m_Image{};

  RegionType m_Region{}; // region to iterate over

  SizeValueType m_Offset{};
  SizeValueType m_BeginOffset{}; // offset to last pixel in region
  SizeValueType m_EndOffset{};   // offset to one pixel before first pixel

  InternalPixelPointer m_Buffer{};

  AccessorType        m_PixelAccessor{};
  AccessorFunctorType m_PixelAccessorFunctor{};
};

template <typename TImage>
class ITK_TEMPLATE_EXPORT ImageReverseConstIterator : public ImageReverseIteratorBase<TImage, /*VIsConst=*/true>
{
public:
  using Superclass = ImageReverseIteratorBase<TImage, /*VIsConst=*/true>;
  using Superclass::Superclass;
};

template <typename TImage>
ImageReverseConstIterator(SmartPointer<TImage>, const typename TImage::RegionType &)
  -> ImageReverseConstIterator<std::remove_const_t<TImage>>;

template <typename TImage>
ImageReverseConstIterator(TImage *, const typename TImage::RegionType &) -> ImageReverseConstIterator<TImage>;

template <typename TImage>
ImageReverseConstIterator(const TImage *, const typename TImage::RegionType &) -> ImageReverseConstIterator<TImage>;

// Deduction guide for class template argument deduction (CTAD).
template <typename TImage, bool VIsConst = std::is_const_v<TImage>>
ImageReverseIteratorBase(SmartPointer<TImage>, const typename TImage::RegionType &)
  -> ImageReverseIteratorBase<std::remove_const_t<TImage>, VIsConst>;

template <typename TImage>
ImageReverseIteratorBase(TImage *, const typename TImage::RegionType &)
  -> ImageReverseIteratorBase<TImage, /*VIsConst=*/false>;

template <typename TImage>
ImageReverseIteratorBase(const TImage *, const typename TImage::RegionType &)
  -> ImageReverseIteratorBase<TImage, /*VIsConst=*/true>;

} // end namespace itk

#endif
