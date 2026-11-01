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
#ifndef itkPathIterator_h
#define itkPathIterator_h


#include "itkImage.h"
#include "itkPath.h"

#include "itkImageIterator.h"
#include <type_traits>

namespace itk
{
/**
 * \class PathIterator
 * \brief PathIterator iterates (traces) over a path through an image.
 *
 * This iterator visits only those indices of the image which are overlapped by
 * the path.  All indices are visited in path order.  If a path crosses itself
 * at an index, that index of the image will be visited twice.  This class add
 * write-access to the functionality of the PathConstIterator.
 *
 * \par MORE INFORMATION
 * For a complete description of the ITK Image Iterators and their API, please
 * see the Iterators chapter in the ITK Software Guide.  The ITK Software Guide
 * is available in print and as a free .pdf download from https://www.itk.org.
 *
 * The constness of TImage selects the access: PathIterator<const TImage>
 * (equivalently PathConstIterator<TImage>) is read-only.
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
 * \sa NeighborhoodIterator \sa PathConstIterator
 * \sa ShapedNeighborhoodIterator  \sa SliceIterator
 * \sa ImageConstIteratorWithIndex
 *
 * \ingroup PathObjects
 * \ingroup ITKPath
 */
template <typename TImage, typename TPath>
class ITK_TEMPLATE_EXPORT PathIterator
{
  template <typename, typename>
  friend class PathIterator;

public:
  /** Standard class type aliases. */
  using Self = PathIterator;

  /** Dimension of the image the iterator walks.  This constant is needed so
   * that functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  static constexpr unsigned int ImageIteratorDimension = TImage::ImageDimension;

  /** Index type alias support */
  using IndexType = typename TImage::IndexType;

  /** Offset type alias support */
  using OffsetType = typename TImage::OffsetType;

  /** Size type alias support */
  using SizeType = typename TImage::SizeType;

  /** Region type alias support */
  using RegionType = typename TImage::RegionType;

  /** Spacing type alias support */
  using SpacingType = typename TImage::SpacingType;

  /** Origin type alias support */
  using PointType = typename TImage::PointType;

  /** Image type alias support */
  using ImageType = std::remove_const_t<TImage>;

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

  /** Path type alias support */
  using PathType = TPath;

  /** Path 1D Input Type */
  using PathInputType = typename PathType::InputType;

  /** Path ND Output Type, which is not necessarily an index type */
  using PathOutputType = typename PathType::OutputType;

  /** \see LightObject::GetNameOfClass() */
  itkVirtualGetNameOfClassMacro(PathIterator);

  /** Get the dimension (size) of the index. */
  static unsigned int
  GetImageIteratorDimension()
  {
    return TImage::ImageDimension;
  }

  /** Get the input. This provides a read only reference to the input. */
  const PathInputType
  GetPathPosition()
  {
    return m_CurrentPathPosition;
  }

  /** Get the index. This provides a read only reference to the index. */
  const IndexType
  GetIndex()
  {
    return m_CurrentImageIndex;
  }

  /** Get the pixel value */
  [[nodiscard]] const PixelType &
  Get() const
  {
    return m_Image->GetPixel(m_CurrentImageIndex);
  }

  /** Is the iterator at the end of the path?
   * Note that for a closed path, it may be possible to increment back to the
   * start of the path. */
  [[nodiscard]] bool
  IsAtEnd() const
  {
    return m_IsAtEnd;
  }

  /** Should GoToBegin() initially skip the first index of a closed path so that
   * the first index will only be visited once--at the end of the path?  If set
   * to false, then GoToBegin() will always move to the 1'st index.  The
   * constructor presets m_VisitStartIndexAsLastIndexIfClosed to true. */
  inline virtual void
  VisitStartIndexAsLastIndexIfClosed(bool flag)
  {
    m_VisitStartIndexAsLastIndexIfClosed = flag;
  }

  /** Move an iterator to the beginning of the path.  If the starting and ending
   * indices of the path are coincident, then move to the 2'nd index of the
   * path, since the 1'st index will be visited later as the last index.
   * However, if m_VisitStartIndexAsLastIndexIfClosed is false, then GoToBegin()
   * will always move to the 1'st index. */
  void
  GoToBegin();

  /** Walk forward along the path to the next index in the image. */
  void
  operator++();

  /** Set the pixel value. Available only when TImage is not const. */
#ifdef __cpp_concepts
  void
  Set(const PixelType & value)
    requires(!std::is_const_v<TImage>)
#else
  template <typename T = TImage, std::enable_if_t<std::is_same_v<T, TImage> && !std::is_const_v<T>, int> = 0>
  void
  Set(const PixelType & value)
#endif
  {
    m_Image->SetPixel(m_CurrentImageIndex, value);
  }

  /** Return a reference to the pixel. Available only when TImage is not const.
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
#ifdef __cpp_concepts
  PixelType &
  Value()
    requires(!std::is_const_v<TImage>)
#else
  template <typename T = TImage, std::enable_if_t<std::is_same_v<T, TImage> && !std::is_const_v<T>, int> = 0>
  PixelType &
  Value()
#endif
  {
    return m_Image->GetPixel(m_CurrentImageIndex);
  }


  /** Conversion from a writable iterator to a read-only iterator. */
  template <typename TOtherImage, std::enable_if_t<detail::IsImageIteratorConvertible_v<TOtherImage, TImage>, int> = 0>
  PathIterator(const PathIterator<TOtherImage, TPath> & it)
    : m_ZeroOffset(it.m_ZeroOffset)
    , m_Image(it.m_Image)
    , m_Path(it.m_Path)
    , m_Region(it.m_Region)
    , m_ImageOrigin(it.m_ImageOrigin)
    , m_ImageSpacing(it.m_ImageSpacing)
    , m_ImageSize(it.m_ImageSize)
    , m_VisitStartIndexAsLastIndexIfClosed(it.m_VisitStartIndexAsLastIndexIfClosed)
    , m_IsAtEnd(it.m_IsAtEnd)
    , m_CurrentPathPosition(it.m_CurrentPathPosition)
    , m_CurrentImageIndex(it.m_CurrentImageIndex)
  {}

  /** operator= is provided to make sure the handles to the image and path are
   * properly reference counted. */
  Self &
  operator=(const Self & it);

  /** Constructor establishes an iterator to walk along a path */
  PathIterator(TImage * imagePtr, const PathType * pathPtr);

  /** Default Destructor. */
  virtual ~PathIterator() = default;

protected: // made protected so other iterators can access
  // This "constant" is initialized in the constructor
  OffsetType m_ZeroOffset{}; // = 0 for all dimensions

  /** Smart pointer to the source image. */
  WeakPointer<TImage> m_Image{};

  /** Smart pointer to the path we're following */
  typename PathType::ConstPointer m_Path{};

  /** Region type to iterate over. */
  RegionType m_Region{};

  /** The origin of the source image */
  PointType m_ImageOrigin{};

  /** The spacing of the source image */
  SpacingType m_ImageSpacing{};

  /** Size of the source image */
  const SizeValueType * m_ImageSize{};

  /** Should GoToBegin() initially skip the first index of a closed path so that
   * the first index will only be visited once--at the end of the path?  If
   * false, then GoToBegin() will always move to the 1'st index.  The default
   * value is true, which is set the constructor. */
  bool m_VisitStartIndexAsLastIndexIfClosed{};

  /** Is the iterator at the end of its walk? */
  bool m_IsAtEnd{};

  /** Current 1D position along the path, such as time or arc length */
  PathInputType m_CurrentPathPosition{};

  /** Current ND index position in the image of the path */
  IndexType m_CurrentImageIndex{};
};

/**
 * \class PathConstIterator
 * \brief PathConstIterator iterates (traces) over a path through an image.
 *
 * This iterator visits only those indices of the image that are overlapped by
 * a specified 1D path.  All indices are visited in path order.  If a path
 * crosses itself at an index, that index of the image will be visited twice.
 * An exception to this rule is that if the path is closed, i.e. its starting
 * and ending indices are coincident.  When starting and ending indices are
 * coincident, GoToBegin() will go to the second index, since the "first" index
 * will be visited later as the "last" index.  This is so that paths
 * (especially parametric paths) can be properly closed, without
 * double-visiting the starting/ending point.  This behavior can be overridden
 * by calling VisitStartIndexAsLastIndexIfClosed(false) before calling
 * GoToBegin().  This class is the const version of the PathIterator, and for
 * this reason it doesn't support the Set() method.
 *
 * \par MORE INFORMATION
 * For a complete description of the ITK Image Iterators and their API, please
 * see the Iterators chapter in the ITK Software Guide.  The ITK Software Guide
 * is available in print and as a free .pdf download from https://www.itk.org.
 *
 * \ingroup PathObjects
 * \ingroup ImageIterators
 * \ingroup ITKPath
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
 * \sa NeighborhoodIterator \sa PathIterator \sa ShapedNeighborhoodIterator
 * \sa SliceIterator \sa ImageConstIteratorWithIndex
 */
template <typename TImage, typename TPath>
class ITK_TEMPLATE_EXPORT PathConstIterator : public PathIterator<std::add_const_t<TImage>, TPath>
{
public:
  /** Standard class type alias. */
  using Self = PathConstIterator;
  using Superclass = PathIterator<std::add_const_t<TImage>, TPath>;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(PathConstIterator);

  using Superclass::Superclass;

  /** Conversion from PathIterator<const TImage>. */
  PathConstIterator(const Superclass & it)
    : Superclass(it)
  {}
};

// Deduction guide for class template argument deduction (CTAD).
template <typename TImage, typename TPath>
PathConstIterator(const TImage *, const TPath *) -> PathConstIterator<TImage, TPath>;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPathIterator.hxx"
#endif

#endif
