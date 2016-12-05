/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkPathConstIterator_h
#define itkPathConstIterator_h

#include "itkImage.h"
#include "itkPath.h"

namespace itk
{
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
template< typename TImage, typename TPath >
class ITK_TEMPLATE_EXPORT PathConstIterator
{
public:
  /** Standard class typedefs. */
  typedef PathConstIterator Self;

  /** Dimension of the image the iterator walks.  This constant is needed so
   * that functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  itkStaticConstMacro(ImageIteratorDimension, unsigned int,
                      TImage::ImageDimension);

  /** Index typedef support. */
  typedef typename TImage::IndexType      IndexType;

  /** Offset typedef support. */
  typedef typename TImage::OffsetType      OffsetType;

  /** Size typedef support. */
  typedef typename TImage::SizeType      SizeType;

  /** Region typedef support */
  typedef typename TImage::RegionType RegionType;

  /** Spacing typedef support */
  typedef typename TImage::SpacingType SpacingType;

  /** Origin typedef support */
  typedef typename TImage::PointType PointType;

  /** Image typedef support. */
  typedef TImage ImageType;

  /** PixelContainer typedef support. Used to refer to the container for
   * the pixel data. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc. */
  typedef typename TImage::PixelContainer  PixelContainer;
  typedef typename PixelContainer::Pointer PixelContainerPointer;

  /** Internal Pixel Type */
  typedef typename TImage::InternalPixelType InternalPixelType;

  /** External Pixel Type */
  typedef typename TImage::PixelType PixelType;

  /**  Accessor type that convert data between internal and external
   *  representations. */
  typedef typename TImage::AccessorType AccessorType;

  /** Path typedef support */
  typedef TPath PathType;

  /** Path 1D Input Type */
  typedef typename PathType::InputType PathInputType;

  /** Path ND Output Type, which is not necessarily an index type */
  typedef typename PathType::OutputType PathOutputType;

  /** Run-time type information (and related methods). */
  itkTypeMacroNoParent(PathConstIterator);

  /** Get the dimension (size) of the index. */
  static unsigned int GetImageIteratorDimension()
  {
    return TImage::ImageDimension;
  }

  /** Get the input. This provides a read only reference to the input. */
  const PathInputType GetPathPosition()
  {
    return m_CurrentPathPosition;
  }

  /** Get the index. This provides a read only reference to the index. */
  const IndexType GetIndex()
  {
    return m_CurrentImageIndex;
  }

  /** Get the pixel value */
  const PixelType & Get(void) const
  {
    return m_Image->GetPixel(m_CurrentImageIndex);
  }

  /** Is the iterator at the end of the path?
   * Note that for a closed path, it may be possible to increment back to the
   * start of the path. */
  bool IsAtEnd()
  {
    return m_IsAtEnd;
  }

  /** Should GoToBegin() initially skip the first index of a closed path so that
   * the first index will only be visited once--at the end of the path?  If set
   * to false, then GoToBegin() will always move to the 1'st index.  The
   * constructor presets m_VisitStartIndexAsLastIndexIfClosed to true. */
  inline virtual void VisitStartIndexAsLastIndexIfClosed(bool flag)
  {
    m_VisitStartIndexAsLastIndexIfClosed = flag;
  }

  /** Move an iterator to the beginning of the path.  If the starting and ending
   * indices of the path are coincident, then move to the 2'nd index of the
   * path, since the 1'st index will be visited later as the last index.
   * However, if m_VisitStartIndexAsLastIndexIfClosed is false, then GoToBegin()
   * will always move to the 1'st index. */
  void GoToBegin();

  /** Walk forward along the path to the next index in the image. */
  void operator++();

  /** operator= is provided to make sure the handles to the image and path are
   * properly reference counted. */
  Self & operator=(const Self & it);

  /** Constructor establishes an iterator to walk along a path */
  PathConstIterator(const ImageType *imagePtr, const PathType  *pathPtr);

  /** Default Destructor. */
  virtual ~PathConstIterator() {}

protected: //made protected so other iterators can access
  // This "constant" is initialized in the constructor
  OffsetType m_ZeroOffset;  // = 0 for all dimensions

  /** Smart pointer to the source image. */
  typename ImageType::ConstWeakPointer m_Image;

  /** Smart pointer to the path we're following */
  typename PathType::ConstPointer m_Path;

  /** Region type to iterate over. */
  RegionType m_Region;

  /** The origin of the source image */
  PointType m_ImageOrigin;

  /** The spacing of the source image */
  SpacingType m_ImageSpacing;

  /** Size of the source image */
  const SizeValueType *m_ImageSize;

  /** Should GoToBegin() initially skip the first index of a closed path so that
   * the first index will only be visited once--at the end of the path?  If
   * false, then GoToBegin() will always move to the 1'st index.  The default
   * value is true, which is set the constructor. */
  bool m_VisitStartIndexAsLastIndexIfClosed;

  /** Is the iterator at the end of its walk? */
  bool m_IsAtEnd;

  /** Current 1D position along the path, such as time or arc length */
  PathInputType m_CurrentPathPosition;

  /** Current ND index position in the image of the path */
  IndexType m_CurrentImageIndex;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPathConstIterator.hxx"
#endif

#endif
