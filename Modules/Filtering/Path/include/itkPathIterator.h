/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkPathIterator_h
#define itkPathIterator_h

#include "itkPathConstIterator.h"

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
class ITK_TEMPLATE_EXPORT PathIterator : public PathConstIterator<TImage, TPath>
{
public:
  /** Standard class type aliases. */
  using Self = PathIterator;

  /** Dimension of the image the iterator walks.  This constant is needed so
   * that functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  static constexpr unsigned int ImageIteratorDimension = TImage::ImageDimension;

  /** Define the superclass */
  using Superclass = PathConstIterator<TImage, TPath>;

  /** Inherit types from the superclass */
  using IndexType = typename Superclass::IndexType;
  using OffsetType = typename Superclass::OffsetType;
  using SizeType = typename Superclass::SizeType;
  using ImageType = typename Superclass::ImageType;
  using PixelContainer = typename Superclass::PixelContainer;
  using PixelContainerPointer = typename Superclass::PixelContainerPointer;
  using InternalPixelType = typename Superclass::InternalPixelType;
  using PixelType = typename Superclass::PixelType;
  using AccessorType = typename Superclass::AccessorType;
  using PathType = typename Superclass::PathType;
  using PathInputType = typename Superclass::PathInputType;
  using PathOutputType = typename Superclass::PathOutputType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PathIterator, PathConstIterator);

  /** Set the pixel value */
  void
  Set(const PixelType & value)
  {
    // Normally, this would just be the following:
    //   m_Image->SetPixel(m_CurrentImageIndex,value);
    // However, we don't want a warning about m_Image being a ConstPointer
    // in the Superclass.
    const_cast<ImageType *>(this->m_Image.GetPointer())->SetPixel(this->m_CurrentImageIndex, value);
  }

  /** Return a reference to the pixel
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  PixelType &
  Value()
  {
    return this->GetImage()->GetPixel(this->m_ImageIndex);
  }

  /** operator= is provided to make sure the handles to the image and path are
   * properly reference counted. */
  Self &
  operator=(const Self & it);

  /** Constructor establishes an iterator to walk along a path */
  PathIterator(ImageType * imagePtr, const PathType * pathPtr);

  /** Default Destructor. */
  ~PathIterator() override = default;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPathIterator.hxx"
#endif

#endif
