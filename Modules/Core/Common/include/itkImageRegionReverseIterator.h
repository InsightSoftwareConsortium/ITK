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
#ifndef itkImageRegionReverseIterator_h
#define itkImageRegionReverseIterator_h

#include "itkImageReverseIterator.h"
#include "itkImageRegionIterator.h"
#include <type_traits> // For remove_const_t.

namespace itk
{
/**
 * \class ImageRegionReverseIterator
 * \brief A multi-dimensional image iterator designed to walk a specified image
 * region in reverse.
 *
 * ImageRegionReverseIterator is a templated class to represent a
 * multi-dimensional iterator. ImageRegionReverseIterator is templated
 * over the dimension of the image and the data type of the
 * image. ImageRegionReverseIterator is constrained to walk only
 * within the specified region.
 *
 * ImageRegionReverseIterator is a multi-dimensional iterator,
 * requiring more information be specified before the iterator can be
 * used than conventional iterators. Whereas the std::vector::iterator
 * from the STL only needs to be passed a pointer to establish the
 * iterator, the multi-dimensional image iterator needs a pointer, the
 * size of the buffer, the size of the region, the start index of the
 * buffer, and the start index of the region. To gain access to this
 * information, ImageRegionReverseIterator holds a reference to the
 * image over which it is traversing.
 *
 * ImageRegionReverseIterator assumes a particular layout of the image
 * data. The is arranged in a 1D array as if it were
 * [][][][slice][row][col] with Index[0] = col, Index[1] = row,
 * Index[2] = slice, etc.
 *
 * operator++ provides a simple syntax for walking around a region of
 * a multidimensional image. For a reverse iterator, operator++ moves
 * backwards through the region. operator++ iterates across a row,
 * constraining the movement to within a region of image. When the
 * iterator reaches the boundary of the region along a row, the
 * iterator automatically wraps to the previous row, starting at the last
 * pixel in the row that is part of the region. This allows for simple
 * processing loops of the form:
 *
   \code

        it.GoToBegin();
        for (; !it.IsAtEnd(); ++it)
           {
           std::cout << it.Value() << std::endl;
           }

    \endcode
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
template <typename TImage>
class ITK_TEMPLATE_EXPORT ImageRegionReverseIterator : public ImageReverseIterator<TImage>
{
  template <typename>
  friend class ImageRegionReverseIterator;

public:
  /** Standard class type aliases. */
  using Self = ImageRegionReverseIterator;
  using Superclass = ImageReverseIterator<TImage>;

  /** Index type alias support While this was already typedef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc. */
  using typename Superclass::IndexType;

  /** Size type alias support While this was already typedef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc. */
  using typename Superclass::SizeType;

  /** Offset type alias support While this was already typedef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc. */
  using typename Superclass::OffsetType;

  /** Region type alias support */
  using typename Superclass::RegionType;

  /** Image type alias support While this was already typedef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc. */
  using typename Superclass::ImageType;

  /** PixelContainer type alias support Used to refer to the container for
   * the pixel data. While this was already typedef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc. */
  using typename Superclass::PixelContainer;
  using PixelContainerPointer = typename PixelContainer::Pointer;

  /** Internal Pixel Type */
  using typename Superclass::InternalPixelType;

  /** External Pixel Type */
  using typename Superclass::PixelType;

  /**  Accessor type that convert data between internal and external
   *  representations. */
  using typename Superclass::AccessorType;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(ImageRegionReverseIterator);

  /** Default constructor. */
  ImageRegionReverseIterator()
    : Superclass()
  {}

  /** Constructor establishes an iterator to walk a particular image and a particular region of that image. Initializes
   * the iterator at the begin of the region. */
  ImageRegionReverseIterator(TImage * ptr, const RegionType & region)
    : Superclass(ptr, region)
  {}

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageRegionReverseIterator. Many routines return an ImageIterator
   * but for a particular task, you may want an
   * ImageRegionReverseIterator.  Rather than provide overloaded APIs
   * that return different types of Iterators, itk returns
   * ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageRegionReverseIterator. */
  template <typename TOtherImage, std::enable_if_t<detail::IsImageIteratorConvertible_v<TOtherImage, TImage>, int> = 0>
  ImageRegionReverseIterator(const ImageIterator<TOtherImage> & it)
    : Superclass(it)
  {}

  /** Constructor that takes in a reverse image iterator.  This can be used
   * to cast between the various types of reverse image iterators. */
  template <typename TOtherImage, std::enable_if_t<detail::IsImageIteratorConvertible_v<TOtherImage, TImage>, int> = 0>
  ImageRegionReverseIterator(const ImageReverseIterator<TOtherImage> & it)
    : Superclass(it)
  {}

  /** Constructor that takes in an image region iterator.  This can be used
   * to cast between the various types of reverse image iterators. */
  template <typename TOtherImage, std::enable_if_t<detail::IsImageIteratorConvertible_v<TOtherImage, TImage>, int> = 0>
  ImageRegionReverseIterator(const ImageRegionIterator<TOtherImage> & it)
    : Superclass(it)
  {}
};

// Deduction guide for class template argument deduction (CTAD).
template <typename TImage>
ImageRegionReverseIterator(SmartPointer<TImage>, const typename TImage::RegionType &)
  -> ImageRegionReverseIterator<TImage>;

/** \class ImageRegionReverseConstIterator
 * \brief Read-only ImageRegionReverseIterator: ImageRegionReverseConstIterator<TImage> behaves as
 * ImageRegionReverseIterator<const TImage>.
 *
 * It is a derived class rather than an alias template so that it supports class template argument deduction.
 *
 * \ingroup ImageIterators
 * \ingroup ITKCommon
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ImageRegionReverseConstIterator : public ImageRegionReverseIterator<std::add_const_t<TImage>>
{
public:
  /** Standard class type alias. */
  using Self = ImageRegionReverseConstIterator;
  using Superclass = ImageRegionReverseIterator<std::add_const_t<TImage>>;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(ImageRegionReverseConstIterator);

  using Superclass::Superclass;

  ImageRegionReverseConstIterator() = default;

  /** Conversion from ImageRegionReverseIterator<const TImage>. */
  ImageRegionReverseConstIterator(const Superclass & it)
    : Superclass(it)
  {}
};

// Deduction guides for class template argument deduction (CTAD).
template <typename TImage>
ImageRegionReverseConstIterator(const TImage *, const typename TImage::RegionType &)
  -> ImageRegionReverseConstIterator<TImage>;

template <typename TImage>
ImageRegionReverseConstIterator(SmartPointer<TImage>, const typename TImage::RegionType &)
  -> ImageRegionReverseConstIterator<std::remove_const_t<TImage>>;

} // end namespace itk

#endif
