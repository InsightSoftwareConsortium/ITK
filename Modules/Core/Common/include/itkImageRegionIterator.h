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
#ifndef itkImageRegionIterator_h
#define itkImageRegionIterator_h

#include "itkImageRegionConstIterator.h"

namespace itk
{
/** \class ImageRegionIterator
 * \brief A multi-dimensional iterator templated over image type that walks a
 * region of pixels.
 *
 * The itk::ImageRegionIterator is optimized for iteration speed and is the
 * first choice for iterative, pixel-wise operations on an image.
 * ImageRegionIterator is the least specialized of the ITK image iterator
 * classes.  ImageRegionIterator is templated over the image type, and is
 * constrained to walk only within the specified region and along a line
 * parallel to one of the coordinate axes, "wrapping" to the next line as it
 * reaches the boundary of the image.  To walk the entire image, specify the
 * region to be \c image->GetRequestedRegion().
 *
 * Most of the functionality is inherited from the ImageRegionConstIterator.
 * The current class only adds write access to image pixels.
 *
 * \par MORE INFORMATION
 * For a complete description of the ITK Image Iterators and their API, please
 * see the Iterators chapter in the ITK Software Guide.  The ITK Software Guide
 * is available in print and as a free .pdf download from https://www.itk.org.
 *
 * example ImageRegionIterator.cxx
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
 * \sa ImageRegionRange
 * \sa ImageRegionIndexRange
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/IterateRegionWithWriteAccess,Iterate Region In Image With Write Access}
 * \endsphinx
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ImageRegionIterator : public ImageRegionConstIterator<TImage>
{
public:
  /** Standard class type aliases. */
  using Self = ImageRegionIterator;
  using Superclass = ImageRegionConstIterator<TImage>;

  /** Types inherited from the Superclass */
  using IndexType = typename Superclass::IndexType;
  using SizeType = typename Superclass::SizeType;
  using OffsetType = typename Superclass::OffsetType;
  using RegionType = typename Superclass::RegionType;
  using ImageType = typename Superclass::ImageType;
  using PixelContainer = typename Superclass::PixelContainer;
  using PixelContainerPointer = typename Superclass::PixelContainerPointer;
  using InternalPixelType = typename Superclass::InternalPixelType;
  using PixelType = typename Superclass::PixelType;
  using AccessorType = typename Superclass::AccessorType;

  /** Default constructor. Needed since we provide a cast constructor. */
  ImageRegionIterator() = default;

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageRegionIterator(ImageType * ptr, const RegionType & region);

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageRegionIterator. Many routines return an ImageIterator but for a
   * particular task, you may want an ImageRegionIterator.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageRegionIterator. */
  ImageRegionIterator(const ImageIterator<TImage> & it);

  /** Set the pixel value */
  void
  Set(const PixelType & value) const
  {
    this->m_PixelAccessorFunctor.Set(*(const_cast<InternalPixelType *>(this->m_Buffer + this->m_Offset)), value);
  }

  /** Return a reference to the pixel
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  PixelType &
  Value()
  {
    return *(const_cast<InternalPixelType *>(this->m_Buffer + this->m_Offset));
  }

protected:
  /** the construction from a const iterator is declared protected
      in order to enforce const correctness. */
  ImageRegionIterator(const ImageRegionConstIterator<TImage> & it);
  Self &
  operator=(const ImageRegionConstIterator<TImage> & it);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageRegionIterator.hxx"
#endif

#endif
