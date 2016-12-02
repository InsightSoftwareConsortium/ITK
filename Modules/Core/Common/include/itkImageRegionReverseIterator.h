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
#ifndef itkImageRegionReverseIterator_h
#define itkImageRegionReverseIterator_h

#include "itkImageRegionReverseConstIterator.h"
#include "itkImageConstIterator.h"

namespace itk
{
/** \class ImageRegionReverseIterator
 * \brief A multi-dimensional image iterator which only walks a region.
 *
 * ImageRegionReverseIterator is a templated class to represent a multi-dimensional
 * iterator. ImageRegionReverseIterator is templated over the image type
 * ImageRegionReverseIterator is constrained to walk only within the
 * specified region and along a line parallel to one of the coordinate axis.
 *
 * Most of the functionality is inherited from the ImageRegionReverseConstIterator.
 * The current class only adds write access to image pixels.
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
template< typename TImage >
class ITK_TEMPLATE_EXPORT ImageRegionReverseIterator:public ImageRegionReverseConstIterator< TImage >
{
public:
  /** Standard class typedefs. */
  typedef ImageRegionReverseIterator                Self;
  typedef ImageRegionReverseConstIterator< TImage > Superclass;

  /** Types inherited from the Superclass */
  typedef typename Superclass::IndexType             IndexType;
  typedef typename Superclass::SizeType              SizeType;
  typedef typename Superclass::OffsetType            OffsetType;
  typedef typename Superclass::RegionType            RegionType;
  typedef typename Superclass::ImageType             ImageType;
  typedef typename Superclass::PixelContainer        PixelContainer;
  typedef typename Superclass::PixelContainerPointer PixelContainerPointer;
  typedef typename Superclass::InternalPixelType     InternalPixelType;
  typedef typename Superclass::PixelType             PixelType;
  typedef typename Superclass::AccessorType          AccessorType;

  /** Default constructor. Needed since we provide a cast constructor. */
  ImageRegionReverseIterator();

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageRegionReverseIterator(ImageType *ptr, const RegionType & region);

  /** Constructor that can be used to cast from an ImageConstIterator to an
   * ImageRegionReverseIterator. Many routines return an ImageConstIterator but for a
   * particular task, you may want an ImageRegionReverseIterator.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageConstIterators and uses constructors to cast from an
   * ImageConstIterator to a ImageRegionReverseIterator. */
  ImageRegionReverseIterator(const ImageConstIterator< TImage > & it);

  /** Set the pixel value */
  void Set(const PixelType & value) const
  { this->m_PixelAccessor.Set(*const_cast< InternalPixelType  * >( ( this->m_Buffer + this->m_Offset ) ), value); }

  /** Return a reference to the pixel
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  PixelType & Value(void)
  { return *const_cast< InternalPixelType  * >( ( this->m_Buffer + this->m_Offset ) ); }

  /** Return an iterator for the beginning of the region. "Begin"
   * is defined as the first pixel in the region.
   * \deprecated Use GoToBegin() instead */
  itkLegacyMacro(Self Begin(void) const);

  /** Return an iterator for the end of the region. "End" is defined
   * as one pixel past the last pixel of the region.
   * \deprecated Use GoToEnd() instead */
  itkLegacyMacro(Self End(void) const);

protected:
  /** the construction from a const iterator is declared protected
      in order to enforce const correctness. */
  ImageRegionReverseIterator(const ImageRegionReverseConstIterator< TImage > & it);
  Self & operator=(const ImageRegionReverseConstIterator< TImage > & it);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRegionReverseIterator.hxx"
#endif

#endif
