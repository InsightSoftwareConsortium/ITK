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
#ifndef itkImageRegionExclusionIteratorWithIndex_h
#define itkImageRegionExclusionIteratorWithIndex_h

#include "itkImageRegionExclusionConstIteratorWithIndex.h"
#include "itkImageIteratorWithIndex.h"

namespace itk
{
/** \class ImageRegionExclusionIteratorWithIndex
 *  \brief A multi-dimensional image iterator that walks an image region,
 *         excluding a second region contained within the first, with write
 *         access to pixels.
 *
 * Most of the functionality of this iterator is inherited from the
 * ImageRegionExclusionConstIteratorWithIndex, which should be consulted for
 * details.  This class only adds write access to the iterator.
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
class ITK_TEMPLATE_EXPORT ImageRegionExclusionIteratorWithIndex:public ImageRegionExclusionConstIteratorWithIndex< TImage >
{
public:
  /** Standard class typedefs. */
  typedef ImageRegionExclusionIteratorWithIndex                Self;
  typedef ImageRegionExclusionConstIteratorWithIndex< TImage > Superclass;

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
  ImageRegionExclusionIteratorWithIndex();

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageRegionExclusionIteratorWithIndex(ImageType *ptr, const RegionType & region);

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageRegionExclusionIteratorWithIndex. Many routines return an ImageIterator, but for a
   * particular task, you may want an ImageRegionExclusionIteratorWithIndex.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageRegionExclusionIteratorWithIndex. */
  ImageRegionExclusionIteratorWithIndex(const ImageIteratorWithIndex< TImage > & it);

  /** Set the pixel value */
  void Set(const PixelType & value) const
  { this->m_PixelAccessorFunctor.Set(*( const_cast< InternalPixelType * >( this->m_Position ) ), value); }

  /** Return a reference to the pixel.
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  PixelType & Value(void)
  { return *( const_cast< InternalPixelType * >( this->m_Position ) ); }

protected:
  /** The construction from a const iterator is declared protected
      in order to enforce const correctness. */
  ImageRegionExclusionIteratorWithIndex(const ImageRegionExclusionConstIteratorWithIndex< TImage > & it);
  Self & operator=(const ImageRegionExclusionConstIteratorWithIndex< TImage > & it);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRegionExclusionIteratorWithIndex.hxx"
#endif

#endif
