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
#ifndef itkImageRandomNonRepeatingIteratorWithIndex_h
#define itkImageRandomNonRepeatingIteratorWithIndex_h

#include "itkImageRandomNonRepeatingConstIteratorWithIndex.h"
#include "itkImageIteratorWithIndex.h"

namespace itk
{
/** \class ImageRandomNonRepeatingIteratorWithIndex
 * \brief A multi-dimensional image iterator that visits image pixels within a
 * region in a random order, without repeating.
 *
 *  This class was contributed by Rupert Brooks, McGill Centre for Intelligent
 *  Machines, Montreal, Canada.  It is heavily based on the
 *  ImageRandomIterator class.
 *
 *  This iterator is a subclass of
 *  itk::ImageRandomNonRepeatingConstIteratorWithIndex that
 *  adds write-access functionality.  Please see
 *  itk::ImageRandomNonRepeatingConstIteratorWithIndex for more information.
 *
 * \par MORE INFORMATION
 * For a complete description of the ITK Image Iterators and their API, please
 * see the Iterators chapter in the ITK Software Guide.  The ITK Software Guide
 * is available in print and as a free .pdf download from https://www.itk.org.
 *
 * \author Rupert Brooks, McGill Centre for Intelligent Machines. Canada
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
 * \sa ImageRandomNonRepeatingConstIteratorWithIndex  \sa ImageRandomNonRepeatingIteratorWithIndex
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
 *
 * \ingroup ITKCommon
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT ImageRandomNonRepeatingIteratorWithIndex:public ImageRandomNonRepeatingConstIteratorWithIndex< TImage >
{
public:
  /** Standard class typedefs. */
  typedef ImageRandomNonRepeatingIteratorWithIndex                Self;
  typedef ImageRandomNonRepeatingConstIteratorWithIndex< TImage > Superclass;

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
  ImageRandomNonRepeatingIteratorWithIndex();

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageRandomNonRepeatingIteratorWithIndex(ImageType *ptr, const RegionType & region);

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageRandomNonRepeatingIteratorWithIndex. Many routines return an ImageIterator, but for a
   * particular task, you may want an ImageRandomNonRepeatingIteratorWithIndex.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageRandomNonRepeatingIteratorWithIndex. */
  ImageRandomNonRepeatingIteratorWithIndex(const ImageIteratorWithIndex< TImage > & it);

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
  ImageRandomNonRepeatingIteratorWithIndex(const ImageRandomNonRepeatingConstIteratorWithIndex< TImage > & it);
  Self & operator=(const ImageRandomNonRepeatingConstIteratorWithIndex< TImage > & it);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRandomNonRepeatingIteratorWithIndex.hxx"
#endif

#endif
