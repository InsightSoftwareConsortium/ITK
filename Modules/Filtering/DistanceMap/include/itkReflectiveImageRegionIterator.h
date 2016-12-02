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
#ifndef itkReflectiveImageRegionIterator_h
#define itkReflectiveImageRegionIterator_h

#include "itkReflectiveImageRegionConstIterator.h"
#include "itkImageIteratorWithIndex.h"

namespace itk
{
/** \class ReflectiveImageRegionIterator
 * \brief Multi-dimensional image iterator which only walks a region.
 *
 * ReflectiveImageRegionIterator is a templated class to represent a multi-dimensional
 * iterator. ReflectiveImageRegionIterator is templated over the image type
 * ReflectiveImageRegionIterator is constrained to walk only within the
 * specified region and along a line parallel to one of the coordinate axis.
 *
 * Most of the functionality is inherited from the ReflectiveImageRegionConstIterator.
 * The current class only adds write access to image pixels.
 *
 *
 * \sa ReflectiveImageRegionConstIterator
 *
 * \ingroup ImageIterators
 * \ingroup ITKDistanceMap
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT ReflectiveImageRegionIterator:public ReflectiveImageRegionConstIterator< TImage >
{
public:
  /** Standard class typedefs. */
  typedef ReflectiveImageRegionIterator                Self;
  typedef ReflectiveImageRegionConstIterator< TImage > Superclass;

  /** Types inherited from the Superclass */
  typedef typename Superclass::IndexType             IndexType;
  typedef typename Superclass::IndexValueType        IndexValueType;
  typedef typename Superclass::SizeType              SizeType;
  typedef typename Superclass::SizeValueType         SizeValueType;
  typedef typename Superclass::OffsetType            OffsetType;
  typedef typename Superclass::OffsetValueType       OffsetValueType;
  typedef typename Superclass::RegionType            RegionType;
  typedef typename Superclass::ImageType             ImageType;
  typedef typename Superclass::PixelContainer        PixelContainer;
  typedef typename Superclass::PixelContainerPointer PixelContainerPointer;
  typedef typename Superclass::InternalPixelType     InternalPixelType;
  typedef typename Superclass::PixelType             PixelType;
  typedef typename Superclass::AccessorType          AccessorType;

  /** Default constructor. Needed since we provide a cast constructor. */
  ReflectiveImageRegionIterator();

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ReflectiveImageRegionIterator(ImageType *ptr, const RegionType & region);

  /** Constructor that can be used to cast from an ImageIterator to an
   * ReflectiveImageRegionIterator. Many routines return an ImageIterator but for a
   * particular task, you may want an ReflectiveImageRegionIterator.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ReflectiveImageRegionIterator. */
  ReflectiveImageRegionIterator(const ImageIteratorWithIndex< TImage > & it);

  /** Set the pixel value */
  void Set(const PixelType & value) const
  { this->m_PixelAccessor.Set(*( const_cast< InternalPixelType * >( this->m_Position ) ), value); }

  /** Return a reference to the pixel
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  PixelType & Value(void)
  { return *( const_cast< InternalPixelType * >( this->m_Position ) ); }

protected:
  /** the construction from a const iterator is declared protected
      in order to enforce const correctness. */
  ReflectiveImageRegionIterator(const ReflectiveImageRegionConstIterator< TImage > & it);
  Self & operator=(const ReflectiveImageRegionConstIterator< TImage > & it);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkReflectiveImageRegionIterator.hxx"
#endif

#endif
