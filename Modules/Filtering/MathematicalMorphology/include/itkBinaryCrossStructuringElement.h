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
#ifndef itkBinaryCrossStructuringElement_h
#define itkBinaryCrossStructuringElement_h

#include "itkNeighborhood.h"

namespace itk
{
/** \class BinaryCrossStructuringElement
 * \brief A Neighborhood that represents a cross structuring element
 *        with binary elements.
 *
 * This class defines a Neighborhood whose elements are either on or off
 * depending on whether they are the face connected neighbors of the
 * neighborhood center when the radii are all 1.
 * The neighorhood is a cross for any size radius.
 * By default, the Neighborhood is defined to be of radii 1
 * (i.e. 3x3x...).
 * This can be changed explicitly using the SetRadius() method.
 *
 * Internally, this class carries out all of its computations using the
 * FlatStructuringElement.  It is preferable to use that class instead
 * of this one because FlatStructuringElement is more flexible.
 *
 * \sa Neighborhood
 * \sa MorphologyImageFilter
 * \sa BinaryDilateImageFilter
 * \sa BinaryErodeImageFilter
 *
 * \ingroup Operators
 * \ingroup ImageIterators
 * \ingroup ITKMathematicalMorphology
 */

template< typename TPixel, unsigned int VDimension = 2,
          typename TAllocator = NeighborhoodAllocator< TPixel > >
class ITK_TEMPLATE_EXPORT BinaryCrossStructuringElement:
  public Neighborhood< TPixel, VDimension, TAllocator >
{
public:
  /** Standard class typedefs. */
  typedef BinaryCrossStructuringElement                  Self;
  typedef Neighborhood< TPixel, VDimension, TAllocator > Superclass;

  /** External support for allocator type. */
  typedef TAllocator AllocatorType;

  /** External support for dimensionality. */
  itkStaticConstMacro(NeighborhoodDimension, unsigned int, VDimension);

  /** External support for pixel type. */
  typedef TPixel PixelType;

  /** Iterator typedef support. Note the naming is intentional, i.e.,
  * AllocatorType::iterator and AllocatorType::const_iterator, because the
  * allocator may be a vnl object or other type, which uses this form. */
  typedef typename AllocatorType::iterator       Iterator;
  typedef typename AllocatorType::const_iterator ConstIterator;

  /** Size and value typedef support. */
  typedef typename Superclass::SizeType      SizeType;
  typedef typename Superclass::SizeValueType SizeValueType;

  /** Offset and value typedef support. */
  typedef typename Superclass::OffsetType      OffsetType;
  typedef typename OffsetType::OffsetValueType OffsetValueType;

  /** Radius typedef support. */
  typedef typename Superclass::RadiusType RadiusType;

  /** External slice iterator type typedef support. */
  typedef SliceIterator< TPixel, Self > SliceIteratorType;

  /** Default constructor. */
  BinaryCrossStructuringElement()
  {
    // Default structuring element is defined to be 3x3x3...
    RadiusType radius;
    radius.Fill(1);
    this->SetRadius(radius);
  }

  /** Default destructor. */
  virtual ~BinaryCrossStructuringElement() {}

  /** Copy constructor. */
  BinaryCrossStructuringElement(const Self & other):
    Neighborhood< TPixel, VDimension, TAllocator >(other)
  {}

  /** Assignment operator. */
  Self & operator=(const Self & other)
  {
    Superclass::operator=(other);
    return *this;
  }

  /** Build the structuring element */
  void CreateStructuringElement();

protected:

private:
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryCrossStructuringElement.hxx"
#endif

#endif
