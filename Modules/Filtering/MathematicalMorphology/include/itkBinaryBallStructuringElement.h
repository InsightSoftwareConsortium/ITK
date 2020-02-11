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
#ifndef itkBinaryBallStructuringElement_h
#define itkBinaryBallStructuringElement_h

#include "itkNeighborhood.h"

namespace itk
{
/** \class BinaryBallStructuringElement
 * \brief A Neighborhood that represents a ball structuring element
 *       (ellipsoid) with binary elements.
 *
 * This class defines a Neighborhood whose elements are either off or on
 * depending on whether they are outside or inside an ellipsoid whose
 * radii match the radii of the Neighborhood.  This class can be used
 * as a structuring element for the Morphology image filters.
 *
 * A BinaryBallStructuringElement has an N-dimensional \em radius.
 * The radius is defined separately for each dimension as the number
 * of pixels that the neighborhood extends outward from the center
 * pixel.  For example, a 2D BinaryBallStructuringElement object with
 * a radius of 2x3 has sides of length 5x7.
 *
 * BinaryBallStructuringElement objects always have an unambiguous
 * center because their side lengths are always odd.
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
 *
 * \sphinx
 * \sphinxexample{Filtering/MathematicalMorphology/CreateABinaryBallStructuringElement,Create A Binary Ball Structuring
 * Element} \endsphinx
 */

template <typename TPixel, unsigned int VDimension = 2, typename TAllocator = NeighborhoodAllocator<TPixel>>
class ITK_TEMPLATE_EXPORT BinaryBallStructuringElement : public Neighborhood<TPixel, VDimension, TAllocator>
{
public:
  /** Standard class type aliases. */
  using Self = BinaryBallStructuringElement;
  using Superclass = Neighborhood<TPixel, VDimension, TAllocator>;

  /** External support for allocator type. */
  using AllocatorType = TAllocator;

  /** External support for dimensionality. */
  static constexpr unsigned int NeighborhoodDimension = VDimension;

  /** External support for pixel type. */
  using PixelType = TPixel;

  /** Iterator type alias support Note the naming is intentional, i.e.,
   * AllocatorType::iterator and AllocatorType::const_iterator, because the
   * allocator may be a vnl object or other type, which uses this form. */
  using Iterator = typename AllocatorType::iterator;
  using ConstIterator = typename AllocatorType::const_iterator;

  /** Size and value type alias support */
  using SizeType = typename Superclass::SizeType;
  using SizeValueType = typename Superclass::SizeValueType;

  /** Radius type alias support */
  using RadiusType = typename Superclass::RadiusType;

  /** External slice iterator type type alias support */
  using SliceIteratorType = SliceIterator<TPixel, Self>;

  /** Default constructor. */
  BinaryBallStructuringElement() = default;

  /** Default destructor. */
  ~BinaryBallStructuringElement() override = default;

  /** Copy constructor. */
  BinaryBallStructuringElement(const Self & other)
    : Neighborhood<TPixel, VDimension, TAllocator>(other)
  {}

  /** Assignment operator. */
  Self &
  operator=(const Self & other)
  {
    Superclass::operator=(other);
    return *this;
  }

  /** Build the structuring element */
  void
  CreateStructuringElement();

protected:
private:
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinaryBallStructuringElement.hxx"
#endif

#endif
