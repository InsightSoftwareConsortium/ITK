/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryBallStructuringElement.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryBallStructuringElement_h
#define __itkBinaryBallStructuringElement_h

#include "itkNeighborhood.h"

namespace itk {
  
/** \class BinaryBallStructuringElement
 * \brief A Neighborhood that represents a ball structuring element 
 *       (ellipsoid) with binary elements.
 *
 * This class defines a Neighborhood whose elements are either 0 or 1
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
 * \sa Neighborhood
 * \sa MorphologyImageFilter
 * \sa BinaryDilateImageFilter
 * \sa BinaryErodeImageFilter
 * 
 * \ingroup Operators
 * \ingroup ImageIterators
 */

template<class TPixel, unsigned int VDimension = 2,
         class TAllocator = NeighborhoodAllocator<TPixel> >
class ITK_EXPORT BinaryBallStructuringElement
  : public Neighborhood<TPixel, VDimension, TAllocator>
{
public:
  /** Standard class typedefs. */
  typedef BinaryBallStructuringElement                 Self;
  typedef Neighborhood<TPixel, VDimension, TAllocator> Superclass;

  /** External support for allocator type. */
  typedef TAllocator AllocatorType;

  /** External support for dimensionality. */
  itkStaticConstMacro(NeighborhoodDimension, unsigned int, VDimension);
  
  /** External support for pixel type. */
  typedef TPixel PixelType;
  
  /** Iterator typedef support. Note the naming is intentional, i.e.,
  * ::iterator and ::const_iterator, because the allocator may be a
  * vnl object or other type, which uses this form. */
  typedef typename AllocatorType::iterator       Iterator;
  typedef typename AllocatorType::const_iterator ConstIterator;
  
  /** Size and value typedef support. */
  typedef typename Superclass::SizeType      SizeType;
  typedef typename Superclass::SizeValueType SizeValueType;
  
  /** Radius typedef support. */
  typedef typename Superclass::RadiusType RadiusType;

  /** External slice iterator type typedef support. */
  typedef SliceIterator<TPixel, Self> SliceIteratorType;
  
  /** Default constructor. */
  BinaryBallStructuringElement() {}

  /** Default destructor. */
  virtual ~BinaryBallStructuringElement() {}
    
  /** Copy constructor. */
  BinaryBallStructuringElement(const Self& other)
    : Neighborhood<TPixel, VDimension, TAllocator>(other)
    {
    }

  /** Assignment operator. */
  Self &operator=(const Self& other)
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
#include "itkBinaryBallStructuringElement.txx"
#endif

#endif
