/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRandomAccessNeighborhoodIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRandomAccessNeighborhoodIterator_h
#define __itkRandomAccessNeighborhoodIterator_h

#include <vector>
#include <string.h>
#include <iostream>
#include "itkConstRandomAccessNeighborhoodIterator.h"

namespace itk {

/** \class RandomAccessNeighborhoodIterator
 * \brief Extends the bi-directional NeighborhoodIterator to a random access
 * iterator.
 *
 * This class adds several methods to NeighborhoodIterator to allow arbitrary,
 * N-dimensional movement of the iterator through an image.  This iterator can
 * be incremented or decremented by an itk::Offset.  Subtracting two
 * RandomAccessNeighborhoodIterator objects returns an itk::Offset representing
 * the distance between them.
 *
 * \ingroup ImageIterators
 *
 * \sa NeighborhoodIterator \sa Offset
 */
template<class TImage>
class ITK_EXPORT RandomAccessNeighborhoodIterator
  :  public ConstRandomAccessNeighborhoodIterator<TImage>
{
public:
  /** Standard class typedefs. */
  typedef RandomAccessNeighborhoodIterator Self;
  typedef ConstRandomAccessNeighborhoodIterator<TImage> Superclass;
  
  /** Extract typedefs from superclass. */
  itkStaticConstMacro(Dimension, unsigned int, Superclass::Dimension);

  /** Extract typedefs from superclass */
  typedef typename Superclass::InternalPixelType InternalPixelType;
  typedef typename Superclass::PixelType  PixelType;
  typedef typename Superclass::SizeType   SizeType;
  typedef typename Superclass::ImageType  ImageType;
  typedef typename Superclass::RegionType RegionType;
  typedef typename Superclass::IndexType  IndexType;
  typedef typename Superclass::OffsetType OffsetType;
  typedef typename Superclass::RadiusType RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::Iterator      Iterator;
  typedef typename Superclass::ConstIterator ConstIterator;
  typedef typename Superclass::ImageBoundaryConditionPointerType
   ImageBoundaryConditionPointerType;
    
  /** Default constructor. */
  RandomAccessNeighborhoodIterator(): Superclass() {}
  
  /** Copy constructor */
  RandomAccessNeighborhoodIterator( const RandomAccessNeighborhoodIterator &n )
    : Superclass(n) { itkGenericOutputMacro( << "The RandomAccessNeighborhood iterator is deprecated and will be removed.  Please use the NeighborhoodIterator instead."); }
  
  /** Assignment operator */
  Self &operator=(const Self& orig)
    {
      Superclass::operator=(orig);
      return *this;
    }
  
  /** Constructor which establishes the region size, neighborhood, and image
   * over which to walk. */
  RandomAccessNeighborhoodIterator(const SizeType &radius,
                       ImageType * ptr,
                       const RegionType &region
                       )
    : Superclass(radius, ptr, region) {  itkGenericOutputMacro( << "The RandomAccessNeighborhood iterator is deprecated and will be removed.  Please use the NeighborhoodIterator instead.");  }

  /** Standard print method */
  virtual void PrintSelf(std::ostream &, Indent) const;

  /** Returns the central memory pointer of the neighborhood. */
  InternalPixelType *GetCenterPointer()
    { return (this->operator[]((this->Size())>>1)); }

  /** Set the center pixel value. */
  virtual void SetCenterPixel(const PixelType &p)
    { *( this->GetCenterPointer() ) = p; }
  
  /** Virtual function that replaces the pixel values in the image
   * neighborhood that are pointed to by this RandomAccessNeighborhoodIterator with
   * the pixel values contained in a Neighborhood. */
  virtual void SetNeighborhood(const NeighborhoodType &);

  /** Set the pixel value. */
  virtual void SetPixel(const unsigned long i, const PixelType &v)
    { *(this->operator[](i)) = v; }
};

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRandomAccessNeighborhoodIterator.txx"
#endif

#endif
