/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstRandomAccessNeighborhoodIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConstRandomAccessNeighborhoodIterator_h
#define __itkConstRandomAccessNeighborhoodIterator_h

#include "itkMacro.h"
#include "itkConstNeighborhoodIterator.h"

namespace itk {

/** \class ConstRandomAccessNeighborhoodIterator
 *
 * \brief Const version of RandomAccessNeighborhoodIterator, defining iteration
 * of a local N-dimensional neighborhood of pixels across an itk::Image.
 *
 * ConstRandomAccessNeighborhoodIterator implements the read-only methods of
 * RandomAccessNeighborhoodIterator.  It serves as a base class from which
 * other iterators are derived. See RandomAccessNeighborhoodIterator for more
 * complete information.
 *
 * \ingroup ImageIterators
 *
 * \sa Neighborhood \sa ImageIterator \sa NeighborhoodIterator
 * \sa SmartNeighborhoodIterator \sa RandomAccessNeighborhoodIterator
 */
template<class TImage>
class ITK_EXPORT ConstRandomAccessNeighborhoodIterator
  :  public ConstNeighborhoodIterator<TImage>
{
public:
  /** Standard class typedefs. */
  typedef ConstRandomAccessNeighborhoodIterator Self;
  typedef ConstNeighborhoodIterator<TImage> Superclass;
  
  /** Extract typedefs from superclass */
  typedef typename Superclass::InternalPixelType InternalPixelType;
  typedef typename Superclass::PixelType  PixelType;
  typedef typename Superclass::SizeType   SizeType;
  typedef typename Superclass::SizeValueType SizeValueType;
  typedef typename Superclass::ImageType  ImageType;
  typedef typename Superclass::RegionType RegionType;
  typedef typename Superclass::IndexType  IndexType;
  typedef typename Superclass::IndexValueType IndexValueType;
  typedef typename Superclass::OffsetType OffsetType;
  typedef typename Superclass::OffsetValueType OffsetValueType;
  typedef typename Superclass::RadiusType RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  itkStaticConstMacro(Dimension, unsigned int, Superclass::Dimension);
  typedef typename Superclass::Iterator      Iterator;
  typedef typename Superclass::ConstIterator ConstIterator;
  typedef typename Superclass::ImageBoundaryConditionPointerType
   ImageBoundaryConditionPointerType;
    
  /** Default constructor */
  ConstRandomAccessNeighborhoodIterator()
    : Superclass()
  {  itkGenericOutputMacro( << "The ConstRandomAccessNeighborhood is deprecated and will be removed.  Use ConstNeighborhoodIterator instead"); }

  /** Virtual destructor */
  virtual ~ConstRandomAccessNeighborhoodIterator() {}
  
  /** Copy constructor */
  ConstRandomAccessNeighborhoodIterator( const
                                         ConstRandomAccessNeighborhoodIterator
                                         &orig )
    : Superclass(orig)
    { itkGenericOutputMacro( << "The ConstRandomAccessNeighborhood iterator is deprecated and will be removed.  Please use the ConstNeighborhoodIterator instead."); }
  
  /** Constructor which establishes the region size, neighborhood, and image
   * over which to walk. */
  ConstRandomAccessNeighborhoodIterator(const SizeType &radius,
                       const ImageType * ptr,
                       const RegionType &region
                                        )
    : Superclass(radius, ptr, region)
     { itkGenericOutputMacro( << "The ConstRandomAccessNeighborhood iterator is deprecated and will be removed.  Please use the NeighborhoodIterator instead."); }

  /** Assignment operator */
  Self &operator=(const Self& orig)
    {
    Superclass::operator=(orig);
    return *this;
    }

  /** Standard itk print method */
  virtual void PrintSelf(std::ostream &, Indent) const;

};
  
} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConstRandomAccessNeighborhoodIterator.txx"
#endif

#endif
