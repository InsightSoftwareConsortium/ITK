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
  {  itkGenericOutputMacro("The ConstRandomAccessNeighborhood is deprecated and will be removed.  Use ConstNeighborhoodIterator instead"); }

  /** Virtual destructor */
  virtual ~ConstRandomAccessNeighborhoodIterator() {}
  
  /** Copy constructor */
  ConstRandomAccessNeighborhoodIterator( const
                                         ConstRandomAccessNeighborhoodIterator
                                         &orig )
    : Superclass(orig)
    {}
  
  /** Constructor which establishes the region size, neighborhood, and image
   * over which to walk. */
  ConstRandomAccessNeighborhoodIterator(const SizeType &radius,
                       const ImageType * ptr,
                       const RegionType &region
                                        )
    : Superclass(radius, ptr, region)
    {}

  /** Assignment operator */
  Self &operator=(const Self& orig)
    {
    Superclass::operator=(orig);
    return *this;
    }

  /** Standard itk print method */
  virtual void PrintSelf(std::ostream &, Indent) const;

  /** Addition of an itk::Offset.  Note that this method does not do any bounds
   * checking.  Adding an offset that moves the iterator out of its assigned
   * region will produce undefined results. */
  //  Self &operator+=(const OffsetType &);

  /** Subtraction of an itk::Offset. Note that this method does not do any bounds
   * checking.  Subtracting an offset that moves the iterator out of its
   * assigned region will produce undefined results. */
  //  Self &operator-=(const OffsetType &);

  /** Distance between two iterators */
  //  OffsetType operator-(const Self& b)
  //  {  return m_Loop - b.m_Loop;  }
  
};
  /*
template<class TImage>
inline ConstRandomAccessNeighborhoodIterator<TImage>
operator+(const ConstRandomAccessNeighborhoodIterator<TImage> &it,
          const typename ConstRandomAccessNeighborhoodIterator<TImage>
          ::OffsetType &ind)
{
  ConstRandomAccessNeighborhoodIterator<TImage> ret;
  ret = it;
  ret += ind;
  return ret;
}

template<class TImage>
inline ConstRandomAccessNeighborhoodIterator<TImage>
operator+(const typename ConstRandomAccessNeighborhoodIterator<TImage>
          ::OffsetType &ind,
          const ConstRandomAccessNeighborhoodIterator<TImage> &it)
{  return (it + ind); }

template<class TImage>
inline ConstRandomAccessNeighborhoodIterator<TImage>
operator-(const ConstRandomAccessNeighborhoodIterator<TImage> &it,
          const typename ConstRandomAccessNeighborhoodIterator<TImage>
          ::OffsetType &ind)
{
  ConstRandomAccessNeighborhoodIterator<TImage> ret;
  ret = it;
  ret -= ind;
  return ret;
}
*/

  
} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConstRandomAccessNeighborhoodIterator.txx"
#endif

#endif
