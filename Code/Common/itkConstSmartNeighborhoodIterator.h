/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstSmartNeighborhoodIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConstSmartNeighborhoodIterator_h
#define __itkConstSmartNeighborhoodIterator_h

#include "itkConstNeighborhoodIterator.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"

namespace itk {

/** \class ConstSmartNeighborhoodIterator
 *
 * \brief Const version of SmartNeighborhoodIterator, defining iteration of a
 * local N-dimensional neighborhood of pixels across an itk::Image.
 *
 * ConstSmartNeighborhoodIterator implements the read-only methods of
 * SmartNeighborhoodIterator.  It serves as a base class from which other
 * iterators are derived. See SmartNeighborhoodIterator for more complete
 * information.
 *
 * \ingroup ImageIterators
 *
 * \sa Neighborhood \sa ImageIterator \sa NeighborhoodIterator
 * \sa SmartNeighborhoodIterator \sa RandomAccessNeighborhoodIterator
 */
template<class TImage, class TBoundaryCondition
                       = ZeroFluxNeumannBoundaryCondition<TImage>  >
class ITK_EXPORT ConstSmartNeighborhoodIterator
  :  public ConstNeighborhoodIterator<TImage, TBoundaryCondition>
{
public:
  /** Standard class typdefs. */
  typedef ConstSmartNeighborhoodIterator Self;
  typedef ConstNeighborhoodIterator<TImage> Superclass;
  
  /** Extract image type information. */
  typedef typename Superclass::InternalPixelType InternalPixelType;
  typedef typename Superclass::PixelType PixelType;
    
  /** Save the dimension of the image. */
  itkStaticConstMacro(Dimension, unsigned int,Superclass::Dimension);

  /** Some common itk object typedefs */
  typedef typename Superclass::ImageType ImageType;
  typedef typename Superclass::RegionType RegionType;
  typedef typename Superclass::SizeType SizeType;
  typedef typename Superclass::SizeValueType SizeValueType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::IndexType IndexType;
  typedef typename Superclass::IndexValueType IndexValueType;
  typedef typename Superclass::OffsetType OffsetType;
  typedef typename Superclass::OffsetValueType OffsetValueType;

  /** Typedef for generic boundary condition pointer */
  typedef typename Superclass::ImageBoundaryConditionPointerType
         ImageBoundaryConditionPointerType;

  /** Typedef for boundary condition type. */
  typedef TBoundaryCondition BoundaryConditionType;
  
  /** Support for internal iterator types.  Only const iteration is supported
   * in this class. */
  typedef typename Superclass::ConstIterator ConstIterator;
  
  /** Default constructor. */
  ConstSmartNeighborhoodIterator()
    : Superclass()
  {
    itkGenericOutputMacro(<< "ConstSmartNeighborhoodIterator has been deprecated and will be removed.  Use ConstNeighborhoodIterator instead.");
    //for (unsigned int i=0; i < Dimension; i++)
    //       { m_InBounds[i] = false; }
    //    this->ResetBoundaryCondition();
    //    m_NeedToUseBoundaryCondition = true;
  }

  /** Copy constructor */
  //  ConstSmartNeighborhoodIterator(const Self& orig);

  /** Constructor establishes a neighborhood of iterators of a specified
   * dimension to walk a particular image and a particular region of
   * that image. */ 
  ConstSmartNeighborhoodIterator(const SizeType& radius,
                                 const ImageType *ptr,
                                 const RegionType& region)
    : Superclass(radius, ptr, region)
  {
    //this->Initialize(radius, ptr, region);
    //      for (unsigned int i=0; i < Dimension; i++)
    //         { m_InBounds[i] = false; }
    //      this->ResetBoundaryCondition();
    itkGenericOutputMacro(<< "ConstSmartNeighborhoodIterator has been deprecated and will be removed.  Use ConstNeighborhoodIterator instead.");
  }

  /** Initializes the iterator to walk a particular image and a particular
   * region of that image. This version is different from the superclass'
   * in that it determines whether the iterator will "ever" have to
   * use the boundary conditions (boundary conditions are skipped if
   * the region padded by radius is within buffered region. */
  //  virtual void Initialize(const SizeType &radius, const ImageType *ptr,
  //                          const RegionType &region);

  /** Assignment operator */
  Self &operator=(const Self& orig);

  /** Prints information about the neighborhood pointer structure to
   * std::cout for debugging purposes. */
  //  virtual void PrintSelf(std::ostream &, Indent) const;
  
  /** Returns the internal, default boundary condition. */
  //  const BoundaryConditionType *GetBoundaryCondition() const
  //    { return dynamic_cast<BoundaryConditionType *>(m_BoundaryCondition); }
  
  /** "Dereferences" the iterator. Returns the Neighborhood of values in the
   * itk::Image masked by the iterator. */
  //  NeighborhoodType GetNeighborhood() const;

  /** Returns the pixel value referenced by a linear array location.  Unlike
   * operator[], this is a safe operation that will automatically detect and
   * handle boundary conditions. */
  //  virtual PixelType GetPixel(const unsigned long i) const;

  /** Returns false if the iterator overlaps region boundaries, true
   * otherwise.  Also updates an internal boolean array indicating
   * which of the iterator's faces are out of bounds. */
  //  bool InBounds() const;
  
  /** Allows a user to override the internal boundary condition. Care should
   * be taken to ensure that the overriding boundary condition is a persistent
   * object during the time it is referenced.  The overriding condition
   * can be of a different type than the default type as long as it is
   * a subclass of ImageBoundaryCondition. */
  //  virtual void OverrideBoundaryCondition(const ImageBoundaryConditionPointerType i)
  //    { m_BoundaryCondition = i; }

  /** Resets the boundary condition to the internal, default conditions
   * specified by the template parameter. */
  //  virtual void ResetBoundaryCondition()
  //    { m_BoundaryCondition = &m_InternalBoundaryCondition;  }

  /** Sets the internal, default boundary condition. */
  //  void SetBoundaryCondition( const TBoundaryCondition &c )
  //    { m_InternalBoundaryCondition = c; }
 
protected:
  
  /** Sets loop boundaries for iteration. */
  //  void SetBound(const SizeType&);

  /** Pointer to the actual boundary condition that will be used.
   * By default this points to m_BoundaryCondition, but
   * OverrideBoundaryCondition allows a user to point this variable an external
   * boundary condition.  */
  //  ImageBoundaryConditionPointerType m_BoundaryCondition;  

  /** Denotes which of the iterators dimensional sides spill outside
   * region of interest boundaries. */
  //  mutable bool m_InBounds[Dimension];
  
  /** Lower threshold of in-bounds loop counter values. */
  //  IndexType m_InnerBoundsLow;
  
  /** Upper threshold of in-bounds loop counter values. */
  //  IndexType m_InnerBoundsHigh;
  
  /** Default boundary condition. */
  //  TBoundaryCondition m_InternalBoundaryCondition;

  /** Does the specified region need to worry about boundary conditions? **/
  //  bool m_NeedToUseBoundaryCondition;
  
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConstSmartNeighborhoodIterator.txx"
#endif

#endif 

