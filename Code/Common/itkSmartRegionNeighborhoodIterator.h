/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSmartRegionNeighborhoodIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkSmartRegionNeighborhoodIterator_h
#define __itkSmartRegionNeighborhoodIterator_h

#include "itkNeighborhoodIterator.h"
#include "itkConstantBoundaryCondition.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"

namespace itk {

/**
 * \class SmartRegionNeighborhoodIterator
 * \brief Subclass of NeighborhoodIterator designed for iteration
 * over an itk::Image region of interest that performs bounds
 * checking and resolves boundary conditions.
 *
 * SmartRegionNeighborhoodIterator is a subclass of NeighborhoodIterator
 * that has the ability to detect when it is overlapping region of interest
 * boundaries.  This iterator will not attempt to dereference memory outside
 * the region of interest.  Pixel values outside the region of interest
 * are defined by the boundary condition.
 *
 * Bounds checking is performed and boundary conditions are resolved on
 * dereferencing.
 *
 * \sa RegionBoundaryNeighborhoodIterator
 * \sa RegionNeighborhoodIterator
 * \sa NeighborhoodIterator
 * \sa Neighborhood
 */
template<class TImage,
  class TAllocator =
     NeighborhoodAllocator<ITK_TYPENAME TImage::InternalPixelType*>,
  class TBoundaryCondition = ZeroFluxNeumannBoundaryCondition
   <TImage, Neighborhood<ITK_TYPENAME TImage::InternalPixelType*,
                         TImage::ImageDimension,
                         TAllocator>  >,
  class TDerefAllocator =
    NeighborhoodAllocator<ITK_TYPENAME TImage::PixelType>
  >
class ITK_EXPORT SmartRegionNeighborhoodIterator
  :  public NeighborhoodIterator<TImage, TAllocator, TDerefAllocator>
{
public:
  /** 
   * Standard "Self" & Superclass typdef.
   */
  typedef SmartRegionNeighborhoodIterator Self;
  typedef NeighborhoodIterator<TImage, TAllocator, TDerefAllocator>
  Superclass;

  /**
   * Extract image type information.
   */
  typedef typename Superclass::InternalPixelType InternalPixelType;
  typedef typename Superclass::PixelType PixelType;
  enum { Dimension = Superclass::Dimension };

  /**
   * Some common itk object typedefs
   */
  typedef typename Superclass::ImageType ImageType;
  typedef typename Superclass::RegionType RegionType;
  typedef typename Superclass::SizeType SizeType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::IndexType IndexType;
  typedef typename Superclass::OffsetType OffsetType;

  /**
   * Typedef for generic boundary condition pointer
   */
  typedef ImageBoundaryCondition<ImageType,
    Neighborhood<typename ImageType::InternalPixelType *,
    ImageType::ImageDimension, TAllocator> > *
  ImageBoundaryConditionPointerType;
  
  /**
   * Scalar data type typedef support
   */
  typedef typename Superclass::ScalarValueType ScalarValueType;
  
  /**
   * Default constructor.
   */
  SmartRegionNeighborhoodIterator()
  { this->ResetBoundaryCondition(); }

  /**
   * Copy constructor
   */
  SmartRegionNeighborhoodIterator(const Self& orig);
  
  /**
   * Assignment operator
   */
  Self &operator=(const Self& orig);
 
  /**
   * Constructor establishes a neighborhood of iterators of a specified
   * dimension to walk a particular image and a particular region of
   * that image.
   */ 
  SmartRegionNeighborhoodIterator(const SizeType& radius,
                                  ImageType *ptr,
                                  const RegionType& region)
  {  this->ResetBoundaryCondition();
     this->Initialize(radius, ptr, region);  }

  /**
   * Return an iterator for the beginning of the region.
   */
  Self Begin() const;

  /**
   * Return an iterator for the end of the region.
   */
  Self End() const;

 /**
   * 
   */
  virtual void SetEnd()
  {    m_EndPointer = this->End().operator[](this->Size()>>1);  }
  /**
   *
   */
  virtual void SetToBegin()
  {    *this = this->Begin();  }

  /**
   * "Dereferences" the iterator. Returns the Neighborhood of values in the
   * itk::Image masked by the iterator.
   */
  NeighborhoodType GetNeighborhood();

  /**
   * Returns the pixel value referenced by a linear array location.
   */
  virtual PixelType GetPixel(unsigned long i)
  {
    if (this->InBounds())
      {
        return Superclass::GetPixel(i);
      }
    else
      {
        return (this->GetNeighborhood())[i];
      }
  }
  
  /**
   * Sets the values in the itk::Image at the iterator location to the values
   * contained in a Neighborhood.
   */
  void SetNeighborhood(NeighborhoodType &);

  /**
   * Prints information about the neighborhood pointer structure to
   * std::cout for debugging purposes.
   */
  virtual void PrintSelf(std::ostream &, Indent) const;

  /**
   * Returns false if the iterator overlaps region boundaries, true
   * otherwise.  Also updates an internal boolean array indicating
   * which of the iterator's faces are out of bounds.
   */
  bool InBounds();
  
  /**
   * Allows a user to override the internal boundary condition. Care should
   * be taken to ensure that the overriding boundary condition is a persistent
   * object during the time it is referenced.  The overriding condition
   * can be of a different type than the default type as long as it is
   * a subclass of ImageBoundaryCondition.
   */
  virtual void OverrideBoundaryCondition(const ImageBoundaryConditionPointerType i)
  { m_BoundaryCondition = i; }

  /**
   * Resets the boundary condition to the internal, default conditions
   * specified by the template parameter.
   */
  virtual void ResetBoundaryCondition()
  { m_BoundaryCondition = &m_InternalBoundaryCondition;  }

  /**
   * Returns the internal, default boundary condition.
   */
  const TBoundaryCondition &GetBoundaryCondition() const
  { return m_InternalBoundaryCondition;}

  /**
   * Sets the internal, default boundary condition.
   */
  void SetBoundaryCondition( const TBoundaryCondition &c )
  { m_InternalBoundaryCondition = c; }
    
  
protected:
  /**
   * Sets loop boundaries for iteration.
   */
  void SetBound(const SizeType&);

  /**
   * Lower threshold of in-bounds loop counter values.
   */
  IndexType m_InnerBoundsLow;
  
  /**
   * Upper threshold of in-bounds loop counter values.
   */
  IndexType m_InnerBoundsHigh;
  
  /**
   * Denotes which of the iterators dimensional sides spill outside
   * region of interest boundaries.
   */
  bool m_InBounds[Dimension];

  /**
   * Default boundary condition.
   */
  TBoundaryCondition m_InternalBoundaryCondition;

  /**
   * Pointer to the actual boundary condition that will be used.
   * By default this points to m_BoundaryCondition, but
   * OverrideBoundaryCondition allows a user to point this variable an external
   * boundary condition. 
   */
  ImageBoundaryConditionPointerType m_BoundaryCondition;
  
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSmartRegionNeighborhoodIterator.txx"
#endif

#endif 

