/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionBoundaryNeighborhoodIterator.h
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
#ifndef __itkRegionBoundaryNeighborhoodIterator_h
#define __itkRegionBoundaryNeighborhoodIterator_h

#include "itkSmartRegionNeighborhoodIterator.h"

namespace itk {

/**
 * \class RegionBoundaryNeighborhoodIterator
 * \brief A "smart" neighborhood region iterator that is constrained
 * to pixels on a region of interest boundary.
 *
 * RegionBoundaryNeighborhoodIterator traverses the pixels on region
 * boundaries. Region boundaries are defined as pixels whose neighborhood
 * overlaps the edge of the region.  RegionBoundaryNeighborhoodIterator
 * is a subclass of SmartRegionNeighborhoodIterator and inherits
 * functionality to resolve boundary conditions.
 *
 * RegionBoundaryNeighborhoodIterator will visit all the pixels in
 * a region where the neighborhood described by the iterator overlaps
 * the edge of the region.  The order in which it visits the pixels
 * is not guaranteed.  See NeighborhoodIterator documentation for
 * ways of guaranteeing spatial fidelity between input and output.
 *
 */
template<class TImage,
  class TBoundaryCondition = ZeroFluxNeumannBoundaryCondition<TImage>
  >
class ITK_EXPORT RegionBoundaryNeighborhoodIterator
  :  public SmartRegionNeighborhoodIterator<TImage, TBoundaryCondition>
{
public:
  /** 
   * Standard "Self" & Superclass typedef support.
   */
  typedef RegionBoundaryNeighborhoodIterator Self;
  typedef SmartRegionNeighborhoodIterator<TImage,
    TBoundaryCondition> Superclass;

  /**
   * Extract image type information.
   */
  typedef typename Superclass::InternalPixelType InternalPixelType;
  typedef typename Superclass::PixelType PixelType;
  enum {Dimension = Superclass::Dimension };
  
 /**
   * Some common itk object typedefs
   */
  typedef typename Superclass::ImageType ImageType;
  typedef typename Superclass::RegionType RegionType;
  typedef typename Superclass::SizeType SizeType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;

  /**
   * Scalar data type typedef support
   */
  typedef typename Superclass::ScalarValueType ScalarValueType;
  
  /**
   * Default constructor.
   */
  RegionBoundaryNeighborhoodIterator() {};

  /**
   * Copy constructor
   */
  RegionBoundaryNeighborhoodIterator(const Self& other)
    : SmartRegionNeighborhoodIterator<TImage,TBoundaryCondition>(other)
  {    m_InnerStride = other.m_InnerStride;  }

  /**
   * Assignment operator
   */
  Self &operator=(const Self& orig)
  {
    Superclass::operator=(orig);
    m_InnerStride = orig.m_InnerStride;
    return *this;
  }
  
  /**
   * Constructor establishes a neighborhood of iterators of a specified
   * dimension to walk a particular image and a particular region of
   * that image.
   */ 
  RegionBoundaryNeighborhoodIterator(const SizeType& radius,
                                     ImageType * ptr,
                                     const RegionType& region)
  {    this->Initialize(radius, ptr, region);  }

  /**
   * Overridden from itkNeighborhoodPointerBase because this
   * iterator follows a different path across a region.
   */ 
  const NeighborhoodIterator<TImage>
  &operator++();  

  /**
   * Overridden from itkNeighborhoodPointerBase because this
   * iterator follows a different path across a region.
   */ 
  const NeighborhoodIterator<TImage>
  &operator--();  

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
   * Print some debugging information.
   */
  virtual void PrintSelf(std::ostream &os, Indent indent) const
  {
    os << indent << "RegionBoundaryNeighborhoodIterator" << std::endl;
    Superclass::PrintSelf(os, indent.GetNextIndent());
  }

protected:
  /**
   * Sets the loop boundaries for iteration.
   */
  void SetBound(const SizeType&);

  /**
   * The iterator strides needed to move between inner boundary pixels
   * at opposite ends of a dimensional side.
   */
  unsigned long  m_InnerStride;
};

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegionBoundaryNeighborhoodIterator.txx"
#endif

#endif 

