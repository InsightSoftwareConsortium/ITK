/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRandomAccessNeighborhoodIterator.h
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
#ifndef __itkRandomAccessNeighborhoodIterator_h
#define __itkRandomAccessNeighborhoodIterator_h

#include <vector>
#include <string.h>
#include <iostream>
#include "itkConstRandomAccessNeighborhoodIterator.h"

namespace itk {

/**
 * \class RandomAccessNeighborhoodIterator
 * \brief
 *
 */
template<class TImage>
class ITK_EXPORT RandomAccessNeighborhoodIterator
  :  public ConstRandomAccessNeighborhoodIterator<TImage>
{
public:
  /** 
   * Standard "Self" & Superclass typedef support.
   */
  typedef RandomAccessNeighborhoodIterator Self;
  typedef ConstRandomAccessNeighborhoodIterator<TImage> Superclass;

  /**
   * Extract typedefs from superclass
   */
  typedef typename Superclass::InternalPixelType InternalPixelType;
  typedef typename Superclass::PixelType  PixelType;
  typedef typename Superclass::SizeType   SizeType;
  typedef typename Superclass::ImageType  ImageType;
  typedef typename Superclass::RegionType RegionType;
  typedef typename Superclass::IndexType  IndexType;
  typedef typename Superclass::OffsetType OffsetType;
  typedef typename Superclass::RadiusType RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  enum {Dimension = Superclass::Dimension };
  typedef typename Superclass::Iterator      Iterator;
  typedef typename Superclass::ConstIterator ConstIterator;
  typedef typename Superclass::ImageBoundaryConditionPointerType
   ImageBoundaryConditionPointerType;
  typedef typename Superclass::ScalarValueType ScalarValueType;
  
  /**
   * Default constructor.
   */
  RandomAccessNeighborhoodIterator(): Superclass() {}
  
  /**
   * Copy constructor
   */
  RandomAccessNeighborhoodIterator( const RandomAccessNeighborhoodIterator &n )
    : Superclass(n) {}
  
  /**
   * Assignment operator
   */
  Self &operator=(const Self& orig)
    {
      Superclass::operator=(orig);
      return *this;
    }
  
  /**
   * Constructor which establishes the region size, neighborhood, and image
   * over which to walk.
   */
  RandomAccessNeighborhoodIterator(const SizeType &radius,
                       ImageType * ptr,
                       const RegionType &region
                       )
    : Superclass(radius, ptr, region)
    { }

  /**
   * Standard print method
   */
  virtual void PrintSelf(std::ostream &, Indent) const;

  /**
   * Returns the central memory pointer of the neighborhood.
   */
  InternalPixelType *GetCenterPointer()
    {    return (this->operator[]((this->Size())>>1));  }


  virtual void SetCenterPixel(const PixelType &p)
    {    *( this->GetCenterPointer() ) = p;  }
  
  /**
   * Virtual function that replaces the pixel values in the image
   * neighborhood that are pointed to by this RandomAccessNeighborhoodIterator with
   * the pixel values contained in a Neighborhood.
   */
  virtual void SetNeighborhood(const NeighborhoodType &);

  /**
   *
   */
  virtual void SetPixel(const unsigned long i, const PixelType &v)
    { *(this->operator[](i)) = v; }
    
};

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRandomAccessNeighborhoodIterator.txx"
#endif

#endif
