/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodIterator.h
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
#ifndef __itkNeighborhoodIterator_h
#define __itkNeighborhoodIterator_h

#include <vector>
#include <string.h>
#include <iostream>
#include "itkConstNeighborhoodIterator.h"

namespace itk {

/**
 * \class NeighborhoodIterator
 * \brief  Defines iteration of a local N-dimensional neighborhood of pixels
 * across an itk::Image.
 *
 * This class is a loose extension of the Standard Template Library (STL)
 * bi-directional iterator concept to \em masks of pixel neighborhoods within
 * itk::Image objects.  This NeighborhoodIterator base class defines simple
 * forward and reverse iteration of an N-dimensional neighborhood mask
 * across an image.  Elements within the mask can be accessed like elements
 * within an array.
 *
 * NeighborhoodIterators are designed to encapsulate some of the complexity of
 * working with image neighborhoods, complexity that would otherwise have to be
 * managed at the algorithmic level.  Use NeighborhoodIterators to simplify
 * writing algorithms that perform geometrically localized operations on images
 * (for example, convolution and morphological operations).
 *
 * To motivate the discussion of NeighborhoodIterators and their use in
 * Itk, consider the following code that takes directional derivatives at each
 * point in an image.  \sa DerivativeOperator \sa NeighborhoodInnerProduct
 *
 * \code
 * itk::NeighborhoodInnerProduct<ImageType> IP;
 *
 * itk::DerivativeOperator<ImageType> operator;
 *  operator->SetOrder(1);
 *  operator->SetDirection(0);
 *  operator->CreateDirectional();
 *
 * itk::NeighborhoodIterator<ImageType>
 *   iterator(operator->GetRadius(), myImage, myImage->GetRequestedRegion());
 *
 * iterator.SetToBegin();
 * while ( ! iterator.IsAtEnd() )
 * {
 *   std::cout << "Derivative at index " << iterator.GetIndex() << is <<
 *     IP(iterator, operator) << std::endl;
 *   ++iterator;
 * } 
 * \endcode
 *
 * Most of the work for the programmer in the code above is in setting up for
 * the iteration.  There are three steps.  First an inner product function
 * object is created which will be used to effect convolution with the
 * derivative kernel.  Setting up the derivative kernel, DerivativeOperator,
 * involves setting the order and direction of the derivative.  Finally, we
 * create an iterator over the RequestedRegion of the itk::Image (see Image)
 * using the radius of the derivative kernel as the size.
 *
 * Itk iterators only loosely follow STL conventions.  Notice that instead of
 * asking myImage for myImage.begin() and myImage.end(), iterator.SetToBegin()
 * and iterator.IsAtEnd() are called.  Itk iterators are typically more complex
 * objects than traditional, pointer-style STL iterators, and the increased
 * overhead required to conform to the complete STL API is not always
 * justified.
 *
 * The API for creating and manipulating a NeighborhoodIterator mimics
 * that of the itk::ImageIterators.  Like the itk::ImageIterator, a
 * ConstNeighborhoodIterator is defined on a region of interest in an itk::Image.
 * Iteration is constrained within that region of interest.
 *
 * A NeighborhoodIterator is constructed as a container of pointers (offsets)
 * to a geometric neighborhood of image pixels.  As the central pixel position
 * in the mask is moved around the image, the neighboring pixel pointers
 * (offsets) are moved accordingly.
 *
 * A \em pixel \em neighborhood is defined as a central pixel location and an
 * N-dimensional radius extending outward from that location.
 
 * Pixels in a neighborhood can be accessed through a NeighborhoodIterator
 * like elements in an array.  For example, a 2D neighborhood with radius 1x2
 * has indices:
 *
 * \code
 *
 * 0  1  2  3  4
 * 5  6  7  8  9
 * 10 11 12 13 14
 *
 * \endcode
 *
 * Now suppose a NeighborhoodIterator with the above dimensions is constructed
 * and positioned over a neighborhood of values in an Image:
 *
 * \code
 *
 * 1.2 1.3 1.8 1.4 1.1
 * 1.8 1.1 0.7 1.0 1.0
 * 2.1 1.9 1.7 1.4 2.0
 *
 * \code
 *
 * Shown below is some sample pixel access code and the values that it returns.
 *
 * \code
 *
 * ::size_t c = (::size_t) (iterator.Size() / 2); // get offset of center pixel
 * ::size_t s = iterator.GetStride(1);            // y-dimension step size
 *
 * std::cout << iterator.GetPixel(7)      << std::endl;
 * std::cout << iterator.GetCenterPixel() << std::endl;
 * std::cout << iterator.GetPixel(c)      << std::endl;
 * std::cout << iterator.GetPixel(c-1)    << std::endl;
 * std::cout << iterator.GetPixel(c-s)    << std::endl;
 * std::cout << iterator.GetPixel(c-s-1)  << std::endl; 
 * std::cout << *iterator[c]              << std::endl;
 *
 * \endcode
 *
 * Results:
 *
 * \code
 * 0.7
 * 0.7
 * 0.7
 * 1.1
 * 1.8
 * 1.3
 * 0.7
 * \endcode
 *
 * Use of GetPixel() is preferred over the *iterator[] form, and can be used
 * without loss of efficiency in most cases. Some variations (subclasses) of
 * NeighborhoodIterators may exist which do not support the latter
 * API. Corresponding SetPixel() methods exist to modify pixel values in
 * non-const NeighborhoodIterators.
 *
 * NeighborhoodIterators are "bidirectional iterators". They move only in two
 * directions through the data set.  These directions correspond to the layout
 * of the image data in memory and not to spatial directions of the
 * N-dimensional itk::Image.  Iteration always proceeds along the fastest
 * increasing dimension (as defined by the layout of the image data) .  For
 * itk::Image this is the first dimension specified (i.e. for 3-dimensional
 * (x,y,z) NeighborhoodIterator proceeds along the x-dimension) (For random
 * access iteration through N-dimensional indicies, use
 * RandomAccessNeighborhoodIterator.)
 *
 * Each subclass of a ConstNeighborhoodIterator may also define its own
 * mechanism for iteration through an image.  In general, the Iterator does not
 * directly keep track of its spatial location in the image, but uses a set of
 * internal loop variables and offsets to trigger wraps at itk::Image region
 * boundaries, and to identify the end of the itk::Image region.
 *
 * NeighborhoodIterator does not perform bounds checking before dereferencing
 * it pixels.  It is up to the user to make sure the iteration region is
 * sufficiently padded for the neighborhood radius.  See
 * SmartNeighborhoodIterator for a way to automatically handle bounds
 * conditions.
 * 
 * \todo Better support for regions with negative indicies.
 * \todo Add Begin() and End() methods?
 *
 * \sa Image \sa Neighborhood \sa ImageIterator \sa NeighborhoodIterator
 * \sa SmartNeighborhoodIterator \sa RandomAccessNeighborhoodIterator
 **/
template<class TImage>
class ITK_EXPORT NeighborhoodIterator
  :  public ConstNeighborhoodIterator<TImage>
{
public:
  /** 
   * Standard "Self" & Superclass typedef support.
   */
  typedef NeighborhoodIterator Self;
  typedef ConstNeighborhoodIterator<TImage> Superclass;

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
  NeighborhoodIterator(): Superclass() {}
  
  /**
   * Copy constructor
   */
  NeighborhoodIterator( const NeighborhoodIterator &n )
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
  NeighborhoodIterator(const SizeType &radius,
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
   * neighborhood that are pointed to by this NeighborhoodIterator with
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
#include "itkNeighborhoodIterator.txx"
#endif

#endif
