/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarImageRegionIterator.h
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
#ifndef __itkScalarImageRegionIterator_h
#define __itkScalarImageRegionIterator_h

#include "itkImageRegionIterator.h"
#include "itkPixelTraits.h"


namespace itk
{

/** \class ScalarImageRegionIterator
 * \brief Iterator that invokes GetScalar() on an image.
 *
 * ScalarImageRegionIterator is a templated class to represent a
 * multi-dimensional iterator. It is a specialized form of
 * ImageIterator that invokes the GetScalar() method. GetScalar() is
 * used when you want to write a filter that processes only the scalar
 * portion of a pixel.  
 *
 *
 *
 * \ingroup ImageIterators
 *
 *
 */

template<class TImage>
class ScalarImageRegionIterator : 
public ImageRegionIterator<TImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ScalarImageRegionIterator  Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageRegionIterator<TImage>  Superclass;

  /**
   * Dimension of the image the iterator walks.  This enum is needed so that
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks.
   */
  enum { ImageIteratorDimension = Superclass::ImageIteratorDimension };

  /** 
   * Index typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   */
  typedef typename Superclass::IndexType IndexType;

  /** 
   * Size typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   */
  typedef typename Superclass::SizeType SizeType;

  /** 
   * Region typedef support.
   */
  typedef typename Superclass::RegionType   RegionType;

  /**
   * Image typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   */
  typedef typename Superclass::ImageType ImageType;

  /** 
   * PixelContainer typedef support. Used to refer to the container for
   * the pixel data. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   */
  typedef typename Superclass::PixelContainer PixelContainer;
  typedef typename PixelContainer::Pointer PixelContainerPointer;

  /**
   * Internal Pixel Type
   */
  typedef typename Superclass::InternalPixelType   InternalPixelType;

  /**
   * External Pixel Type
   */
  typedef typename Superclass::PixelType   PixelType;

  /** 
   *  Accessor type that convert data between internal and external
   *  representations.
   */
  typedef typename Superclass::AccessorType     AccessorType;

  /**
   * Scalar value type typedef
   */
  typedef typename ScalarTraits<PixelType>::ScalarValueType ScalarValueType;
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ScalarImageRegionIterator, ImageRegionIterator);

  /**
   * Default constructor. Needed since we provide a cast constructor.
   */
  ScalarImageRegionIterator()
    : ImageRegionIterator<TImage>() {}
  
  /**
   * Constructor establishes an iterator to walk a particular image and a
   * particular region of that image.
   */
  ScalarImageRegionIterator(ImageType *ptr,
                            const RegionType &region)
    : ImageRegionIterator<TImage>(ptr, region) {}

  /**
   * Constructor that can be used to cast from an ImageIterator to an
   * ScalarImageRegionIterator. Many routines return an ImageIterator but for a
   * particular task, you may want an ScalarImageRegionIterator.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ScalarImageRegionIterator.
   */
  ScalarImageRegionIterator( const ImageIterator<TImage> &it)
    { this->ImageIterator<TImage>::operator=(it); }

  /**
   * Get the pixel value
   */
  ScalarValueType Get(void) const  
  { return ScalarTraits<PixelType>
      ::GetScalar(m_PixelAccessor.Get(*(m_Buffer + m_Offset))); }
  
  /**
   * Set the pixel value
   */
  void Set( const ScalarValueType value) const  
  {
    PixelType p;
    ScalarTraits<PixelType>::SetScalar( p, value );
    m_PixelAccessor.Set(*(m_Buffer + m_Offset), p);
  }

  
  /**
   * Define operator= for native types.
   */
//   void operator=(const typename TPixel::ScalarValueType v)
//     { 
//     }
  
  /**
   * Define operator= for native types.
   */
//   void operator=(const double v)
//     { 
//     }  
  
};


} // end namespace itk
  
#endif
