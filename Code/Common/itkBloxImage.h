/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxImage.h
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
#ifndef __itkBloxImage_h
#define __itkBloxImage_h

#include "itkBloxPixel.h"
#include "itkImage.h"

namespace itk
{

/**
 * \class BloxImage
 * \brief Templated n-dimensional image class used to store linked lists.
 *
 * BloxImage is not templated over pixel type in order to ensure that the
 * pixel type is *always* a  BloxPixel. Because BloxPixels are not
 * "normal" pixels, this distinction is very important. A BloxPixel contains
 * a linked list of BloxItem's, or classes derived from BloxItem.
 *
 * By not templating over pixel type, we ensure that scalar traits are defined
 * for all derived classes of BloxImage, regardless of the type of BloxItem
 * type they are designed to address.
 *
 * Important note: classes derived from BloxImage should handle different
 * BloxItem types, *not* different BloxPixel types.
 * \ingroup ImageObjects
 *
 * */

template <unsigned int VImageDimension=3,
          class TImageTraits = DefaultImageTraits<BloxPixel, VImageDimension> >
class ITK_EXPORT BloxImage : public Image<BloxPixel, VImageDimension, TImageTraits>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef BloxImage  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Image<BloxPixel, VImageDimension, TImageTraits>  Superclass;
  
  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Pixel typedef support. Used to declare pixel type in filters
   * or other operations.
   */
  typedef BloxPixel PixelType;

  /** 
   * Internal Pixel representation. Used to maintain a uniform API
   * with Image Adaptors and allow to keep a particular internal
   * representation of data while showing a different external 
   * representation.
   */
  typedef BloxPixel InternalPixelType;

  /** 
   *  Accessor type that convert data between internal and external
   *  representations.
   */
  typedef DefaultPixelAccessor< PixelType > AccessorType;

  /**
   * Dimension of the image.  This enum is used by functions that are
   * templated over image type (as opposed to being templated over pixel
   * type and dimension) when they need compile time access to the dimension
   * of the image.
   */
  enum { ImageDimension = VImageDimension };

  /**
   * The ImageTraits for this image.
   */
  typedef TImageTraits ImageTraits;

  /*@{
   * Convenient typedefs obtained from TImageTraits template parameter.
   */
  typedef typename ImageTraits::PixelContainer PixelContainer;
  typedef typename ImageTraits::SizeType SizeType;
  typedef typename ImageTraits::IndexType IndexType;
  typedef typename ImageTraits::OffsetType OffsetType;
  typedef typename ImageTraits::RegionType RegionType;
  //@}

  /**
   * A pointer to the pixel container.
   */
  typedef typename PixelContainer::Pointer PixelContainerPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(BloxImage, Image);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  

protected:
  BloxImage();
  virtual ~BloxImage();
  BloxImage(const Self&) {}
  void operator=(const Self&) {}

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBloxImage.txx"
#endif

#endif
