/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDefaultImageTraits.h
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
#ifndef __itkDefaultImageTraits_h
#define __itkDefaultImageTraits_h

#include "itkImageRegion.h"
#include "itkValarrayImageContainer.h"

namespace itk
{


/**
 * Default ImageTraits for any PixelType.
 *
 * \sa Image
 */
template <typename TPixelType,
          unsigned int VImageDimension,
          typename TPixelContainer = ValarrayImageContainer<unsigned long, TPixelType> >
class DefaultImageTraits
{
public:
  /**
   * The pixel type of the image.
   */
  typedef TPixelType PixelType;

  /**
   * The dimension of the image.
   */
  enum { ImageDimension = VImageDimension };
  
  /**
   * The container of Pixels for the image.
   */
  typedef TPixelContainer PixelContainer;

  /** 
   * Index typedef support. An index is used to access pixel values.
   */
  typedef Index<ImageDimension>  IndexType;

  /** 
   * Offset typedef support. An offset is used to access pixel values.
   */
  typedef Offset<ImageDimension>  OffsetType;

  /** 
   * Size typedef support. A size is used to define region bounds.
   */
  typedef Size<ImageDimension>  SizeType;

  /** 
   * Region typedef support. A region is used to specify a subset of an image.
   */
  typedef ImageRegion<ImageDimension>  RegionType;
};


} // namespace itk

#endif
