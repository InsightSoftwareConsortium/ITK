/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageBoundaryCondition.h
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
#ifndef __itkImageBoundaryCondition_h_
#define __itkImageBoundaryCondition_h_

#include "itkImage.h"
#include "itkIndex.h"
#include "itkOffset.h"
#include "itkNeighborhood.h"

namespace itk
{

/**
 *\class ImageBoundaryCondition
 * \brief A virtual base object that defines an interface to a class of
 * boundary condition objects for use by neighborhood iterators.
 *
 * A boundary condition object supplies a phantom pixel value when
 * given a neighborhood of (pointers to) image values, the (ND) index of
 * the phantom pixel, and its (ND) offset from the boundary.  The index
 * of the phantom pixel is relative to the "upper left-hand corner" of
 * the neighborhood (as opposed to its center). 
 *
 * 
 * Associated Types                 Description
 * ----------------                 -----------
 * PixelType                         The data type of the return value.
 * PixelPointerType                  A pointer to PixelType.
 * PixelPointerTypeNeighborhood      A neighborhood of PixelPointerTypes
 *                                   that points to the pixel values in
 *                                   an image neighborhood.
 * 
 * \ingroup DataRepresentation
 * \ingroup ImageObjects
 */
template <class TImageType>
class ITK_EXPORT ImageBoundaryCondition
{
public:
  /**
   * Extract information from the image type
   */
  typedef typename TImageType::PixelType PixelType;
  typedef typename TImageType::InternalPixelType *PixelPointerType;
  enum { ImageDimension = TImageType::ImageDimension };
  typedef Index<ImageDimension> IndexType;
  typedef Offset<ImageDimension> OffsetType;

  /**
   * Type of the data container passed to this function object.
   */
  typedef Neighborhood<PixelPointerType, ImageDimension> NeighborhoodType;

  /**
   * Default constructor.
   */
  ImageBoundaryCondition() {}
  
  /**
   * Returns a value for a given out-of-bounds pixel.  The arguments are the
   * phantom pixel (ND) index within the neighborhood, the pixel's offset from
   * the nearest image border pixel, and a neighborhood of pointers to pixel
   * values in the image. 
   */
  virtual PixelType operator()(const OffsetType& point_index,
                               const OffsetType &boundary_offset,
                                 const NeighborhoodType *data) const = 0;
};
  
}// end namespace itk


#endif
