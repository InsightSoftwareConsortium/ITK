/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDefaultImageTraits.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDefaultImageTraits_h
#define __itkDefaultImageTraits_h

#include "itkImageRegion.h"
#include "itkValarrayImageContainer.h"

namespace itk
{
/** \class DefaultImageTraits
 *
 * \brief Default ImageTraits for any PixelType.
 *
 * \sa Image
 * \ingroup ImageObjects
 */
template< typename TPixelType,
          unsigned int VImageDimension,
          typename TPixelContainer = ValarrayImageContainer< unsigned long, TPixelType > >
class DefaultImageTraits
{
public:
  /** The pixel type of the image. */
  typedef TPixelType PixelType;

  /** The dimension of the image. */
  itkStaticConstMacro(ImageDimension, unsigned int, VImageDimension);

  /** The container of Pixels for the image. */
  typedef TPixelContainer PixelContainer;

  /** Index typedef support. An index is used to access pixel values. */
  typedef Index< VImageDimension > IndexType;

  /** Offset typedef support. An offset is used to access pixel values. */
  typedef Offset< VImageDimension > OffsetType;

  /** Size typedef support. A size is used to define region bounds. */
  typedef Size< VImageDimension > SizeType;

  /** Region typedef support. A region is used to specify a subset of an image.
    */
  typedef ImageRegion< VImageDimension > RegionType;
};
} // namespace itk

#endif
