/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPixelAccessor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPixelAccessor_h
#define __itkPixelAccessor_h

#include "itkMacro.h"
namespace itk
{
/** \class PixelAccessor
 * \brief Give access to partial aspects of a type
 *
 * PixelAccessor is templated over an internal type and an
 * external type representation. This class encapsulates a
 * customized convertion between the internal and external
 * type representations.
 *
 * PixelAccessor is designed to be used in conjuntion with
 * ImageAdaptors. An ImageAdaptor take an image and present it
 * as another image in which the pixels are a pixel-to-pixel
 * modification of the original image.
 *
 * ImageAdaptors are intended to perform task similar to ImageFilters,
 * but reducing the overhead in memory allocation, at the price of some
 * reduction in performance.
 *
 * ImageAdaptors are templated over a PixelAccessor class that
 * will define what kind of transformation is applied to the
 * pixel data.  Typical uses of PixelAccessor include basic type
 * casting, (e.g. make a float image looks like a unsigned int image).
 *
 * Every Image has a default PixelAccessor that performs an identity
 * operation. ImageIterators use the PixelAccessor defined by the image
 * in order to get and set the values of pixels.
 *
 * \ingroup ImageAdaptors
 */
template< class TInternalType, class TExternalType >
class ITK_EXPORT PixelAccessor
{
public:
  /** External typedef. It defines the external aspect
   * that this class will exhibit. */
  typedef TExternalType ExternalType;

  /** Internal typedef. It defines the internal real
   * representation of data. */
  typedef TInternalType InternalType;

  inline void Set(TInternalType & output, const TExternalType & input) const
  { output = (TInternalType)input; }

  inline TExternalType Get(const TInternalType & input) const
  { return (TExternalType)input; }
};
} // end namespace itk

#endif
