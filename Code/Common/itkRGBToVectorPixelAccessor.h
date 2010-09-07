/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGBToVectorPixelAccessor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRGBToVectorPixelAccessor_h
#define __itkRGBToVectorPixelAccessor_h

#include "itkRGBPixel.h"
#include "itkVector.h"

namespace itk
{
namespace Accessor
{
/**
 * \class RGBToVectorPixelAccessor
 * \brief Give access to a RGBPixel as if it were a Vector type.
 *
 * This class is intended to be used as parameter of
 * an ImageAdaptor to make an RGBPixel image appear as being
 * an image of Vector pixel type.
 *
 * \sa ImageAdaptor
 * \ingroup ImageAdaptors
 *
 */

template< class T >
class ITK_EXPORT RGBToVectorPixelAccessor
{
public:
  /** Standard class typedefs. */
  typedef   RGBToVectorPixelAccessor Self;

  /** External typedef. It defines the external aspect
    * that this class will exhibit */
  typedef  Vector< T, 3 > ExternalType;

  /** Internal typedef. It defines the internal real
   * representation of data */
  typedef   RGBPixel< T > InternalType;

  /** Write access to the RGBToVector component */
  inline void Set(InternalType & output, const ExternalType & input) const
  {
    output[0] = input[0];
    output[1] = input[1];
    output[2] = input[2];
  }

  /** Read access to the RGBToVector component */
  inline ExternalType Get(const InternalType & input) const
  {
    ExternalType v( input.GetDataPointer() );

    return v;
  }

private:
};
}  // end namespace Accessor
}  // end namespace itk

#endif
