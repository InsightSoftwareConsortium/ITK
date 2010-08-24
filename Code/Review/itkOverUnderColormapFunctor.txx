/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOverUnderColormapFunctor.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkOverUnderColormapFunctor_txx
#define __itkOverUnderColormapFunctor_txx

#include "itkOverUnderColormapFunctor.h"

namespace itk
{
namespace Functor
{
template< class TScalar, class TRGBPixel >
typename OverUnderColormapFunctor< TScalar, TRGBPixel >::RGBPixelType
OverUnderColormapFunctor< TScalar, TRGBPixel >
::operator()(const TScalar & v) const
{
  // Map the input scalar between [0, 1].
  RealType value = this->RescaleInputValue(v);

  // Apply the color mapping.
  RealType red = value;
  RealType green = value;
  RealType blue = value;

  if ( value == 0.0 )
    {
    // pixel is saturated in the dark
    red = 0.0;
    green = 0.0;
    blue = 1.0;
    }
  else if ( value == 1.0 )
    {
    // pixel is saturated in the white
    red = 1.0;
    green = 0.0;
    blue = 0.0;
    }

  // Set the rgb components after rescaling the values.
  RGBPixelType pixel;

  pixel[0] = this->RescaleRGBComponentValue(red);
  pixel[1] = this->RescaleRGBComponentValue(green);
  pixel[2] = this->RescaleRGBComponentValue(blue);

  return pixel;
}
} // end namespace Functor
} // end namespace itk

#endif
