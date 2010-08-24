/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCoolColormapFunctor.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCoolColormapFunctor_txx
#define __itkCoolColormapFunctor_txx

#include "itkCoolColormapFunctor.h"

namespace itk
{
namespace Functor
{
template< class TScalar, class TRGBPixel >
typename CoolColormapFunctor< TScalar, TRGBPixel >::RGBPixelType
CoolColormapFunctor< TScalar, TRGBPixel >
::operator()(const TScalar & v) const
{
  // Map the input scalar between [0, 1].
  RealType value = this->RescaleInputValue(v);

  // Apply the color mapping.
  RealType red = value;

  RealType green = 1.0 - value;

  RealType blue = 1.0;

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
