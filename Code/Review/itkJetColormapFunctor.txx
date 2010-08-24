/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkJetColormapFunctor.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkJetColormapFunctor_txx
#define __itkJetColormapFunctor_txx

#include "itkJetColormapFunctor.h"

namespace itk
{
namespace Functor
{
template< class TScalar, class TRGBPixel >
typename JetColormapFunctor< TScalar, TRGBPixel >::RGBPixelType
JetColormapFunctor< TScalar, TRGBPixel >
::operator()(const TScalar & v) const
{
  // Map the input scalar between [0, 1].
  RealType value = this->RescaleInputValue(v);

  // Apply the color mapping.
  RealType red = -vnl_math_abs( 3.75 * ( value - 0.75 ) ) + 1.625;

  red = vnl_math_min(red, 1.0);
  red = vnl_math_max(0.0, red);

  RealType green = -vnl_math_abs( 3.75 * ( value - 0.5 ) ) + 1.625;
  green = vnl_math_min(green, 1.0);
  green = vnl_math_max(0.0, green);

  RealType blue = -vnl_math_abs( 3.75 * ( value - 0.25 ) ) + 1.625;
  blue = vnl_math_min(blue, 1.0);
  blue = vnl_math_max(0.0, blue);

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
