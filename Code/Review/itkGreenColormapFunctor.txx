/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGreenColormapFunctor.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGreenColormapFunctor_txx
#define __itkGreenColormapFunctor_txx

#include "itkGreenColormapFunctor.h"

namespace itk
{
namespace Functor
{
template< class TScalar, class TRGBPixel >
typename GreenColormapFunctor< TScalar, TRGBPixel >::RGBPixelType
GreenColormapFunctor< TScalar, TRGBPixel >
::operator()(const TScalar & v) const
{
  // Map the input scalar between [0, 1].
  RealType value = this->RescaleInputValue(v);

  // Set the rgb components after rescaling the values.
  RGBPixelType pixel;

  pixel[0] = 0;
  pixel[1] = this->RescaleRGBComponentValue(value);
  pixel[2] = 0;

  return pixel;
}
} // end namespace Functor
} // end namespace itk

#endif
