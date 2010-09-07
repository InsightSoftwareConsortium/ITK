/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGreyColormapFunctor.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGreyColormapFunctor_txx
#define __itkGreyColormapFunctor_txx

#include "itkGreyColormapFunctor.h"

namespace itk
{
namespace Functor
{
template< class TScalar, class TRGBPixel >
typename GreyColormapFunctor< TScalar, TRGBPixel >::RGBPixelType
GreyColormapFunctor< TScalar, TRGBPixel >
::operator()(const TScalar & v) const
{
  // Map the input scalar between [0, 1].
  RealType value = this->RescaleInputValue(v);

  // Set the rgb components after rescaling the values.
  RGBPixelType pixel;

  pixel[0] = this->RescaleRGBComponentValue(value);
  pixel[1] = pixel[0];
  pixel[2] = pixel[0];

  return pixel;
}
} // end namespace Functor
} // end namespace itk

#endif
