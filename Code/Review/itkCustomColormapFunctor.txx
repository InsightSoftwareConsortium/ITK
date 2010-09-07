/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCustomColormapFunctor.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCustomColormapFunctor_txx
#define __itkCustomColormapFunctor_txx

#include "itkCustomColormapFunctor.h"

namespace itk
{
namespace Functor
{
template< class TScalar, class TRGBPixel >
typename CustomColormapFunctor< TScalar, TRGBPixel >::RGBPixelType
CustomColormapFunctor< TScalar, TRGBPixel >
::operator()(const TScalar & v) const
{
  // Map the input scalar between [0, 1].
  RealType value = this->RescaleInputValue(v);

  // Apply the color mapping.
  RealType red = 0.0;

  if ( this->m_RedChannel.size() == 1 || value == 0.0 )
    {
    red = this->m_RedChannel[0];
    }
  else if ( this->m_RedChannel.size() > 1 )
    {
    RealType     size = static_cast< RealType >( this->m_RedChannel.size() );
    unsigned int index = Math::Ceil< unsigned int >( value * ( size - 1.0 ) );
    RealType     p1 = this->m_RedChannel[index];
    RealType     m1 = this->m_RedChannel[index - 1u];
    RealType     d = p1 - m1;
    red = d * ( size - 1.0 ) * ( value - ( index - 1.0 ) / ( size - 1.0 ) )
          + m1;
    }

  RealType green = 0.0;
  if ( this->m_GreenChannel.size() == 1 || value == 0.0 )
    {
    green = this->m_GreenChannel[0];
    }
  else if ( this->m_GreenChannel.size() > 1 )
    {
    RealType     size = static_cast< RealType >( this->m_GreenChannel.size() );
    unsigned int index = Math::Ceil< unsigned int >( value * ( size - 1.0 ) );
    RealType     p1 = this->m_GreenChannel[index];
    RealType     m1 = this->m_GreenChannel[index - 1u];
    RealType     d = p1 - m1;
    green = d * ( size - 1.0 ) * ( value - ( index - 1.0 ) / ( size - 1.0 ) )
            + m1;
    }

  RealType blue = 0.0;
  if ( this->m_BlueChannel.size() == 1 || value == 0.0 )
    {
    blue = this->m_BlueChannel[0];
    }
  else if ( this->m_BlueChannel.size() > 1 )
    {
    RealType     size = static_cast< RealType >( this->m_BlueChannel.size() );
    unsigned int index = Math::Ceil< unsigned int >( value * ( size - 1.0 ) );
    RealType     p1 = this->m_BlueChannel[index];
    RealType     m1 = this->m_BlueChannel[index - 1u];
    RealType     d = p1 - m1;
    blue = d * ( size - 1.0 ) * ( value - ( index - 1.0 ) / ( size - 1.0 ) )
           + m1;
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
