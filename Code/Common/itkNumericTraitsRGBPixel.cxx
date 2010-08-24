/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsRGBPixel.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkRGBPixel.h"

namespace itk
{
// All the specializations that were here previously have now been
// replaced with a single template in the header file.
//

//
// Helper macro for initializing the Zero and One static member of the
// NumericTraits<>.
//
#define RGBPIXELSTATICTRAITSMACRO(T)                                                                   \
  template< >                                                                                          \
  const RGBPixel< T >  NumericTraits< RGBPixel< T > >::Zero = RGBPixel< T >(NumericTraits< T >::Zero); \
  template< >                                                                                          \
  const RGBPixel< T >  NumericTraits< RGBPixel< T > >::One = RGBPixel< T >(NumericTraits< T >::One);

//
// List here the specializations of the Traits:
//
RGBPIXELSTATICTRAITSMACRO(char);
RGBPIXELSTATICTRAITSMACRO(unsigned char);
RGBPIXELSTATICTRAITSMACRO(short);
RGBPIXELSTATICTRAITSMACRO(unsigned short);
RGBPIXELSTATICTRAITSMACRO(int);
RGBPIXELSTATICTRAITSMACRO(unsigned int);
RGBPIXELSTATICTRAITSMACRO(long);
RGBPIXELSTATICTRAITSMACRO(unsigned long);
RGBPIXELSTATICTRAITSMACRO(float);
RGBPIXELSTATICTRAITSMACRO(double);
} // end namespace itk
