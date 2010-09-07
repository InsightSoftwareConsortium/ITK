/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsRGBAPixel.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkRGBAPixel.h"

namespace itk
{
// All the specializations that were here previously have now been
// replaced with a single template in the header file.
//

//
// Helper macro for initializing the Zero and One static member of the
// NumericTraits<>.
//
#define RGBAPIXELSTATICTRAITSMACRO(T)                                                                     \
  template< >                                                                                             \
  const RGBAPixel< T >  NumericTraits< RGBAPixel< T > >::Zero = RGBAPixel< T >(NumericTraits< T >::Zero); \
  template< >                                                                                             \
  const RGBAPixel< T >  NumericTraits< RGBAPixel< T > >::One = RGBAPixel< T >(NumericTraits< T >::One);

//
// List here the specializations of the Traits:
//
RGBAPIXELSTATICTRAITSMACRO(char);
RGBAPIXELSTATICTRAITSMACRO(unsigned char);
RGBAPIXELSTATICTRAITSMACRO(short);
RGBAPIXELSTATICTRAITSMACRO(unsigned short);
RGBAPIXELSTATICTRAITSMACRO(int);
RGBAPIXELSTATICTRAITSMACRO(unsigned int);
RGBAPIXELSTATICTRAITSMACRO(long);
RGBAPIXELSTATICTRAITSMACRO(unsigned long);
RGBAPIXELSTATICTRAITSMACRO(float);
RGBAPIXELSTATICTRAITSMACRO(double);
} // end namespace itk
