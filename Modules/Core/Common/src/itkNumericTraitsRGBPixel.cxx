/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
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
  ITKCommon_EXPORT const RGBPixel< T >  NumericTraits< RGBPixel< T > >::Zero = RGBPixel< T >(NumericTraits< T >::Zero); \
  template< >                                                                                          \
  ITKCommon_EXPORT const RGBPixel< T >  NumericTraits< RGBPixel< T > >::One = RGBPixel< T >(NumericTraits< T >::One);

//
// List here the specializations of the Traits:
//
RGBPIXELSTATICTRAITSMACRO(char);
RGBPIXELSTATICTRAITSMACRO(signed char);
RGBPIXELSTATICTRAITSMACRO(unsigned char);
RGBPIXELSTATICTRAITSMACRO(short);
RGBPIXELSTATICTRAITSMACRO(unsigned short);
RGBPIXELSTATICTRAITSMACRO(int);
RGBPIXELSTATICTRAITSMACRO(unsigned int);
RGBPIXELSTATICTRAITSMACRO(long);
RGBPIXELSTATICTRAITSMACRO(unsigned long);
RGBPIXELSTATICTRAITSMACRO(float);
RGBPIXELSTATICTRAITSMACRO(double);
RGBPIXELSTATICTRAITSMACRO(long long);
RGBPIXELSTATICTRAITSMACRO(unsigned long long);
RGBPIXELSTATICTRAITSMACRO(long double);
} // end namespace itk
