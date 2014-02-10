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
#include "itkNumericTraitsDiffusionTensor3DPixel.h"

namespace itk
{
// All the specializations that were here previously have now been
// replaced with a single template in the header file.
//
// Helper macro for initializing the Zero and One static member of the
// NumericTraits<>.
//

#define DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(T)                                                      \
  template< >                                                                                           \
  ITKCommon_EXPORT const DiffusionTensor3D< T >  NumericTraits< DiffusionTensor3D< T > >::Zero = DiffusionTensor3D< T >( \
    NumericTraits< T >::Zero);                                                                          \
  template< >                                                                                           \
  ITKCommon_EXPORT const DiffusionTensor3D< T >  NumericTraits< DiffusionTensor3D< T > >::One = DiffusionTensor3D< T >( \
    NumericTraits< T >::One);

//
// List here the specializations of the Traits:
//

DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(char);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(unsigned char);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(short);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(unsigned short);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(int);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(unsigned int);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(long);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(unsigned long);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(float);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(double);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(long long);
DIFFUSIONTENSOR3DPIXELSTATICTRAITSMACRO(unsigned long long);
} // end namespace itk
