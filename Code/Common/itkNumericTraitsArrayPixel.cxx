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
#include "itkNumericTraitsArrayPixel.h"

namespace itk
{
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, char);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, unsigned char);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, signed char);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, short);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, unsigned short);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, int);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, unsigned int);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, long);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, unsigned long);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, float);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, double);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, long double);
} // end namespace itk
