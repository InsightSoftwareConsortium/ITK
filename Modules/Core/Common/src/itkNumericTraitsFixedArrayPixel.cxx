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
#include "itkNumericTraitsFixedArrayPixel.h"

namespace itk
{
itkStaticNumericTraitsGenericArrayDimensionsMacro(FixedArray, char);
itkStaticNumericTraitsGenericArrayDimensionsMacro(FixedArray, unsigned char);
itkStaticNumericTraitsGenericArrayDimensionsMacro(FixedArray, signed char);
itkStaticNumericTraitsGenericArrayDimensionsMacro(FixedArray, short);
itkStaticNumericTraitsGenericArrayDimensionsMacro(FixedArray, unsigned short);
itkStaticNumericTraitsGenericArrayDimensionsMacro(FixedArray, int);
itkStaticNumericTraitsGenericArrayDimensionsMacro(FixedArray, unsigned int);
itkStaticNumericTraitsGenericArrayDimensionsMacro(FixedArray, long);
itkStaticNumericTraitsGenericArrayDimensionsMacro(FixedArray, unsigned long);

//
//    Subsequent Types can be found in
//
//               itkNumericTraitsFixedArrayPixel2.cxx
//
//    This was split in two files in order to help the Sun CC 5.6 compiler to
//    manage the size of the compilation unit.
//
} // end namespace itk
