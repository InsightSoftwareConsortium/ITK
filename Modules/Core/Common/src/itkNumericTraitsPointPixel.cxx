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
#include "itkNumericTraitsPointPixel.h"

namespace itk
{
itkStaticNumericTraitsGenericArrayDimensionsMacro(Point, char);
itkStaticNumericTraitsGenericArrayDimensionsMacro(Point, unsigned char);
itkStaticNumericTraitsGenericArrayDimensionsMacro(Point, signed char);
itkStaticNumericTraitsGenericArrayDimensionsMacro(Point, short);
itkStaticNumericTraitsGenericArrayDimensionsMacro(Point, unsigned short);
itkStaticNumericTraitsGenericArrayDimensionsMacro(Point, int);
itkStaticNumericTraitsGenericArrayDimensionsMacro(Point, unsigned int);
itkStaticNumericTraitsGenericArrayDimensionsMacro(Point, long);
itkStaticNumericTraitsGenericArrayDimensionsMacro(Point, unsigned long);
itkStaticNumericTraitsGenericArrayDimensionsMacro(Point, float);
itkStaticNumericTraitsGenericArrayDimensionsMacro(Point, double);
itkStaticNumericTraitsGenericArrayDimensionsMacro(Point, long double);
itkStaticNumericTraitsGenericArrayDimensionsMacro(Point, long long);
itkStaticNumericTraitsGenericArrayDimensionsMacro(Point, unsigned long long);
} // end namespace itk
