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
#include "itkNumericTraitsCovariantVectorPixel.h"

namespace itk
{
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, char);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, unsigned char);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, signed char);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, short);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, unsigned short);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, int);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, unsigned int);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, long);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, unsigned long);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, float);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, double);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, long double);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, long long);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, unsigned long long);
} // end namespace itk
