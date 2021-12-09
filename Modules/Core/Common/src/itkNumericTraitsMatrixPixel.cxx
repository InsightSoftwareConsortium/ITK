/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkNumericTraitsMatrixPixel.h"

namespace itk
{
itkStaticNumericTraitsGenericMatrixDimensionsMacro(Matrix, char);
itkStaticNumericTraitsGenericMatrixDimensionsMacro(Matrix, unsigned char);
itkStaticNumericTraitsGenericMatrixDimensionsMacro(Matrix, signed char);
itkStaticNumericTraitsGenericMatrixDimensionsMacro(Matrix, short);
itkStaticNumericTraitsGenericMatrixDimensionsMacro(Matrix, unsigned short);
itkStaticNumericTraitsGenericMatrixDimensionsMacro(Matrix, int);
itkStaticNumericTraitsGenericMatrixDimensionsMacro(Matrix, unsigned int);
itkStaticNumericTraitsGenericMatrixDimensionsMacro(Matrix, long);
itkStaticNumericTraitsGenericMatrixDimensionsMacro(Matrix, unsigned long);
itkStaticNumericTraitsGenericMatrixDimensionsMacro(Matrix, float);
itkStaticNumericTraitsGenericMatrixDimensionsMacro(Matrix, double);
itkStaticNumericTraitsGenericMatrixDimensionsMacro(Matrix, long double);
itkStaticNumericTraitsGenericMatrixDimensionsMacro(Matrix, long long);
itkStaticNumericTraitsGenericMatrixDimensionsMacro(Matrix, unsigned long long);
} // end namespace itk
