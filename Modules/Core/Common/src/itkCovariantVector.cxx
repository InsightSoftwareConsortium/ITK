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
#include "itkCovariantVector.h"

namespace itk
{
/**
 *  Methods specialized by dimension
 *
 *  Limitations of VC++ on partial specialization
 *  force us to define the following method for
 *  a set of types.
 */

void
ITKCommon_EXPORT CrossProduct(CovariantVector< double, 3 > & c,
                              const Vector< double, 3 > & a,
                              const Vector< double, 3 > & b)
{
  c[0] = a[1] * b[2] - a[2] * b[1];
  c[1] = a[2] * b[0] - a[0] * b[2];
  c[2] = a[0] * b[1] - a[1] * b[0];
}

void
ITKCommon_EXPORT CrossProduct(CovariantVector< float, 3 > & c,
                              const Vector< float, 3 > & a,
                              const Vector< float, 3 > & b)
{
  c[0] = a[1] * b[2] - a[2] * b[1];
  c[1] = a[2] * b[0] - a[0] * b[2];
  c[2] = a[0] * b[1] - a[1] * b[0];
}

void
ITKCommon_EXPORT CrossProduct(CovariantVector< int, 3 > & c, const Vector< int, 3 > & a, const Vector< int, 3 > & b)
{
  c[0] = a[1] * b[2] - a[2] * b[1];
  c[1] = a[2] * b[0] - a[0] * b[2];
  c[2] = a[0] * b[1] - a[1] * b[0];
}
} // end namespace itk
