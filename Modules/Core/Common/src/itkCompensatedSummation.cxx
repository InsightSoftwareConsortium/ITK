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
// To exclude the pragmas in itkCompensatedSummation.hxx since we use compiler
// flags to prevent the undesirable optimization on this file.
#define itkCompensatedSummation_cxx

#include "itkNumericTraits.h"
#include "itkCompensatedSummation.hxx"

namespace itk
{

void ITKCommon_EXPORT CompensatedSummationAddElement( float& compensation, float& sum, const float& element )
{
  CompensatedSummationAddElement( compensation, sum, element, 1 );
}
void ITKCommon_EXPORT CompensatedSummationAddElement( double& compensation, double& sum, const double& element )
{
  CompensatedSummationAddElement( compensation, sum, element, 1 );
}

} // end namespace itk
