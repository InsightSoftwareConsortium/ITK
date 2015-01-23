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
#ifndef itkForwardDifferenceOperator_hxx
#define itkForwardDifferenceOperator_hxx
#include "itkForwardDifferenceOperator.h"

#include "itkNumericTraits.h"

namespace itk
{
template< typename TPixel, unsigned int VDimension, typename TAllocator >
typename ForwardDifferenceOperator< TPixel, VDimension, TAllocator >
::CoefficientVector
ForwardDifferenceOperator< TPixel, VDimension, TAllocator >
::GenerateCoefficients()
{
  CoefficientVector coeff(3);

  coeff[0] = NumericTraits< PixelType >::ZeroValue();
  coeff[1] = -1.0f *  NumericTraits< PixelType >::OneValue();
  coeff[2] =  NumericTraits< PixelType >::OneValue();

  return coeff;
}
} // namespace itk

#endif
