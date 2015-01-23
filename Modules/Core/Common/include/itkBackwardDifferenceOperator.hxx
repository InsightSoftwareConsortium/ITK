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
#ifndef itkBackwardDifferenceOperator_hxx
#define itkBackwardDifferenceOperator_hxx

#include "itkBackwardDifferenceOperator.h"

namespace itk
{
template< typename TPixel, unsigned int TDimension, typename TAllocator >
typename BackwardDifferenceOperator< TPixel, TDimension, TAllocator >
::CoefficientVector
BackwardDifferenceOperator< TPixel, TDimension, TAllocator >
::GenerateCoefficients()
{
  CoefficientVector coeff(3);

  coeff[0] = -1.0f *  NumericTraits< PixelType >::OneValue();
  coeff[1] =  NumericTraits< PixelType >::OneValue();
  coeff[2] =  NumericTraits< PixelType >::ZeroValue();

  return coeff;
}
} // namespace itk

#endif
