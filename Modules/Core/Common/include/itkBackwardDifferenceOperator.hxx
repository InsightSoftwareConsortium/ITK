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
#ifndef __itkBackwardDifferenceOperator_hxx
#define __itkBackwardDifferenceOperator_hxx

#include "itkBackwardDifferenceOperator.h"

namespace itk
{
template< class TPixel, unsigned int TDimension, class TAllocator >
typename BackwardDifferenceOperator< TPixel, TDimension, TAllocator >
::CoefficientVector
BackwardDifferenceOperator< TPixel, TDimension, TAllocator >
::GenerateCoefficients()
{
  CoefficientVector coeff(3);

  coeff[0] = -1.0f *  NumericTraits< PixelType >::One;
  coeff[1] =  NumericTraits< PixelType >::One;
  coeff[2] =  NumericTraits< PixelType >::Zero;

  return coeff;
}

template< class TPixel, unsigned int TDimension, class TAllocator >
void
BackwardDifferenceOperator< TPixel, TDimension, TAllocator >
::Fill(const CoefficientVector & coeff)
{
  this->FillCenteredDirectional(coeff);
}

/** Constructor. */
template< class TPixel, unsigned int TDimension, class TAllocator >
BackwardDifferenceOperator< TPixel, TDimension, TAllocator >
::BackwardDifferenceOperator()
{

}

} // namespace itk

#endif
