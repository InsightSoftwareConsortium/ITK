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
#ifndef itkDerivativeOperator_hxx
#define itkDerivativeOperator_hxx
#include "itkDerivativeOperator.h"

#include "itkNumericTraits.h"

namespace itk
{
template< typename TPixel, unsigned int VDimension, typename TAllocator >
typename DerivativeOperator< TPixel, VDimension, TAllocator >
::CoefficientVector
DerivativeOperator< TPixel, VDimension, TAllocator >
::GenerateCoefficients()
{
  unsigned int       i;
  unsigned int       j;
  PixelRealType      previous;
  PixelRealType      next;
  const unsigned int w = 2 * ( ( m_Order + 1 ) / 2 ) + 1;
  CoefficientVector  coeff(w);

  coeff[w / 2] = 1.0;
  for ( i = 0; i < m_Order / 2; i++ )
    {
    previous = coeff[1] - 2 * coeff[0];
    for ( j = 1; j < w - 1; j++ )
      {
      next = coeff[j - 1]  + coeff[j + 1] - 2 * coeff[j];
      coeff[j - 1] = previous;
      previous = next;
      }
    next = coeff[j - 1] - 2 * coeff[j];
    coeff[j - 1] = previous;
    coeff[j] = next;
    }
  for ( i = 0; i < m_Order % 2; i++ )
    {
    previous =  0.5 * coeff[1];
    for ( j = 1; j < w - 1; j++ )
      {
      next = -0.5 * coeff[j - 1] + 0.5 * coeff[j + 1];
      coeff[j - 1] = previous;
      previous = next;
      }
    next = -0.5 * coeff[j - 1];
    coeff[j - 1] = previous;
    coeff[j] = next;
    }

  return coeff;
}
} // namespace itk

#endif
