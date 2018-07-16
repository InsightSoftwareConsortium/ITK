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
#ifndef itkHigherOrderAccurateDerivativeOperator_hxx
#define itkHigherOrderAccurateDerivativeOperator_hxx
#include "itkHigherOrderAccurateDerivativeOperator.h"

#include "itkNumericTraits.h"

namespace itk
{

template <typename TPixel, unsigned int VDimension, typename TAllocator>
typename HigherOrderAccurateDerivativeOperator<TPixel, VDimension, TAllocator>::CoefficientVector
HigherOrderAccurateDerivativeOperator<TPixel, VDimension, TAllocator>::GenerateCoefficients()
{
  switch (m_Order)
  {
    case 1:
      return this->GenerateFirstOrderCoefficients();
    default:
      itkExceptionMacro(<< "The specified derivative order/degree is not yet supported.");
  }
}


template <typename TPixel, unsigned int VDimension, typename TAllocator>
typename HigherOrderAccurateDerivativeOperator<TPixel, VDimension, TAllocator>::CoefficientVector
HigherOrderAccurateDerivativeOperator<TPixel, VDimension, TAllocator>::GenerateFirstOrderCoefficients()
{
  unsigned int      order = this->m_OrderOfAccuracy;
  unsigned int      length = 2 * order + 1;
  CoefficientVector coeff(length);

  coeff[order + 1] = static_cast<double>(order) / (order + 1);
  coeff[order - 1] = -1 * coeff[order + 1];

  unsigned int i;
  // TODO Try to refactor this loop to pull out common multiplications
  for (i = 1; i < order; ++i)
  {
    coeff[order + 1 + i] =
      -1 * (i * static_cast<double>(order - i)) / (static_cast<double>(order + i + 1) * (i + 1)) * coeff[order + i];
    coeff[order - 1 - i] = -1 * coeff[order + 1 + i];
  }

  // We perform a flip of axes here to keep in line with
  // itk::DerivativeOperator.  The DerivativeImageFilter then calls FlipAxes(),
  // so I am not sure why they put it in reverse order in the first place.  Note
  // that a flip in this case is the same as flipping the sign.
  for (i = 0; i < length; ++i)
    coeff[i] *= -1;

  // Center point.
  coeff[order] = 0.0;

  return coeff;
}

} // namespace itk

#endif
