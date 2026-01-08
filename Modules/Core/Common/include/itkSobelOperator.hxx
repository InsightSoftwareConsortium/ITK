/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkSobelOperator_hxx
#define itkSobelOperator_hxx

#include "itkIndexRange.h"
#include "itkObject.h"

namespace itk
{
template <typename TPixel, unsigned int VDimension, typename TAllocator>
void
SobelOperator<TPixel, VDimension, TAllocator>::Fill(const CoefficientVector & coeff)
{
  this->InitializeToZero();

  // Note that this code is only good for 2d and 3d operators.  Places the
  // coefficients in the exact center of the neighborhood
  const unsigned int center = this->GetCenterNeighborhoodIndex();

  using IndexType = Index<VDimension>;

  unsigned int coeff_index = 0;
  for (const IndexType & index :
       ImageRegionIndexRange<VDimension>(ImageRegion{ IndexType::Filled(-1), SizeType::Filled(3) }))
  {
    auto pos = static_cast<int>(center);

    for (unsigned int i{}; i < VDimension; ++i)
    {
      pos += static_cast<int>(index[i] * this->GetStride(i));
    }

    // Note, The following line copies the double precision
    // coefficients of SobelOperator to the pixel type
    // of the neighborhood operator which may not support
    // negative numbers, or floating point numbers.
    this->operator[](pos) = static_cast<TPixel>(coeff[coeff_index]);

    ++coeff_index;
  }
}

template <typename TPixel, unsigned int VDimension, typename TAllocator>
auto
SobelOperator<TPixel, VDimension, TAllocator>::GenerateCoefficients() -> CoefficientVector
{
  const unsigned int direction = this->GetDirection();

  if constexpr (VDimension == 2)
  {
    switch (direction)
    {
      case 0:
        return { -1, 0, 1, -2, 0, 2, -1, 0, 1 };
      case 1:
        return { -1, -2, -1, 0, 0, 0, 1, 2, 1 };
    }
  }
  if constexpr (VDimension == 3)
  {
    switch (direction)
    {
      case 0:
        return { -1, 0, 1, -3, 0, 3, -1, 0, 1, -3, 0, 3, -6, 0, 6, -3, 0, 3, -1, 0, 1, -3, 0, 3, -1, 0, 1 };
      case 1:
        return { -1, -3, -1, 0, 0, 0, 1, 3, 1, -3, -6, -3, 0, 0, 0, 3, 6, 3, -1, -3, -1, 0, 0, 0, 1, 3, 1 };
      case 2:
        return { -1, -3, -1, -3, -6, -3, -1, -3, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 3, 1, 3, 6, 3, 1, 3, 1 };
    }
  }
  itkExceptionMacro("The direction value (" << direction << ") should be less than the dimensionality (" << VDimension
                                            << ").");
}
} // namespace itk

#endif
