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
#include "itkIntTypes.h" // For size_t.
#include "itkMath.h"
#include "itkObject.h"
#include <array>

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
  if (const unsigned int direction{ this->GetDirection() }; direction < VDimension)
  {
    if constexpr (VDimension == 3)
    {
      if (m_UseLegacyCoefficients)
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
    }

    // Create N-dimensional Sobel kernels (one per axis).
    // Each kernel is size 3^N, stored in row-major order with the last dimension varying fastest.
    // Standard Sobel definition: derivative = [-1, 0, 1], smoothing = [1, 2, 1].
    // Kernel for axis a is: K_a(x) = d[x_a] * Product_{i != a} s[x_i], with x_i being element of {-1,0,1}.
    static constexpr auto sobelKernels = [] {
      constexpr int derivative[3] = { -1, 0, 1 };
      constexpr int smoothing[3] = { 1, 2, 1 };

      constexpr size_t total = Math::UnsignedPower(3, VDimension);

      // Allocate one kernel per axis
      std::array<std::array<int, total>, VDimension> kernels{};

      // Iterate over all N-D indices in {0,1,2}^N (mapping to {-1,0,1})
      for (size_t linear = 0; linear < total; ++linear)
      {
        // Convert linear index -> base-3 digits (idx[0..N-1])
        const auto idx = [linear] {
          std::array<int, VDimension> result{};
          size_t                      temp = linear;
          for (size_t i = 0; i < VDimension; ++i)
          {
            result[VDimension - 1 - i] = static_cast<int>(temp % 3);
            temp /= 3;
          }
          return result;
        }();

        // For each axis, compute Sobel value at this point
        for (size_t axis = 0; axis < VDimension; ++axis)
        {
          int val = derivative[idx[axis]];
          if (val == 0)
          { // derivative center => whole product is 0
            kernels[axis][linear] = 0;
          }
          else
          {
            for (size_t i = 0; i < VDimension; ++i)
            {
              if (i != axis)
              {
                val *= smoothing[idx[i]];
              }
            }
            kernels[axis][linear] = val;
          }
        }
      }
      return kernels;
    }();

    // Note: It appears that `sobelKernels` is in the reverse order!
    const auto & kernel = sobelKernels[VDimension - 1 - direction];
    return CoefficientVector(kernel.cbegin(), kernel.cend());
  }
  else
  {
    itkExceptionMacro("The direction value (" << direction << ") should be less than the dimensionality (" << VDimension
                                              << ").");
  }
}
} // namespace itk

#endif
