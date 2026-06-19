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
#ifndef itkEigenDecompositionSignConvention_h
#define itkEigenDecompositionSignConvention_h

#include "vnl/vnl_matrix.h"
#include <cmath>

namespace itk::detail
{

/** Canonicalize the sign of every eigenvector column of \a V so that its
 * largest-magnitude entry is positive (tie-break by lowest row index). An
 * eigenvector's sign is mathematically arbitrary and otherwise depends on
 * solver internals and SIMD width; pinning it this way makes a real-valued
 * eigendecomposition bit-reproducible across platforms. */
template <typename T>
void
CanonicalizeEigenvectorColumnSigns(vnl_matrix<T> & V)
{
  const unsigned int rows = V.rows();
  for (unsigned int j = 0; j < V.cols(); ++j)
  {
    unsigned int pivot = 0;
    for (unsigned int i = 1; i < rows; ++i)
    {
      if (std::abs(V(i, j)) > std::abs(V(pivot, j)))
      {
        pivot = i;
      }
    }
    if (V(pivot, j) < T{ 0 })
    {
      V.scale_column(j, T{ -1 });
    }
  }
}

} // namespace itk::detail

#endif // itkEigenDecompositionSignConvention_h
