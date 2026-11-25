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
#ifndef itkRieszUtilities_h
#define itkRieszUtilities_h

#include <algorithm>
#include <functional>
#include <set>
#include <vector>

#include "itkMacro.h"

#include "IsotropicWaveletsExport.h"

namespace itk
{
namespace utils
{
/// Factorial
IsotropicWavelets_EXPORT long
Factorial(long n);

/**
 * Compute number of components p(N, d), where N = Order, d = Dimension.
 * p(N,d) = (N + d - 1)!/( (d-1)! N! )
 *
 * @param order N of the Riesz transform
 * @param dimension d of the image
 *
 * @return NumberOfComponents given the order for the ImageDimension.
 */
IsotropicWavelets_EXPORT unsigned int
ComputeNumberOfComponents(const unsigned int & order, const unsigned int & dimension);

/**
 * Compute all possible unique indices given the subIndex: (X, 0, ..., 0).
 * Where X can be any number greater than 0, but probably want to use this->m_Order.
 *
 * TIndicesArrayType = std::vector<unsigned int>
 * or any std array type with begin(), end() methods.
 *
 * @param subIndex Index (X,0,...,0) where X > 0.
 * @param uniqueIndices Reference to set that store results.
 * @param init position to evaluate  subIndex. Needed for recursion purposes.
 */
template <typename TIndicesArrayType, unsigned int VImageDimension>
ITK_TEMPLATE_EXPORT void
ComputeUniqueIndices(TIndicesArrayType                                              subIndex,
                     std::set<TIndicesArrayType, std::greater<TIndicesArrayType>> & uniqueIndices,
                     unsigned int                                                   init = 0)
{
  auto subIndiceSize = static_cast<unsigned int>(subIndex.size());

  if (init == subIndiceSize - 1)
  {
    return;
  }

  // If OK, store it.
  if (std::distance(subIndex.begin(),
                    std::max_element(subIndex.begin(), subIndex.end(), std::greater<unsigned int>())) <=
      VImageDimension - 1)
  {
    TIndicesArrayType subIndiceCopy = subIndex;
    std::sort(subIndiceCopy.rbegin(), subIndiceCopy.rend());
    uniqueIndices.insert(subIndiceCopy);
  }
  else
  {
    // Process remaining index positions in this branch.
    itk::utils::ComputeUniqueIndices<TIndicesArrayType, VImageDimension>(subIndex, uniqueIndices, init + 1);
    return;
  }

  unsigned int first = --subIndex[init];
  ++subIndex[init + 1];
  // Stop
  if (first == 0)
  {
    return;
  }
  // Process modified subIndex.
  itk::utils::ComputeUniqueIndices<TIndicesArrayType, VImageDimension>(subIndex, uniqueIndices, init);
  // Process modified init.
  itk::utils::ComputeUniqueIndices<TIndicesArrayType, VImageDimension>(subIndex, uniqueIndices, init + 1);
}

/**
 * Compute all the permutations from a set of uniqueIndices.
 */
template <typename TIndicesArrayType>
ITK_TEMPLATE_EXPORT std::set<TIndicesArrayType, std::greater<TIndicesArrayType>>
ComputeAllPermutations(const std::set<TIndicesArrayType, std::greater<TIndicesArrayType>> & uniqueIndices)
{
  using SetType = std::set<TIndicesArrayType, std::greater<TIndicesArrayType>>;
  SetType out;
  for (auto it = uniqueIndices.begin(); it != uniqueIndices.end(); ++it)
  {
    out.insert(*it);
    TIndicesArrayType permutation = *it;
    while (std::prev_permutation(permutation.begin(), permutation.end()))
    {
      out.insert(permutation);
    }
  }
  return out;
}

/**
 * Compute all possible indices given an order.
 * The order imposes the constraint:
 * \f[ \sum_{i}^{ImageDimension} \text{index}[i] = \text{order} \f]
 * where \f$ \text{index}[i]>=0 \f$
 */
template <typename TIndicesArrayType, unsigned int VImageDimension>
ITK_TEMPLATE_EXPORT std::set<TIndicesArrayType, std::greater<TIndicesArrayType>>
                    ComputeAllPossibleIndices(const unsigned int & order)
{
  using SetType = std::set<TIndicesArrayType, std::greater<TIndicesArrayType>>;
  SetType           uniqueIndices;
  TIndicesArrayType index(VImageDimension);
  index[0] = order;
  itk::utils::ComputeUniqueIndices<TIndicesArrayType, VImageDimension>(index, uniqueIndices, 0);
  return itk::utils::ComputeAllPermutations<TIndicesArrayType>(uniqueIndices);
}

template <typename TIndicesArrayType, unsigned int VImageDimension>
ITK_TEMPLATE_EXPORT bool
LessOrEqualIndiceComparisson(const TIndicesArrayType & rhs, const TIndicesArrayType & lhs)
{
  for (unsigned int i = 0; i < VImageDimension; ++i)
  {
    if (rhs[i] > lhs[i])
    {
      return false;
    }
  }
  return true;
}
} // end namespace utils
} // end namespace itk

#endif
