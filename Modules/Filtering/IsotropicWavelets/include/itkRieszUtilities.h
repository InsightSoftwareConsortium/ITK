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
#ifndef itkRieszUtilities_h
#define itkRieszUtilities_h

#include <set>
#include <vector>
#include <functional>
#include <algorithm>

namespace itk
{
namespace utils
{
/// Factorial
long
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
unsigned int
ComputeNumberOfComponents(const unsigned int & order, const unsigned int & dimension);

/**
 * Compute all possible unique indices given the subIndice: (X, 0, ..., 0).
 * Where X can be any number greater than 0, but probably want to use this->m_Order.
 *
 * TIndicesArrayType = std::vector<unsigned int>
 * or any std array type with begin(), end() methods.
 *
 * @param subIndice Indice (X,0,...,0) where X > 0.
 * @param uniqueIndices Reference to set that store results.
 * @param init position to evaluate  subIndice. Needed for recursion purposes.
 */
template <typename TIndicesArrayType, unsigned int VImageDimension>
void
ComputeUniqueIndices(TIndicesArrayType                                              subIndice,
                     std::set<TIndicesArrayType, std::greater<TIndicesArrayType>> & uniqueIndices,
                     unsigned int                                                   init = 0)
{
  unsigned int subIndiceSize = static_cast<unsigned int>(subIndice.size());

  if (init == subIndiceSize - 1)
  {
    return;
  }

  // If OK, store it.
  if (std::distance(subIndice.begin(),
                    std::max_element(subIndice.begin(), subIndice.end(), std::greater<unsigned int>())) <=
      VImageDimension - 1)
  {
    TIndicesArrayType subIndiceCopy = subIndice;
    std::sort(subIndiceCopy.rbegin(), subIndiceCopy.rend());
    uniqueIndices.insert(subIndiceCopy);
  }
  else
  {
    // Process remaining index positions in this branch.
    itk::utils::ComputeUniqueIndices<TIndicesArrayType, VImageDimension>(subIndice, uniqueIndices, init + 1);
    return;
  }

  unsigned int first = --subIndice[init];
  ++subIndice[init + 1];
  // Stop
  if (first == 0)
  {
    return;
  }
  // Process modified subIndice.
  itk::utils::ComputeUniqueIndices<TIndicesArrayType, VImageDimension>(subIndice, uniqueIndices, init);
  // Process modified init.
  itk::utils::ComputeUniqueIndices<TIndicesArrayType, VImageDimension>(subIndice, uniqueIndices, init + 1);
}

/**
 * Compute all the permutations from a set of uniqueIndices.
 */
template <typename TIndicesArrayType>
std::set<TIndicesArrayType, std::greater<TIndicesArrayType>>
ComputeAllPermutations(const std::set<TIndicesArrayType, std::greater<TIndicesArrayType>> & uniqueIndices)
{
  typedef std::set<TIndicesArrayType, std::greater<TIndicesArrayType>> SetType;
  SetType                                                              out;
  for (typename SetType::const_iterator it = uniqueIndices.begin(); it != uniqueIndices.end(); ++it)
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
 * The order imposes the constain:
 * \f[ \sum_{i}^{ImageDimension} \text{indice}[i] = \text{order} \f]
 * where \f$ \text{indice}[i]>=0 \f$
 */
template <typename TIndicesArrayType, unsigned int VImageDimension>
std::set<TIndicesArrayType, std::greater<TIndicesArrayType>>
ComputeAllPossibleIndices(const unsigned int & order)
{
  typedef std::set<TIndicesArrayType, std::greater<TIndicesArrayType>> SetType;
  SetType                                                              uniqueIndices;
  TIndicesArrayType                                                    indice(VImageDimension);
  indice[0] = order;
  itk::utils::ComputeUniqueIndices<TIndicesArrayType, VImageDimension>(indice, uniqueIndices, 0);
  return itk::utils::ComputeAllPermutations<TIndicesArrayType>(uniqueIndices);
}

template <typename TIndicesArrayType, unsigned int VImageDimension>
bool
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
