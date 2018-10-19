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

#include "itkRieszUtilities.h"

namespace itk
{
namespace utils
{

long
Factorial(const long n)
{
  if (n < 1)
  {
    return 1;
  }
  return n * itk::utils::Factorial(n - 1);
}

unsigned int
ComputeNumberOfComponents(const unsigned int & order, const unsigned int & dimension)
{
  return itk::utils::Factorial(order + dimension - 1) /
         (itk::utils::Factorial(dimension - 1) * itk::utils::Factorial(order));
}

// explicit instantiation of template functions with std::vector<unsigned int>
template <>
void
ComputeUniqueIndices<std::vector<unsigned int>, 3>(
  std::vector<unsigned int>                                                      subIndex,
  std::set<std::vector<unsigned int>, std::greater<std::vector<unsigned int>>> & uniqueIndices,
  unsigned int                                                                   init);

template <>
void
ComputeUniqueIndices<std::vector<unsigned int>, 2>(
  std::vector<unsigned int>                                                      subIndex,
  std::set<std::vector<unsigned int>, std::greater<std::vector<unsigned int>>> & uniqueIndices,
  unsigned int                                                                   init);

template <>
std::set<std::vector<unsigned int>, std::greater<std::vector<unsigned int>>>
ComputeAllPermutations<std::vector<unsigned int>>(
  const std::set<std::vector<unsigned int>, std::greater<std::vector<unsigned int>>> & uniqueIndices);

template <>
std::set<std::vector<unsigned int>, std::greater<std::vector<unsigned int>>>
ComputeAllPossibleIndices<std::vector<unsigned int>, 3>(const unsigned int & order);

template <>
std::set<std::vector<unsigned int>, std::greater<std::vector<unsigned int>>>
ComputeAllPossibleIndices<std::vector<unsigned int>, 2>(const unsigned int & order);

template <>
bool
LessOrEqualIndiceComparisson<std::vector<unsigned int>, 3>(const std::vector<unsigned int> & rhs,
                                                           const std::vector<unsigned int> & lhs);

template <>
bool
LessOrEqualIndiceComparisson<std::vector<unsigned int>, 2>(const std::vector<unsigned int> & rhs,
                                                           const std::vector<unsigned int> & lhs);
} // end namespace utils
} // end namespace itk
