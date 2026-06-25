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
#ifndef itkSpecialFunctions_h
#define itkSpecialFunctions_h

#include "itkeigen/unsupported/Eigen/SpecialFunctions"
#include <type_traits>

namespace itk
{
namespace Math
{

/** \brief Regularized lower incomplete gamma P(a, x) = γ(a, x) / Γ(a). */
template <typename TReal>
TReal
IncompleteGammaP(TReal a, TReal x)
{
  static_assert(std::is_floating_point_v<TReal>, "IncompleteGammaP requires a floating-point type");
  return Eigen::numext::igamma(a, x);
}

/** \brief Regularized incomplete beta I_x(a, b). */
template <typename TReal>
TReal
RegularizedIncompleteBeta(TReal a, TReal b, TReal x)
{
  static_assert(std::is_floating_point_v<TReal>, "RegularizedIncompleteBeta requires a floating-point type");
  return Eigen::numext::betainc(a, b, x);
}

} // namespace Math
} // namespace itk

#endif // itkSpecialFunctions_h
