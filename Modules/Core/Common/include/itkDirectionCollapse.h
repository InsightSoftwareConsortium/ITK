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
#ifndef itkDirectionCollapse_h
#define itkDirectionCollapse_h

#include "itkBridgeMathDeterminant.h"

namespace itk::detail
{
enum class DirectionCollapseResult
{
  Success,
  Singular,
  InvalidStrategy
};

// Internal policy application; callers retain axis selection and diagnostics.
template <typename TDirection, typename TStrategy>
[[nodiscard]] DirectionCollapseResult
ResolveDirectionCollapse(TDirection & direction, const TStrategy strategy)
{
  switch (strategy)
  {
    case TStrategy::DIRECTIONCOLLAPSETOIDENTITY:
      direction.SetIdentity();
      return DirectionCollapseResult::Success;
    case TStrategy::DIRECTIONCOLLAPSETOSUBMATRIX:
      return bridge::Math::Determinant(direction.GetVnlMatrix()) == 0.0 ? DirectionCollapseResult::Singular
                                                                        : DirectionCollapseResult::Success;
    case TStrategy::DIRECTIONCOLLAPSETOGUESS:
      if (bridge::Math::Determinant(direction.GetVnlMatrix()) == 0.0)
      {
        direction.SetIdentity();
      }
      return DirectionCollapseResult::Success;
    default:
      return DirectionCollapseResult::InvalidStrategy;
  }
}
} // namespace itk::detail

#endif
