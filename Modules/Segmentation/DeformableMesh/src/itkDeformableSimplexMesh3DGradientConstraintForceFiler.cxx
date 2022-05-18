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
#include "itkDeformableSimplexMesh3DGradientConstraintForceFilter.h"

namespace itk
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const DeformableSimplexMesh3DGradientConstraintForceFilterEnums::SIDE value)
{
  return out << [value] {
    switch (value)
    {
      case DeformableSimplexMesh3DGradientConstraintForceFilterEnums::SIDE::NORMAL:
        return "itk::DeformableSimplexMesh3DGradientConstraintForceFilterEnums::SIDE::NORMAL";
      case DeformableSimplexMesh3DGradientConstraintForceFilterEnums::SIDE::INVERSE:
        return "itk::DeformableSimplexMesh3DGradientConstraintForceFilterEnums::SIDE::INVERSE";
      case DeformableSimplexMesh3DGradientConstraintForceFilterEnums::SIDE::BOTH:
        return "itk::DeformableSimplexMesh3DGradientConstraintForceFilterEnums::SIDE::BOTH";
      default:
        return "INVALID VALUE FOR itk::DeformableSimplexMesh3DGradientConstraintForceFilterEnums::SIDE";
    }
  }();
}
} // end namespace itk
