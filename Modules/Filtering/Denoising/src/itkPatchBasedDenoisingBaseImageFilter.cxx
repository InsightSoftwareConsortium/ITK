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
#include "itkPatchBasedDenoisingBaseImageFilter.h"

namespace itk
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const PatchBasedDenoisingBaseImageFilterEnums::NoiseModel value)
{
  return out << [value] {
    switch (value)
    {
      case PatchBasedDenoisingBaseImageFilterEnums::NoiseModel::NOMODEL:
        return "itk::PatchBasedDenoisingBaseImageFilterEnums::NoiseModel::NOMODEL";
      case PatchBasedDenoisingBaseImageFilterEnums::NoiseModel::GAUSSIAN:
        return "itk::PatchBasedDenoisingBaseImageFilterEnums::NoiseModel::GAUSSIAN";
      case PatchBasedDenoisingBaseImageFilterEnums::NoiseModel::RICIAN:
        return "itk::PatchBasedDenoisingBaseImageFilterEnums::NoiseModel::RICIAN";
      case PatchBasedDenoisingBaseImageFilterEnums::NoiseModel::POISSON:
        return "itk::PatchBasedDenoisingBaseImageFilterEnums::NoiseModel::POISSON";
      default:
        return "INVALID VALUE FOR itk::PatchBasedDenoisingBaseImageFilterEnums::NoiseModel";
    }
  }();
}

std::ostream &
operator<<(std::ostream & out, const PatchBasedDenoisingBaseImageFilterEnums::ComponentSpace value)
{
  return out << [value] {
    switch (value)
    {
      case PatchBasedDenoisingBaseImageFilterEnums::ComponentSpace::EUCLIDEAN:
        return "itk::PatchBasedDenoisingBaseImageFilterEnums::ComponentSpace::EUCLIDEAN";
      case PatchBasedDenoisingBaseImageFilterEnums::ComponentSpace::RIEMANNIAN:
        return "itk::PatchBasedDenoisingBaseImageFilterEnums::ComponentSpace::RIEMANNIAN";
      default:
        return "INVALID VALUE FOR itk::PatchBasedDenoisingBaseImageFilterEnums::ComponentSpace";
    }
  }();
}

std::ostream &
operator<<(std::ostream & out, const PatchBasedDenoisingBaseImageFilterEnums::FilterState value)
{
  return out << [value] {
    switch (value)
    {
      case PatchBasedDenoisingBaseImageFilterEnums::FilterState::UNINITIALIZED:
        return "itk::PatchBasedDenoisingBaseImageFilterEnums::FilterStateEnum::UNINITIALIZED";
      case PatchBasedDenoisingBaseImageFilterEnums::FilterState::INITIALIZED:
        return "itk::PatchBasedDenoisingBaseImageFilterEnums::FilterStateEnum::INITIALIZED";
      default:
        return "INVALID VALUE FOR itk::PatchBasedDenoisingBaseImageFilterEnums::FilterStateEnum";
    }
  }();
}
} // end namespace itk
