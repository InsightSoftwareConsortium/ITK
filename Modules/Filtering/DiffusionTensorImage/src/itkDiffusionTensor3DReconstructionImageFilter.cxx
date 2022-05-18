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
#include "itkDiffusionTensor3DReconstructionImageFilter.h"

namespace itk
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const DiffusionTensor3DReconstructionImageFilterEnums::GradientImageFormat value)
{
  return out << [value] {
    switch (value)
    {
      case DiffusionTensor3DReconstructionImageFilterEnums::GradientImageFormat::GradientIsInASingleImage:
        return "itk::DiffusionTensor3DReconstructionImageFilterEnums::GradientImageFormat::GradientIsInASingleImage";
      case DiffusionTensor3DReconstructionImageFilterEnums::GradientImageFormat::GradientIsInManyImages:
        return "itk::DiffusionTensor3DReconstructionImageFilterEnums::GradientImageFormat::GradientIsInManyImages";
      case DiffusionTensor3DReconstructionImageFilterEnums::GradientImageFormat::Else:
        return "itk::DiffusionTensor3DReconstructionImageFilterEnums::GradientImageFormat::Else";
      default:
        return "INVALID VALUE FOR itk::DiffusionTensor3DReconstructionImageFilterEnums::GradientImageFormat";
    }
  }();
}
} // end namespace itk
