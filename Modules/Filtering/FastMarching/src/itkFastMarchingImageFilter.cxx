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
#include "itkFastMarchingImageFilter.h"

namespace itk
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const FastMarchingImageFilterEnums::Label value)
{
  return out << [value] {
    switch (value)
    {
      case FastMarchingImageFilterEnums::Label::FarPoint:
        return "itk::FastMarchingImageFilterEnums::Label::FarPoint";
      case FastMarchingImageFilterEnums::Label::AlivePoint:
        return "itk::FastMarchingImageFilterEnums::Label::AlivePoint";
      case FastMarchingImageFilterEnums::Label::TrialPoint:
        return "itk::FastMarchingImageFilterEnums::Label::TrialPoint";
      case FastMarchingImageFilterEnums::Label::InitialTrialPoint:
        return "itk::FastMarchingImageFilterEnums::Label::InitialTrialPoint";
      case FastMarchingImageFilterEnums::Label::OutsidePoint:
        return "itk::FastMarchingImageFilterEnums::Label::OutsidePoint";
      default:
        return "INVALID VALUE FOR itk::FastMarchingImageFilterEnums::Label";
    }
  }();
}
} // end namespace itk
