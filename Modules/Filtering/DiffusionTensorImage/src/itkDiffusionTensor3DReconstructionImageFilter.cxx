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
#include "itkDiffusionTensor3DReconstructionImageFilter.h"

namespace itk
{
std::ostream &
operator<<(std::ostream & out, const GradientEnumeration value)
{
  const char * s = nullptr;
  switch (value)
  {
    case GradientEnumeration::GradientIsInASingleImage:
      s = "GradientEnumeration::GradientIsInASingleImage";
      break;
    case GradientEnumeration::GradientIsInManyImages:
      s = "GradientEnumeration::GradientIsInManyImages";
      break;
    case GradientEnumeration::Else:
      s = "GradientEnumeration::Else";
      break;
    default:
      s = "INVALID VALUE FOR GradientEnumeration";
  }
  return out << s;
}
} // end namespace itk
