/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "../include/itkNormalQuadEdgeMeshFilter.h"


namespace itk
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const NormalQuadEdgeMeshFilterEnums::Weight value)
{
  return out << [value] {
    switch (value)
    {
      case NormalQuadEdgeMeshFilterEnums::Weight::GOURAUD:
        return "itk::NormalQuadEdgeMeshFilterEnums::Weight::GOURAUD";
      case NormalQuadEdgeMeshFilterEnums::Weight::THURMER:
        return "itk::NormalQuadEdgeMeshFilterEnums::Weight::THURMER";
      case NormalQuadEdgeMeshFilterEnums::Weight::AREA:
        return "itk::NormalQuadEdgeMeshFilterEnums::Weight::AREA";
      default:
        return "INVALID VALUE FOR itk::NormalQuadEdgeMeshFilterEnums::Weight";
    }
  }();
}
} // end namespace itk
