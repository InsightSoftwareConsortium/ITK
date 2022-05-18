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
#include "itkBorderQuadEdgeMeshFilter.h"


namespace itk
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const BorderQuadEdgeMeshFilterEnums::BorderTransform value)
{
  return out << [value] {
    switch (value)
    {
      case BorderQuadEdgeMeshFilterEnums::BorderTransform::SQUARE_BORDER_TRANSFORM:
        return "itk::BorderQuadEdgeMeshFilterEnums::BorderTransform::SQUARE_BORDER_TRANSFORM";
      case BorderQuadEdgeMeshFilterEnums::BorderTransform::DISK_BORDER_TRANSFORM:
        return "itk::BorderQuadEdgeMeshFilterEnums::BorderTransform::DISK_BORDER_TRANSFORM";
      default:
        return "INVALID VALUE FOR itk::BorderQuadEdgeMeshFilterEnums::BorderTransform";
    }
  }();
}
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const BorderQuadEdgeMeshFilterEnums::BorderPick value)
{
  return out << [value] {
    switch (value)
    {
      case BorderQuadEdgeMeshFilterEnums::BorderPick::LONGEST:
        return "itk::BorderQuadEdgeMeshFilterEnums::BorderPick::LONGEST";
      case BorderQuadEdgeMeshFilterEnums::BorderPick::LARGEST:
        return "itk::BorderQuadEdgeMeshFilterEnums::BorderPick::LARGEST";
      default:
        return "INVALID VALUE FOR itk::BorderQuadEdgeMeshFilterEnums::BorderPick";
    }
  }();
}

} // end namespace itk
