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
#include "itkDTITubeSpatialObjectPoint.h"

namespace itk
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField value)
{
  return out << [value] {
    switch (value)
    {
      case DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::FA:
        return "itk::DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::FA";
      case DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::ADC:
        return "itk::DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::ADC";
      case DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::GA:
        return "itk::DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField::GA";
      default:
        return "INVALID VALUE FOR itk::DTITubeSpatialObjectPointEnums::DTITubeSpatialObjectPointField";
    }
  }();
}
} // end namespace itk
