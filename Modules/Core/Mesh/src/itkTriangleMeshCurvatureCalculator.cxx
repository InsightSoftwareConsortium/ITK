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
#include "itkTriangleMeshCurvatureCalculator.h"

namespace itk
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const TriangleMeshCurvatureCalculatorEnums::Curvatures value)
{
  return out << [value] {
    switch (value)
    {
      case TriangleMeshCurvatureCalculatorEnums::Curvatures::GaussCurvature:
        return "itk::TriangleMeshCurvatureCalculatorEnums::Curvatures::GaussCurvature";
      case TriangleMeshCurvatureCalculatorEnums::Curvatures::MeanCurvature:
        return "itk::TriangleMeshCurvatureCalculatorEnums::Curvatures::MeanCurvature";
      case TriangleMeshCurvatureCalculatorEnums::Curvatures::MinCurvature:
        return "itk::TriangleMeshCurvatureCalculatorEnums::Curvatures::MinCurvature";
      case TriangleMeshCurvatureCalculatorEnums::Curvatures::MaxCurvature:
        return "itk::TriangleMeshCurvatureCalculatorEnums::Curvatures::MaxCurvature";
      default:
        return "INVALID VALUE FOR TriangleMeshCurvatureCalculatorEnums::Curvatures";
    }
  }();
}
} // namespace itk
