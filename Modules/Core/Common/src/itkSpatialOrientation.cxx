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

#include "itkSpatialOrientation.h"


namespace itk
{
std::ostream &
operator<<(std::ostream & out, const SpatialOrientationEnums::CoordinateTerms value)
{
  return out << [value] {
    switch (value)
    {
      case SpatialOrientationEnums::CoordinateTerms::ITK_COORDINATE_UNKNOWN:
        return "itk::SpatialOrientationEnums::CoordinateTerms::ITK_COORDINATE_UNKNOWN";
      case SpatialOrientationEnums::CoordinateTerms::ITK_COORDINATE_Right:
        return "itk::SpatialOrientationEnums::CoordinateTerms::ITK_COORDINATE_Right";
      case SpatialOrientationEnums::CoordinateTerms::ITK_COORDINATE_Left:
        return "itk::SpatialOrientationEnums::CoordinateTerms::ITK_COORDINATE_Left";
      case SpatialOrientationEnums::CoordinateTerms::ITK_COORDINATE_Posterior:
        return "itk::SpatialOrientationEnums::CoordinateTerms::ITK_COORDINATE_Posterior";
      case SpatialOrientationEnums::CoordinateTerms::ITK_COORDINATE_Anterior:
        return "itk::SpatialOrientationEnums::CoordinateTerms::ITK_COORDINATE_Anterior";
      case SpatialOrientationEnums::CoordinateTerms::ITK_COORDINATE_Inferior:
        return "itk::SpatialOrientationEnums::CoordinateTerms::ITK_COORDINATE_Inferior";
      case SpatialOrientationEnums::CoordinateTerms::ITK_COORDINATE_Superior:
        return "itk::SpatialOrientationEnums::CoordinateTerms::ITK_COORDINATE_Superior";
      default:
        return "INVALID VALUE FOR itk::SpatialOrientationEnums::CoordinateTerms";
    }
  }();
}

std::ostream &
operator<<(std::ostream & out, const SpatialOrientationEnums::CoordinateMajornessTerms value)
{
  return out << [value] {
    switch (value)
    {
      case SpatialOrientationEnums::CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor:
        return "itk::SpatialOrientationEnums::CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor";
      case SpatialOrientationEnums::CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor:
        return "itk::SpatialOrientationEnums::CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor";
      case SpatialOrientationEnums::CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor:
        return "itk::SpatialOrientationEnums::CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor";
      default:
        return "INVALID VALUE FOR itk::SpatialOrientationEnums::CoordinateMajornessTerms";
    }
  }();
}

std::ostream &
operator<<(std::ostream & out, const SpatialOrientationEnums::ValidCoordinateOrientations value)
{
  return out << [value] {
    switch (value)
    {
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_INVALID:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_INVALID";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RIP:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RIP";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LIP:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LIP";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RSP:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RSP";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LSP:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LSP";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RIA:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RIA";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LIA:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LIA";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RSA:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RSA";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LSA:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LSA";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IRP:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IRP";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ILP:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ILP";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SRP:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SRP";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SLP:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SLP";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IRA:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IRA";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ILA:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ILA";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SRA:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SRA";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SLA:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SLA";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RPI:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RPI";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LPI:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LPI";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RAI:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RAI";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LAI:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LAI";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RPS:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RPS";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LPS:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LPS";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RAS:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RAS";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LAS:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LAS";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PRI:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PRI";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PLI:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PLI";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ARI:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ARI";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ALI:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ALI";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PRS:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PRS";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PLS:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PLS";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ARS:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ARS";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ALS:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ALS";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IPR:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IPR";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SPR:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SPR";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IAR:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IAR";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SAR:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SAR";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IPL:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IPL";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SPL:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SPL";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IAL:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IAL";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SAL:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SAL";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PIR:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PIR";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PSR:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PSR";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_AIR:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_AIR";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ASR:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ASR";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PIL:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PIL";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PSL:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PSL";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_AIL:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_AIL";
      case SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ASL:
        return "itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ASL";
      default:
        return "INVALID VALUE FOR itk::SpatialOrientationEnums::ValidCoordinateOrientations";
    }
  }();
}
} // namespace itk
