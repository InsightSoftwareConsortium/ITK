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
#include "itkDICOMOrientation.h"

// Needed for itk::Function::Max3
#include "itkSpatialOrientationAdapter.h"


namespace itk
{


DICOMOrientation::DICOMOrientation(CoordinateEnum primary, CoordinateEnum secondary, CoordinateEnum tertiary)
{
if (SameOrientationAxes(primary, secondary) || SameOrientationAxes(primary, tertiary) ||
SameOrientationAxes(secondary, tertiary))
{
m_Value = OrientationEnum::INVALID;
}
else{
m_Value = static_cast<OrientationEnum>(toOrientation(primary, secondary, tertiary));
}
}

DICOMOrientation::DICOMOrientation(const std::string & str)
  : m_Value(OrientationEnum::INVALID)
{
  const std::map<std::string, typename DICOMOrientation::OrientationEnum> & stringToCode = GetStringToCode();
  auto                                                                      iter = stringToCode.find(str);
  if (iter != stringToCode.end())
  {
    m_Value = iter->second;
  }
}


const std::string &
DICOMOrientation::GetAsString() const
{
  auto iter = GetCodeToString().find(m_Value);
  if (iter != GetCodeToString().end())
  {
    GetCodeToString().find(OrientationEnum::INVALID);
  }
  return iter->second;
}


DICOMOrientation::CoordinateEnum
DICOMOrientation::GetCoordinateTerm(CoordinateMajornessTermsEnum cmt) const
{
  return static_cast<CoordinateEnum>(static_cast<uint32_t>(m_Value) >> static_cast<uint8_t>(cmt) & 0xff);
}

std::map<std::string, typename DICOMOrientation::OrientationEnum>
DICOMOrientation::CreateStringToCode()
{
  std::map<std::string, OrientationEnum>         stringToCode;
  const std::map<OrientationEnum, std::string> & codeToString = GetCodeToString();

  for (const auto & kv : codeToString)
  {
    stringToCode[kv.second] = kv.first;
  }
  return stringToCode;
}


const std::map<typename DICOMOrientation::OrientationEnum, std::string> &
DICOMOrientation::GetCodeToString()
{
  static const std::map<OrientationEnum, std::string> codeToString = CreateCodeToString();
  return codeToString;
}

const std::map<std::string, DICOMOrientation::OrientationEnum> &
DICOMOrientation::GetStringToCode()
{
  static const std::map<std::string, DICOMOrientation::OrientationEnum> stringToCode = CreateStringToCode();
  return stringToCode;
}


std::map<typename DICOMOrientation::OrientationEnum, std::string>
DICOMOrientation::CreateCodeToString()
{
  std::map<OrientationEnum, std::string> orientToString;
  auto                                   helperAddCode = [&orientToString](std::string str, OrientationEnum code) {
    orientToString[code] = std::move(str);
  };

  // Map between axis string labels and SpatialOrientation
  helperAddCode("RIP", OrientationEnum::RIP);
  helperAddCode("LIP", OrientationEnum::LIP);
  helperAddCode("RSP", OrientationEnum::RSP);
  helperAddCode("LSP", OrientationEnum::LSP);
  helperAddCode("RIA", OrientationEnum::RIA);
  helperAddCode("LIA", OrientationEnum::LIA);
  helperAddCode("RSA", OrientationEnum::RSA);
  helperAddCode("LSA", OrientationEnum::LSA);
  helperAddCode("IRP", OrientationEnum::IRP);
  helperAddCode("ILP", OrientationEnum::ILP);
  helperAddCode("SRP", OrientationEnum::SRP);
  helperAddCode("SLP", OrientationEnum::SLP);
  helperAddCode("IRA", OrientationEnum::IRA);
  helperAddCode("ILA", OrientationEnum::ILA);
  helperAddCode("SRA", OrientationEnum::SRA);
  helperAddCode("SLA", OrientationEnum::SLA);
  helperAddCode("RPI", OrientationEnum::RPI);
  helperAddCode("LPI", OrientationEnum::LPI);
  helperAddCode("RAI", OrientationEnum::RAI);
  helperAddCode("LAI", OrientationEnum::LAI);
  helperAddCode("RPS", OrientationEnum::RPS);
  helperAddCode("LPS", OrientationEnum::LPS);
  helperAddCode("RAS", OrientationEnum::RAS);
  helperAddCode("LAS", OrientationEnum::LAS);
  helperAddCode("PRI", OrientationEnum::PRI);
  helperAddCode("PLI", OrientationEnum::PLI);
  helperAddCode("ARI", OrientationEnum::ARI);
  helperAddCode("ALI", OrientationEnum::ALI);
  helperAddCode("PRS", OrientationEnum::PRS);
  helperAddCode("PLS", OrientationEnum::PLS);
  helperAddCode("ARS", OrientationEnum::ARS);
  helperAddCode("ALS", OrientationEnum::ALS);
  helperAddCode("IPR", OrientationEnum::IPR);
  helperAddCode("SPR", OrientationEnum::SPR);
  helperAddCode("IAR", OrientationEnum::IAR);
  helperAddCode("SAR", OrientationEnum::SAR);
  helperAddCode("IPL", OrientationEnum::IPL);
  helperAddCode("SPL", OrientationEnum::SPL);
  helperAddCode("IAL", OrientationEnum::IAL);
  helperAddCode("SAL", OrientationEnum::SAL);
  helperAddCode("PIR", OrientationEnum::PIR);
  helperAddCode("PSR", OrientationEnum::PSR);
  helperAddCode("AIR", OrientationEnum::AIR);
  helperAddCode("ASR", OrientationEnum::ASR);
  helperAddCode("PIL", OrientationEnum::PIL);
  helperAddCode("PSL", OrientationEnum::PSL);
  helperAddCode("AIL", OrientationEnum::AIL);
  helperAddCode("ASL", OrientationEnum::ASL);
  helperAddCode("INVALID", OrientationEnum::INVALID);

  return orientToString;
}


typename DICOMOrientation::OrientationEnum
DICOMOrientation::DirectionCosinesToOrientation(const DirectionType & dir)
{
  // NOTE: This method was based off of itk::SpatialObjectAdaptor::FromDirectionCosines
  // but it is DIFFERENT in the meaning of direction in terms of sign-ness.
  CoordinateEnum terms[3] = { CoordinateEnum::UNKNOWN, CoordinateEnum::UNKNOWN, CoordinateEnum::UNKNOWN };

  for (unsigned i = 0; i < 3; i++)
  {

    const unsigned dominant_axis = Function::Max3(dir[0][i], dir[1][i], dir[2][i]);

    const int dominate_sgn = Math::sgn(dir[dominant_axis][i]);

    switch (dominant_axis)
    {
      case 0: {
        // When the dominate axis sign is positive, assign the coordinate for the direction we are increasing towards.
        // ITK is in LPS, so that is the positive direction
        terms[i] = (dominate_sgn == 1) ? CoordinateEnum::Left : CoordinateEnum::Right;
        break;
      }
      case 1: {
        terms[i] = (dominate_sgn == 1) ? CoordinateEnum::Posterior : CoordinateEnum::Anterior;
        break;
      }
      case 2: {
        terms[i] = (dominate_sgn == 1) ? CoordinateEnum::Superior : CoordinateEnum::Inferior;
        break;
      }
      default:
        itkGenericExceptionMacro("Unexpected Axis")
    }
  }

  return static_cast<OrientationEnum>(toOrientation(terms[0], terms[1], terms[2]));
}


typename DICOMOrientation::DirectionType
DICOMOrientation::OrientationToDirectionCosines(OrientationEnum orientationEnum)
{
  const DICOMOrientation o(orientationEnum);

  CoordinateEnum terms[Dimension] = { o.GetPrimaryTerm(),
                                      o.GetSecondaryTerm(),
                                      o.GetTertiaryTerm() };
  DirectionType direction;
  direction.Fill(0.0);

  for (unsigned int i = 0; i < Dimension; ++i)
  {
    const int sign = (static_cast<uint8_t>(terms[i]) & 0x1) ? 1 : -1;

    switch (terms[i])
    {
      case CoordinateEnum::Left:
      case CoordinateEnum::Right:
        direction[0][i] = 1 * sign;
        break;
      case CoordinateEnum::Anterior:
      case CoordinateEnum::Posterior:
        direction[1][i] = 1 * sign;
        break;
      case CoordinateEnum::Inferior:
      case CoordinateEnum::Superior:
        direction[2][i] = 1 * sign;
        break;
      default:
        break;
    }
  }
  return direction;
}

std::ostream &
operator<<(std::ostream & out, typename DICOMOrientation::OrientationEnum value)
{
  auto iter = DICOMOrientation::GetCodeToString().find(value);
  assert(iter != DICOMOrientation::GetCodeToString().end());
  return (out << iter->second);
}


std::ostream &
operator<<(std::ostream & out, const DICOMOrientation &orientation)
{
  return (out << orientation.GetAsString());
}

} // namespace itk
