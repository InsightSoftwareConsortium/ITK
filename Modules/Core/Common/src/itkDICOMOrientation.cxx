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
#include "itkDICOMOrientation.h"
#ifndef ITK_FUTURE_LEGACY_REMOVE
#  include "itkSpatialOrientationAdapter.h"
#endif

namespace itk
{


DICOMOrientation::DICOMOrientation(CoordinateEnum primary, CoordinateEnum secondary, CoordinateEnum tertiary)
{
  if (SameOrientationAxes(primary, secondary) || SameOrientationAxes(primary, tertiary) ||
      SameOrientationAxes(secondary, tertiary))
  {
    m_Value = OrientationEnum::INVALID;
  }
  else
  {
    m_Value = static_cast<OrientationEnum>(
      (static_cast<uint32_t>(primary) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::PrimaryMinor)) +
      (static_cast<uint32_t>(secondary) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::SecondaryMinor)) +
      (static_cast<uint32_t>(tertiary) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::TertiaryMinor)));
  }
}

DICOMOrientation::DICOMOrientation(std::string str)
  : m_Value(OrientationEnum::INVALID)
{
  std::transform(str.begin(), str.end(), str.begin(), ::toupper);
  const std::map<std::string, typename DICOMOrientation::OrientationEnum> & stringToCode = GetStringToCode();
  auto                                                                      iter = stringToCode.find(str);
  if (iter != stringToCode.end())
  {
    m_Value = iter->second;
  }
}


#ifndef ITK_FUTURE_LEGACY_REMOVE
DICOMOrientation::DICOMOrientation(LegacyOrientationType legacyOrientation)
  : DICOMOrientation(SpatialOrientationAdapter::ToDirectionCosines(legacyOrientation))
{}
#endif


const std::string &
DICOMOrientation::GetAsString() const
{
  const auto & codeToString = GetCodeToString();
  auto         iter = codeToString.find(m_Value);
  if (iter != codeToString.end())
  {
    return iter->second;
  }
  assert(codeToString.find(OrientationEnum::INVALID) != codeToString.end());
  return codeToString.find(OrientationEnum::INVALID)->second;
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

  std::multimap<double, std::pair<unsigned, unsigned>> value_to_idx;
  for (unsigned int c = 0; c < 3; ++c)
  {
    for (unsigned int r = 0; r < 3; ++r)
    {
      value_to_idx.emplace(std::abs(dir[c][r]), std::make_pair(c, r));
    }
  }

  for (unsigned i = 0; i < 3; ++i)
  {

    auto               max_idx = value_to_idx.rbegin()->second;
    const unsigned int max_c = max_idx.first;
    const unsigned int max_r = max_idx.second;

    const int max_sgn = Math::sgn(dir[max_c][max_r]);

    for (auto it = value_to_idx.begin(); it != value_to_idx.end();)
    {
      if (it->second.first == max_c || it->second.second == max_r)
      {
        value_to_idx.erase(it++);
      }
      else
      {
        ++it;
      }
    }

    switch (max_c)
    {
      case 0:
      {
        // When the dominant axis sign is positive, assign the coordinate for the direction we are increasing towards.
        // ITK is in LPS, so that is the positive direction
        terms[max_r] = (max_sgn == 1) ? CoordinateEnum::RightToLeft : CoordinateEnum::LeftToRight;
        break;
      }
      case 1:
      {
        terms[max_r] = (max_sgn == 1) ? CoordinateEnum::AnteriorToPosterior : CoordinateEnum::PosteriorToAnterior;
        break;
      }
      case 2:
      {
        terms[max_r] = (max_sgn == 1) ? CoordinateEnum::InferiorToSuperior : CoordinateEnum::SuperiorToInferior;
        break;
      }
      default:
        itkGenericExceptionMacro("Unexpected Axis");
    }
  }

  return DICOMOrientation(terms[0], terms[1], terms[2]);
}


typename DICOMOrientation::DirectionType
DICOMOrientation::OrientationToDirectionCosines(OrientationEnum orientationEnum)
{
  const DICOMOrientation o(orientationEnum);

  CoordinateEnum terms[Dimension] = { o.GetPrimaryTerm(), o.GetSecondaryTerm(), o.GetTertiaryTerm() };
  DirectionType  direction;
  direction.Fill(0.0);

  for (unsigned int i = 0; i < Dimension; ++i)
  {
    const int sign = (static_cast<uint8_t>(terms[i]) & 0x1) ? 1 : -1;

    switch (terms[i])
    {
      case CoordinateEnum::LeftToRight:
      case CoordinateEnum::RightToLeft:
        direction[0][i] = -1 * sign;
        break;
      case CoordinateEnum::AnteriorToPosterior:
      case CoordinateEnum::PosteriorToAnterior:
        direction[1][i] = 1 * sign;
        break;
      case CoordinateEnum::InferiorToSuperior:
      case CoordinateEnum::SuperiorToInferior:
        direction[2][i] = -1 * sign;
        break;
      case CoordinateEnum::UNKNOWN:
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
operator<<(std::ostream & out, const DICOMOrientation & orientation)
{
  return (out << orientation.GetAsString());
}

} // namespace itk
