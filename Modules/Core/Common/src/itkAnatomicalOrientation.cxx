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
#include "itkAnatomicalOrientation.h"
#ifndef ITK_FUTURE_LEGACY_REMOVE
#  include "itkSpatialOrientationAdapter.h"
#endif

namespace itk
{


#ifndef ITK_FUTURE_LEGACY_REMOVE
AnatomicalOrientation::AnatomicalOrientation(LegacyOrientationType legacyOrientation)
  : AnatomicalOrientation(SpatialOrientationAdapter().ToDirectionCosines(legacyOrientation))
{
  assert(uint32_t(legacyOrientation) == uint32_t(m_Value));
}
#endif


std::string
AnatomicalOrientation::GetAsPositiveStringEncoding() const
{

  // a lambda function to convert a CoordinateEnum to a char
  auto enumAsChar = [](CoordinateEnum coord) -> char {
    switch (coord)
    {
      case CoordinateEnum::RightToLeft:
        return 'L';
      case CoordinateEnum::LeftToRight:
        return 'R';
      case CoordinateEnum::AnteriorToPosterior:
        return 'P';
      case CoordinateEnum::PosteriorToAnterior:
        return 'A';
      case CoordinateEnum::InferiorToSuperior:
        return 'S';
      case CoordinateEnum::SuperiorToInferior:
        return 'I';
      default:
        return 'X';
    }
  };

  if (m_Value == PositiveEnum::INVALID)
  {
    return "INVALID";
  }

  return std::string({ enumAsChar(GetPrimaryTerm()), enumAsChar(GetSecondaryTerm()), enumAsChar(GetTertiaryTerm()) });
}


std::string
AnatomicalOrientation::GetAsNegativeStringEncoding() const
{
  return ConvertStringEncoding(GetAsPositiveStringEncoding());
}


AnatomicalOrientation
AnatomicalOrientation::CreateFromPositiveStringEncoding(std::string str)
{
  std::transform(str.begin(), str.end(), str.begin(), ::toupper);

  const std::map<std::string, typename AnatomicalOrientation::PositiveEnum> & stringToCode = GetStringToCode();
  auto                                                                        iter = stringToCode.find(str);
  if (iter == stringToCode.end())
  {
    return { PositiveEnum::INVALID };
  }
  return { iter->second };
}

AnatomicalOrientation
AnatomicalOrientation::CreateFromNegativeStringEncoding(std::string str)
{
  return AnatomicalOrientation::CreateFromPositiveStringEncoding(ConvertStringEncoding(str));
}


std::string
AnatomicalOrientation::ConvertStringEncoding(std::string str)
{

  auto flip = [](char c) -> char {
    switch (::toupper(c))
    {
      case 'R':
        return 'L';
      case 'L':
        return 'R';
      case 'A':
        return 'P';
      case 'P':
        return 'A';
      case 'S':
        return 'I';
      case 'I':
        return 'S';
      case 'X':
        return 'X';
      default:
        return c;
    }
  };

  for (auto & c : str)
  {
    c = flip(c);
  }
  return str;
}


AnatomicalOrientation::CoordinateEnum
AnatomicalOrientation::GetCoordinateTerm(CoordinateMajornessTermsEnum cmt) const
{
  return static_cast<CoordinateEnum>(static_cast<uint32_t>(m_Value) >> static_cast<uint8_t>(cmt) & 0xff);
}


const std::map<typename AnatomicalOrientation::PositiveEnum, std::string> &
AnatomicalOrientation::GetCodeToString()
{
  auto createCodeToString = []() -> std::map<PositiveEnum, std::string> {
    std::map<PositiveEnum, std::string> orientToString;

    for (auto code : { PositiveEnum::RIP, PositiveEnum::LIP, PositiveEnum::RSP, PositiveEnum::LSP,    PositiveEnum::RIA,
                       PositiveEnum::LIA, PositiveEnum::RSA, PositiveEnum::LSA, PositiveEnum::IRP,    PositiveEnum::ILP,
                       PositiveEnum::SRP, PositiveEnum::SLP, PositiveEnum::IRA, PositiveEnum::ILA,    PositiveEnum::SRA,
                       PositiveEnum::SLA, PositiveEnum::RPI, PositiveEnum::LPI, PositiveEnum::RAI,    PositiveEnum::LAI,
                       PositiveEnum::RPS, PositiveEnum::LPS, PositiveEnum::RAS, PositiveEnum::LAS,    PositiveEnum::PRI,
                       PositiveEnum::PLI, PositiveEnum::ARI, PositiveEnum::ALI, PositiveEnum::PRS,    PositiveEnum::PLS,
                       PositiveEnum::ARS, PositiveEnum::ALS, PositiveEnum::IPR, PositiveEnum::SPR,    PositiveEnum::IAR,
                       PositiveEnum::SAR, PositiveEnum::IPL, PositiveEnum::SPL, PositiveEnum::IAL,    PositiveEnum::SAL,
                       PositiveEnum::PIR, PositiveEnum::PSR, PositiveEnum::AIR, PositiveEnum::ASR,    PositiveEnum::PIL,
                       PositiveEnum::PSL, PositiveEnum::AIL, PositiveEnum::ASL, PositiveEnum::INVALID })
    {
      orientToString[code] = AnatomicalOrientation(code).GetAsPositiveStringEncoding();
    }

    return orientToString;
  };
  static const std::map<PositiveEnum, std::string> codeToString = createCodeToString();
  return codeToString;
}

const std::map<std::string, AnatomicalOrientation::PositiveEnum> &
AnatomicalOrientation::GetStringToCode()
{

  auto createStringToCode = []() -> std::map<std::string, PositiveEnum> {
    std::map<std::string, PositiveEnum>         stringToCode;
    const std::map<PositiveEnum, std::string> & codeToString = GetCodeToString();

    for (const auto & kv : codeToString)
    {
      stringToCode[kv.second] = kv.first;
    }
    return stringToCode;
  };

  static const std::map<std::string, AnatomicalOrientation::PositiveEnum> stringToCode = createStringToCode();
  return stringToCode;
}


AnatomicalOrientation::PositiveEnum
AnatomicalOrientation::ConvertDirectionToPositiveEnum(const DirectionType & dir)
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

  return AnatomicalOrientation(terms[0], terms[1], terms[2]);
}


typename AnatomicalOrientation::DirectionType
AnatomicalOrientation::ConvertPositiveEnumToDirection(PositiveEnum orientationEnum)
{
  const AnatomicalOrientation o(orientationEnum);

  const CoordinateEnum terms[Dimension] = { o.GetPrimaryTerm(), o.GetSecondaryTerm(), o.GetTertiaryTerm() };
  DirectionType        direction{};

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
operator<<(std::ostream & out, typename AnatomicalOrientation::CoordinateEnum value)
{
  switch (value)
  {
    case AnatomicalOrientation::CoordinateEnum::RightToLeft:
      return out << "right-to-left";
    case AnatomicalOrientation::CoordinateEnum::LeftToRight:
      return out << "left-to-right";
    case AnatomicalOrientation::CoordinateEnum::AnteriorToPosterior:
      return out << "anterior-to-posterior";
    case AnatomicalOrientation::CoordinateEnum::PosteriorToAnterior:
      return out << "posterior-to-anterior";
    case AnatomicalOrientation::CoordinateEnum::InferiorToSuperior:
      return out << "inferior-to-superior";
    case AnatomicalOrientation::CoordinateEnum::SuperiorToInferior:
      return out << "superior-to-inferior";
    case AnatomicalOrientation::CoordinateEnum::UNKNOWN:
      return out << "unknown";
    default:
      return out << "invalid";
  }
}

std::ostream &
operator<<(std::ostream & out, typename AnatomicalOrientation::PositiveEnum value)
{
  return (out << AnatomicalOrientation(value).GetAsPositiveStringEncoding());
}

std::ostream &
operator<<(std::ostream & out, typename AnatomicalOrientation::NegativeEnum value)
{
  return (out << AnatomicalOrientation(value).GetAsNegativeStringEncoding());
}

std::ostream &
operator<<(std::ostream & out, const AnatomicalOrientation & orientation)
{
  const auto terms = orientation.GetTerms();
  static_assert(std::tuple_size<decltype(terms)>{} == 3);
  return out << terms[0] << " " << terms[1] << " " << terms[2];
}

} // namespace itk
