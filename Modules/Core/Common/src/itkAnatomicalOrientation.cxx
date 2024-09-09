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


AnatomicalOrientation::AnatomicalOrientation(CoordinateEnum primary, CoordinateEnum secondary, CoordinateEnum tertiary)
{
  if (SameOrientationAxes(primary, secondary) || SameOrientationAxes(primary, tertiary) ||
      SameOrientationAxes(secondary, tertiary))
  {
    m_Value = ToEnum::INVALID;
  }
  else
  {
    m_Value = static_cast<ToEnum>(
      (static_cast<uint32_t>(primary) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::PrimaryMinor)) +
      (static_cast<uint32_t>(secondary) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::SecondaryMinor)) +
      (static_cast<uint32_t>(tertiary) << static_cast<uint8_t>(CoordinateMajornessTermsEnum::TertiaryMinor)));
  }
}

#ifndef ITK_FUTURE_LEGACY_REMOVE
AnatomicalOrientation::AnatomicalOrientation(LegacyOrientationType legacyOrientation)
  : AnatomicalOrientation(SpatialOrientationAdapter().ToDirectionCosines(legacyOrientation))
{
  assert(uint32_t(legacyOrientation) == uint32_t(m_Value));
}
#endif

AnatomicalOrientation::AnatomicalOrientation(AnatomicalOrientation::FromEnum fromOrientation)
  : m_Value(ToEnum(uint32_t(fromOrientation)))
{}


std::string
AnatomicalOrientation::GetAsToStringEncoding() const
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

  if (m_Value == ToEnum::INVALID)
  {
    return "INVALID";
  }

  return std::string({ enumAsChar(GetPrimaryTerm()), enumAsChar(GetSecondaryTerm()), enumAsChar(GetTertiaryTerm()) });
}


std::string
AnatomicalOrientation::GetAsFromStringEncoding() const
{
  return ConvertStringEncoding(GetAsToStringEncoding());
}


AnatomicalOrientation
AnatomicalOrientation::CreateFromToStringEncoding(std::string str)
{
  std::transform(str.begin(), str.end(), str.begin(), ::toupper);

  const std::map<std::string, typename AnatomicalOrientation::ToEnum> & stringToCode = GetStringToCode();
  auto                                                                  iter = stringToCode.find(str);
  if (iter == stringToCode.end())
  {
    return AnatomicalOrientation(ToEnum::INVALID);
  }
  return AnatomicalOrientation(iter->second);
}

AnatomicalOrientation
AnatomicalOrientation::CreateFromFromStringEncoding(std::string str)
{
  return AnatomicalOrientation::CreateFromToStringEncoding(ConvertStringEncoding(str));
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


const std::map<typename AnatomicalOrientation::ToEnum, std::string> &
AnatomicalOrientation::GetCodeToString()
{
  auto createCodeToString = []() -> std::map<ToEnum, std::string> {
    std::map<ToEnum, std::string> orientToString;

    for (auto code : { ToEnum::RIP, ToEnum::LIP, ToEnum::RSP, ToEnum::LSP, ToEnum::RIA, ToEnum::LIA, ToEnum::RSA,
                       ToEnum::LSA, ToEnum::IRP, ToEnum::ILP, ToEnum::SRP, ToEnum::SLP, ToEnum::IRA, ToEnum::ILA,
                       ToEnum::SRA, ToEnum::SLA, ToEnum::RPI, ToEnum::LPI, ToEnum::RAI, ToEnum::LAI, ToEnum::RPS,
                       ToEnum::LPS, ToEnum::RAS, ToEnum::LAS, ToEnum::PRI, ToEnum::PLI, ToEnum::ARI, ToEnum::ALI,
                       ToEnum::PRS, ToEnum::PLS, ToEnum::ARS, ToEnum::ALS, ToEnum::IPR, ToEnum::SPR, ToEnum::IAR,
                       ToEnum::SAR, ToEnum::IPL, ToEnum::SPL, ToEnum::IAL, ToEnum::SAL, ToEnum::PIR, ToEnum::PSR,
                       ToEnum::AIR, ToEnum::ASR, ToEnum::PIL, ToEnum::PSL, ToEnum::AIL, ToEnum::ASL, ToEnum::INVALID })
    {
      orientToString[code] = AnatomicalOrientation(code).GetAsToStringEncoding();
    }

    return orientToString;
  };
  static const std::map<ToEnum, std::string> codeToString = createCodeToString();
  return codeToString;
}

const std::map<std::string, AnatomicalOrientation::ToEnum> &
AnatomicalOrientation::GetStringToCode()
{

  auto createStringToCode = []() -> std::map<std::string, ToEnum> {
    std::map<std::string, ToEnum>         stringToCode;
    const std::map<ToEnum, std::string> & codeToString = GetCodeToString();

    for (const auto & kv : codeToString)
    {
      stringToCode[kv.second] = kv.first;
    }
    return stringToCode;
  };

  static const std::map<std::string, AnatomicalOrientation::ToEnum> stringToCode = createStringToCode();
  return stringToCode;
}


typename AnatomicalOrientation::ToEnum
AnatomicalOrientation::DirectionCosinesToOrientation(const DirectionType & dir)
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
AnatomicalOrientation::OrientationToDirectionCosines(ToEnum orientationEnum)
{
  const AnatomicalOrientation o(orientationEnum);

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
operator<<(std::ostream & out, typename AnatomicalOrientation::ToEnum value)
{
  return (out << AnatomicalOrientation(value).GetAsToStringEncoding());
}

std::ostream &
operator<<(std::ostream & out, typename AnatomicalOrientation::FromEnum value)
{
  return (out << AnatomicalOrientation(value).GetAsFromStringEncoding());
}

std::ostream &
operator<<(std::ostream & out, const AnatomicalOrientation & orientation)
{
  const auto terms = orientation.GetTerms();
  static_assert(std::tuple_size<decltype(terms)>{} == 3);
  return out << terms[0] << " " << terms[1] << " " << terms[2];
}

} // namespace itk
