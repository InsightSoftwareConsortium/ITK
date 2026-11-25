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
#include "itkStringConvert.h"
#include "itkConfigurePrivate.h"

#include <cerrno>
#include <cmath>
#include <cstddef>
#include <cstdlib>
#include <limits>
#include <stdexcept>
#include <string>
#include <type_traits>

// A locale-aware strtod would silently parse "0.5" as 0 under a decimal-comma LC_NUMERIC.
#if !defined(ITK_HAS_NEWLOCALE) && !defined(ITK_HAS_CONFIGTHREADLOCALE)
#  error "ITK requires newlocale() or _configthreadlocale() for locale-independent numeric parsing"
#endif

#include <locale.h>
#if defined(ITK_HAS_NEWLOCALE) && defined(__APPLE__)
#  include <xlocale.h>
#endif

namespace itk
{

namespace
{
// Cap quoted-input length in exception messages so a multi-megabyte
// header value cannot blow up the exception text.
constexpr std::size_t maxQuotedInputLength = 512;

std::string
QuoteForMessage(const std::string & str)
{
  if (str.size() <= maxQuotedInputLength)
  {
    return "'" + str + "'";
  }
  return "'" + str.substr(0, maxQuotedInputLength) + "...' (truncated, " + std::to_string(str.size()) + " chars)";
}

[[noreturn]] void
ThrowParseFailure(const char *        context,
                  const std::string & str,
                  const char *        targetType,
                  const char *        underlyingWhat,
                  const char *        failureKind)
{
  itkGenericExceptionMacro("String-to-" << targetType << " conversion failed (" << failureKind << ") while parsing "
                                        << (context ? context : "<unspecified>") << ": input " << QuoteForMessage(str)
                                        << " (" << underlyingWhat << ')');
}

// Reject inputs whose first non-whitespace, non-`+` character is `-`.
// std::stoull silently wraps a leading minus sign into a large unsigned
// value; we make that an explicit error for the unsigned helpers.
void
RejectLeadingMinus(const char * context, const std::string & str, const char * targetType)
{
  for (const char c : str)
  {
    if (c == '-')
    {
      ThrowParseFailure(context, str, targetType, "leading minus sign", "invalid_argument");
    }
    if (c != ' ' && c != '\t' && c != '\n' && c != '\r' && c != '\f' && c != '\v' && c != '+')
    {
      break;
    }
  }
}

// Parse against an explicit "C" locale so '.' is always the decimal separator.
#if defined(ITK_HAS_NEWLOCALE)
locale_t
GetCLocale()
{
  static const locale_t cLocale = newlocale(LC_NUMERIC_MASK, "C", static_cast<locale_t>(nullptr));
  if (!cLocale)
  {
    throw std::runtime_error("cannot create \"C\" numeric locale");
  }
  return cLocale;
}
#else
_locale_t
GetCLocale()
{
  static const _locale_t cLocale = _create_locale(LC_NUMERIC, "C");
  if (!cLocale)
  {
    throw std::runtime_error("cannot create \"C\" numeric locale");
  }
  return cLocale;
}
#endif

// ERANGE with an infinite result is overflow; ERANGE underflow yields a valid subnormal or zero.
template <typename TValue>
TValue
ParseFloatingPointCLocale(const std::string & str)
{
  static_assert(std::is_same_v<TValue, double> || std::is_same_v<TValue, float>,
                "ParseFloatingPointCLocale supports only double and float");
  const char * begin = str.c_str();
  char *       end = nullptr;
  errno = 0;
  TValue value{};
  if constexpr (std::is_same_v<TValue, float>)
  {
#if defined(ITK_HAS_NEWLOCALE)
    value = strtof_l(begin, &end, GetCLocale());
#else
    value = _strtof_l(begin, &end, GetCLocale());
#endif
  }
  else
  {
#if defined(ITK_HAS_NEWLOCALE)
    value = strtod_l(begin, &end, GetCLocale());
#else
    value = _strtod_l(begin, &end, GetCLocale());
#endif
  }
  if (end == begin)
  {
    throw std::invalid_argument("no conversion");
  }
  if (errno == ERANGE && std::isinf(value))
  {
    throw std::out_of_range("out of range");
  }
  return value;
}
} // namespace


std::int32_t
StringToInt32(const std::string & str, const char * context)
{
  try
  {
    // std::stoll returns long long (>= 64 bits guaranteed); range-check
    // explicitly so the int32_t promise is enforced regardless of how
    // wide `int` happens to be on the host platform.
    const long long parsed = std::stoll(str);
    if (parsed < std::numeric_limits<std::int32_t>::min() || parsed > std::numeric_limits<std::int32_t>::max())
    {
      ThrowParseFailure(context, str, "int32_t", "value does not fit in int32_t", "out_of_range");
    }
    return static_cast<std::int32_t>(parsed);
  }
  catch (const std::invalid_argument & e)
  {
    ThrowParseFailure(context, str, "int32_t", e.what(), "invalid_argument");
  }
  catch (const std::out_of_range & e)
  {
    ThrowParseFailure(context, str, "int32_t", e.what(), "out_of_range");
  }
}


std::int64_t
StringToInt64(const std::string & str, const char * context)
{
  try
  {
    return static_cast<std::int64_t>(std::stoll(str));
  }
  catch (const std::invalid_argument & e)
  {
    ThrowParseFailure(context, str, "int64_t", e.what(), "invalid_argument");
  }
  catch (const std::out_of_range & e)
  {
    ThrowParseFailure(context, str, "int64_t", e.what(), "out_of_range");
  }
}


std::uint32_t
StringToUInt32(const std::string & str, const char * context)
{
  RejectLeadingMinus(context, str, "uint32_t");
  try
  {
    const unsigned long long parsed = std::stoull(str);
    if (parsed > std::numeric_limits<std::uint32_t>::max())
    {
      ThrowParseFailure(context, str, "uint32_t", "value does not fit in uint32_t", "out_of_range");
    }
    return static_cast<std::uint32_t>(parsed);
  }
  catch (const std::invalid_argument & e)
  {
    ThrowParseFailure(context, str, "uint32_t", e.what(), "invalid_argument");
  }
  catch (const std::out_of_range & e)
  {
    ThrowParseFailure(context, str, "uint32_t", e.what(), "out_of_range");
  }
}


std::uint64_t
StringToUInt64(const std::string & str, const char * context)
{
  RejectLeadingMinus(context, str, "uint64_t");
  try
  {
    return static_cast<std::uint64_t>(std::stoull(str));
  }
  catch (const std::invalid_argument & e)
  {
    ThrowParseFailure(context, str, "uint64_t", e.what(), "invalid_argument");
  }
  catch (const std::out_of_range & e)
  {
    ThrowParseFailure(context, str, "uint64_t", e.what(), "out_of_range");
  }
}


double
StringToDouble(const std::string & str, const char * context)
{
  try
  {
    return ParseFloatingPointCLocale<double>(str);
  }
  catch (const std::invalid_argument & e)
  {
    ThrowParseFailure(context, str, "double", e.what(), "invalid_argument");
  }
  catch (const std::out_of_range & e)
  {
    ThrowParseFailure(context, str, "double", e.what(), "out_of_range");
  }
  catch (const std::runtime_error & e)
  {
    ThrowParseFailure(context, str, "double", e.what(), "runtime_error");
  }
}


float
StringToFloat(const std::string & str, const char * context)
{
  try
  {
    return ParseFloatingPointCLocale<float>(str);
  }
  catch (const std::invalid_argument & e)
  {
    ThrowParseFailure(context, str, "float", e.what(), "invalid_argument");
  }
  catch (const std::out_of_range & e)
  {
    ThrowParseFailure(context, str, "float", e.what(), "out_of_range");
  }
  catch (const std::runtime_error & e)
  {
    ThrowParseFailure(context, str, "float", e.what(), "runtime_error");
  }
}

} // namespace itk
