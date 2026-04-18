// SPDX-FileCopyrightText: Copyright NumFOCUS
// SPDX-License-Identifier: Apache-2.0
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

#include <cstddef>
#include <limits>
#include <stdexcept>
#include <string>

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
    return std::stod(str);
  }
  catch (const std::invalid_argument & e)
  {
    ThrowParseFailure(context, str, "double", e.what(), "invalid_argument");
  }
  catch (const std::out_of_range & e)
  {
    ThrowParseFailure(context, str, "double", e.what(), "out_of_range");
  }
}


float
StringToFloat(const std::string & str, const char * context)
{
  try
  {
    return std::stof(str);
  }
  catch (const std::invalid_argument & e)
  {
    ThrowParseFailure(context, str, "float", e.what(), "invalid_argument");
  }
  catch (const std::out_of_range & e)
  {
    ThrowParseFailure(context, str, "float", e.what(), "out_of_range");
  }
}

} // namespace itk
