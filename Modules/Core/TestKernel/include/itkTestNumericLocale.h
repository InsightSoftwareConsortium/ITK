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
#ifndef itkTestNumericLocale_h
#define itkTestNumericLocale_h

#include <string>

namespace itk::test
{

/** Decimal-comma locale names in POSIX and Windows spelling; a test loops over
 * these and skips the names its runner does not provide. */
inline constexpr const char * const commaDecimalLocales[] = { "de_DE.UTF-8",       "fr_FR.UTF-8", "nl_NL.UTF-8",
                                                              "it_IT.UTF-8",       "de-DE",       "German_Germany.1252",
                                                              "French_France.1252" };

/** True where the OS ships decimal-comma locales unconditionally, so finding
 * none is a broken runner; a Linux image legitimately may have no `locale-gen`. */
inline bool
CommaDecimalLocaleIsExpected()
{
#if defined(__APPLE__) || defined(_WIN32)
  return true;
#else
  return false;
#endif
}

inline std::string
NoCommaDecimalLocaleMessage()
{
  return "No decimal-comma locale installed, so the locale-independent parse and format paths were "
         "NOT exercised on this runner. Install e.g. de_DE.UTF-8 (Linux: locale-gen) to cover them.";
}

} // namespace itk::test

#endif // itkTestNumericLocale_h
