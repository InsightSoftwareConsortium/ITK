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
#ifndef itkStringConvert_h
#define itkStringConvert_h

#include "itkMacro.h"
#include <cstdint>
#include <string>

namespace itk
{

/** \brief Convert a string to a numeric value, wrapping low-level
 *  conversion exceptions as itk::ExceptionObject.
 *
 * The C++ Standard `std::stoi`, `std::stol`, `std::stod`, etc. throw
 * `std::invalid_argument` when the input cannot be parsed and
 * `std::out_of_range` when the parsed value does not fit. Those
 * exceptions are not derived from `itk::ExceptionObject`, so they
 * propagate past ITK call sites that catch only `itk::ExceptionObject`
 * and crash applications that rely on ITK's exception contract
 * (Slicer being a notable example, see ITK issue #3213 and Slicer PR
 * https://github.com/Slicer/Slicer/pull/6200).
 *
 * The functions in this header parse the same inputs as the
 * corresponding `std::sto*` calls, but rethrow conversion failures as
 * `itk::ExceptionObject` whose message includes a caller-supplied
 * `context` describing what was being parsed (e.g.
 * ``"NRRD header field 'sizes'"``) plus the offending input string.
 *
 * **Fixed-width integer return types** are used (`int32_t`, `int64_t`,
 * `uint32_t`, `uint64_t`) rather than the platform-dependent C++
 * spellings (`int`, `long`, `unsigned long`). The C++ Standard only
 * guarantees minimum widths for the named types; in particular `long`
 * and `unsigned long` are 32 bits on 64-bit Windows (LLP64 model) and
 * 64 bits on Linux/macOS (LP64). Returning fixed-width types makes the
 * helpers behave identically across all ITK-supported platforms.
 *
 * Callers that legitimately tolerate empty / malformed input (for
 * example, treating an empty string as zero, the way `std::atoi` did)
 * should test for that case before calling these helpers; the helpers
 * always throw on malformed input.
 *
 * \ingroup ITKCommon
 */

/** Parse `str` as a decimal `int32_t`. Throws itk::ExceptionObject on
 *  failure or if the parsed value does not fit in `int32_t`,
 *  mentioning `context` and the offending input. */
ITKCommon_EXPORT std::int32_t
                 StringToInt32(const std::string & str, const char * context);

/** Parse `str` as a decimal `int64_t`. Throws itk::ExceptionObject on
 *  failure, mentioning `context` and the offending input. */
ITKCommon_EXPORT std::int64_t
                 StringToInt64(const std::string & str, const char * context);

/** Parse `str` as a decimal `uint32_t`. Throws itk::ExceptionObject on
 *  failure or if the parsed value does not fit in `uint32_t`,
 *  mentioning `context` and the offending input.
 *  An explicit leading minus sign is rejected (rather than silently
 *  wrapping the way `std::stoull` does). */
ITKCommon_EXPORT std::uint32_t
                 StringToUInt32(const std::string & str, const char * context);

/** Parse `str` as a decimal `uint64_t`. Throws itk::ExceptionObject on
 *  failure, mentioning `context` and the offending input.
 *  An explicit leading minus sign is rejected (rather than silently
 *  wrapping the way `std::stoull` does). */
ITKCommon_EXPORT std::uint64_t
                 StringToUInt64(const std::string & str, const char * context);

/** Parse `str` as a `double`. Throws itk::ExceptionObject on failure,
 *  mentioning `context` and the offending input. */
ITKCommon_EXPORT double
StringToDouble(const std::string & str, const char * context);

/** Parse `str` as a `float`. Throws itk::ExceptionObject on failure,
 *  mentioning `context` and the offending input. */
ITKCommon_EXPORT float
StringToFloat(const std::string & str, const char * context);

} // namespace itk

#endif
