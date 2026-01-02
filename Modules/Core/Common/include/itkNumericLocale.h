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
#ifndef itkNumericLocale_h
#define itkNumericLocale_h

#include "itkMacro.h"
#include "ITKCommonExport.h"

#include <memory>

namespace itk
{

/** \class NumericLocale
 * \brief RAII class for thread-safe temporary setting of LC_NUMERIC locale to "C".
 *
 * This class provides a thread-safe mechanism to temporarily set the LC_NUMERIC
 * locale to "C" for locale-independent parsing and formatting of floating-point
 * numbers. The original locale is automatically restored when the object goes
 * out of scope.
 *
 * This is particularly useful when parsing file formats that use dot as decimal
 * separator (like NRRD, VTK, etc.) regardless of the system locale setting.
 *
 * Thread safety:
 * - On POSIX systems (when newlocale/uselocale are available): Uses thread-local locale
 * - On Windows (when _configthreadlocale is available): Uses thread-specific locale
 * - Fallback (when neither is available): Issues a warning if locale differs from "C",
 *   but does not change the locale. Applications must manage locale externally.
 *
 * Example usage:
 * \code
 * {
 *   NumericLocale cLocale;
 *   // Parse file with dot decimal separator
 *   double value = std::strtod("3.14159", nullptr);
 *   // Locale automatically restored here
 * }
 * \endcode
 *
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT NumericLocale
{
public:
  /** Constructor: Saves current LC_NUMERIC locale and sets it to "C" */
  NumericLocale();

  /** Destructor: Restores the original LC_NUMERIC locale */
  ~NumericLocale();

  // Delete copy and move operations
  NumericLocale(const NumericLocale &) = delete;
  NumericLocale &
  operator=(const NumericLocale &) = delete;
  NumericLocale(NumericLocale &&) = delete;
  NumericLocale &
  operator=(NumericLocale &&) = delete;

private:
  // Forward declaration of implementation structure
  struct Impl;
  // Pointer to implementation (pImpl idiom)
  std::unique_ptr<Impl> m_Impl;
};

} // end namespace itk

#endif // itkNumericLocale_h
