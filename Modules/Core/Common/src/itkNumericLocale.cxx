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

#include "itkNumericLocale.h"
#include "itkConfigurePrivate.h"

#include <locale.h>
#include <cstring>
#include <cstdlib>

// Include platform-specific headers based on detected features
#ifdef ITK_HAS_NEWLOCALE
#  if defined(__APPLE__)
#    include <xlocale.h>
#  endif
#endif

namespace itk
{

// Implementation structure definition
struct NumericLocale::Impl
{
#ifdef ITK_HAS_CONFIGTHREADLOCALE
  // Windows: thread-specific locale
  int    m_PreviousThreadLocaleSetting{ -1 };
  char * m_SavedLocale{ nullptr };
#elif defined(ITK_HAS_NEWLOCALE)
  // POSIX: thread-local locale
  locale_t m_PreviousLocale{ nullptr };
  locale_t m_CLocale{ nullptr };
#else
  // Fallback: no locale change, only check and warn
  bool m_WarningIssued{ false };
#endif
};

#ifdef ITK_HAS_CONFIGTHREADLOCALE

// Windows implementation using thread-specific locale
NumericLocale::NumericLocale()
  : m_Impl(new Impl())
{
  // Enable thread-specific locale for this thread
  m_Impl->m_PreviousThreadLocaleSetting = _configthreadlocale(_ENABLE_PER_THREAD_LOCALE);

  // Save current LC_NUMERIC locale
  const char * currentLocale = setlocale(LC_NUMERIC, nullptr);
  if (currentLocale)
  {
    m_Impl->m_SavedLocale = _strdup(currentLocale);
    // If _strdup fails (returns nullptr), m_SavedLocale remains nullptr
    // and the locale will not be restored in the destructor
  }

  // Set to C locale for parsing
  setlocale(LC_NUMERIC, "C");
}

NumericLocale::~NumericLocale()
{
  // Restore original locale
  if (m_Impl->m_SavedLocale)
  {
    setlocale(LC_NUMERIC, m_Impl->m_SavedLocale);
    free(m_Impl->m_SavedLocale);
  }

  // Restore previous thread-specific locale setting
  if (m_Impl->m_PreviousThreadLocaleSetting != -1)
  {
    _configthreadlocale(m_Impl->m_PreviousThreadLocaleSetting);
  }
}

#elif defined(ITK_HAS_NEWLOCALE)

// POSIX implementation using thread-local locale (uselocale/newlocale)
NumericLocale::NumericLocale()
  : m_Impl(new Impl())
{
  // Create a new C locale
  // If newlocale fails (returns nullptr), m_CLocale remains nullptr
  // and the locale will not be changed - this is a safe fallback
  m_Impl->m_CLocale = newlocale(LC_NUMERIC_MASK, "C", nullptr);

  if (m_Impl->m_CLocale)
  {
    // Set the C locale for this thread and save the previous locale
    // uselocale returns the previous locale, which may be LC_GLOBAL_LOCALE
    m_Impl->m_PreviousLocale = uselocale(m_Impl->m_CLocale);
  }
}

NumericLocale::~NumericLocale()
{
  // If we created and installed a C locale for this thread,
  // restore the previous locale and free the C locale.
  // Always restore if m_CLocale is set, as m_PreviousLocale may be
  // LC_GLOBAL_LOCALE which is a valid non-null value.
  if (m_Impl->m_CLocale)
  {
    uselocale(m_Impl->m_PreviousLocale);
    freelocale(m_Impl->m_CLocale);
  }
}

#else

// Fallback implementation - only check locale and issue warning if not "C"
// Do not modify the locale, application must manage it externally
NumericLocale::NumericLocale()
  : m_Impl(new Impl())
{
  // Check if current locale is compatible with expected "C" locale
  const char * currentLocale = setlocale(LC_NUMERIC, nullptr);
  if (currentLocale && std::strcmp(currentLocale, "C") != 0)
  {
    // Issue warning only once per instance
    itkWarningMacro("LC_NUMERIC locale is '" << currentLocale << "' (not 'C'). "
                                             << "Thread-safe locale functions not available. "
                                             << "Locale-dependent number parsing may cause issues. "
                                             << "Please manage locale at application level.");
    m_Impl->m_WarningIssued = true;
  }
}

NumericLocale::~NumericLocale()
{
  // No action needed in fallback - we don't change the locale
}

#endif

} // end namespace itk
