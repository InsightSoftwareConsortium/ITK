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

#include <mutex>

namespace itk
{

#if defined(_WIN32)

// Windows implementation using thread-specific locale
NumericLocale::NumericLocale()
{
  // Enable thread-specific locale for this thread
  m_PreviousThreadLocaleSetting = _configthreadlocale(_ENABLE_PER_THREAD_LOCALE);

  // Save current LC_NUMERIC locale
  const char * currentLocale = setlocale(LC_NUMERIC, nullptr);
  if (currentLocale)
  {
    m_SavedLocale = _strdup(currentLocale);
  }

  // Set to C locale for parsing
  setlocale(LC_NUMERIC, "C");
}

NumericLocale::~NumericLocale()
{
  // Restore original locale
  if (m_SavedLocale)
  {
    setlocale(LC_NUMERIC, m_SavedLocale);
    free(m_SavedLocale);
  }

  // Restore previous thread-specific locale setting
  if (m_PreviousThreadLocaleSetting != -1)
  {
    _configthreadlocale(m_PreviousThreadLocaleSetting);
  }
}

#elif defined(__APPLE__) || defined(__linux__) || defined(__unix__)

// POSIX implementation using thread-local locale (uselocale/newlocale)
NumericLocale::NumericLocale()
{
  // Create a new C locale
  m_CLocale = newlocale(LC_NUMERIC_MASK, "C", nullptr);

  if (m_CLocale)
  {
    // Set the C locale for this thread and save the previous locale
    m_PreviousLocale = uselocale(m_CLocale);
  }
}

NumericLocale::~NumericLocale()
{
  // Restore the previous locale for this thread
  if (m_PreviousLocale)
  {
    uselocale(m_PreviousLocale);
  }

  // Free the C locale
  if (m_CLocale)
  {
    freelocale(m_CLocale);
  }
}

#else

// Fallback implementation using global locale with mutex protection
// This is not ideal but provides some protection against concurrent access
namespace
{
std::mutex localeMutex;
}

NumericLocale::NumericLocale()
{
  // Lock mutex to protect global locale state
  localeMutex.lock();

  // Save current LC_NUMERIC locale
  const char * currentLocale = setlocale(LC_NUMERIC, nullptr);
  if (currentLocale)
  {
    m_SavedLocale = strdup(currentLocale);
  }

  // Set to C locale for parsing
  setlocale(LC_NUMERIC, "C");
}

NumericLocale::~NumericLocale()
{
  // Restore original locale
  if (m_SavedLocale)
  {
    setlocale(LC_NUMERIC, m_SavedLocale);
    free(m_SavedLocale);
  }

  // Unlock mutex
  localeMutex.unlock();
}

#endif

} // end namespace itk
