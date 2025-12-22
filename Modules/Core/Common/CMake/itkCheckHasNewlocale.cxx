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

// Check if newlocale and uselocale are available (POSIX thread-local locale)
#include <locale.h>

#if defined(__APPLE__)
#  include <xlocale.h>
#endif

int
main()
{
  locale_t loc = newlocale(LC_NUMERIC_MASK, "C", nullptr);
  if (loc)
  {
    locale_t prev = uselocale(loc);
    uselocale(prev);
    freelocale(loc);
  }
  return 0;
}
