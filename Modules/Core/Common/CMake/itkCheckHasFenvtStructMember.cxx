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

#include <fenv.h>

int
main()
{
#if defined(ITK_CHECK_FENV_T_CONTROL)
  static_assert(sizeof(fenv_t().__control) > 0);
#elif defined(ITK_CHECK_FENV_T_CONTROL_WORD)
  static_assert(sizeof(fenv_t().__control_word) > 0);
#elif defined(ITK_CHECK_FENV_T_CW)
  static_assert(sizeof(fenv_t().__cw) > 0);
#else
#  error \
    "Unknown fenv_t struct member test: Make sure to specify a compile definition of the form -DITK_CHECK_FENV_T_xxx"
#endif
  return 0;
}
