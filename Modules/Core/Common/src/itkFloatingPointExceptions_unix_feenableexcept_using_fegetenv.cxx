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

//
// This file was designed to be directly included from itkFloatingPointExceptions
// cxx files.
//

#include <cfenv>

#include "itkFloatingPointExceptions.h"

#if defined(__ppc__) || defined(__ppc64__) // PPC

// Implementation for PPC

#  define FE_EXCEPT_SHIFT 22 // shift flags right to get masks
#  define FM_ALL_EXCEPT FE_ALL_EXCEPT >> FE_EXCEPT_SHIFT

/* GNU C Library:
https://www.gnu.org/software/libc/manual/html_node/Control-Functions.html

- Function: int fegetexcept (int excepts)

The function returns a bitmask of all currently enabled
exceptions.  It returns static_cast<fexcept_t>(-1) in case of failure.

The excepts argument appears in other functions in fenv.h,
and corresponds to the FE_xxx exception flag constants.  It
is unclear whether the bitmask is for the flags or the masks.
We return that for the flags, which corresponds to the
excepts argument in feenableexcept(excepts) and
fedisableexcept(excepts).  In GNU/Linux the argument is void,
and that's what we implement.  Linux "man fegetenv" appears
to suggest that it's the mask corresponding to bits in
excepts that is returned.
*/

static fexcept_t
itk_feenableexcept(const fexcept_t excepts)
{
  const fexcept_t new_excepts = (excepts & FE_ALL_EXCEPT) >> FE_EXCEPT_SHIFT;

  static fenv_t fenv;
  if (fegetenv(&fenv))
  {
    return static_cast<fexcept_t>(-1);
  }

  // all previous masks
  const fexcept_t old_excepts = (fenv & FM_ALL_EXCEPT) << FE_EXCEPT_SHIFT;

  fenv = (fenv & ~new_excepts) | new_excepts;
  return (fesetenv(&fenv) ? static_cast<fexcept_t>(-1) : old_excepts);
}

static fexcept_t
itk_fedisableexcept(const fexcept_t excepts)
{
  const fexcept_t still_on = ~((excepts & FE_ALL_EXCEPT) >> FE_EXCEPT_SHIFT);

  static fenv_t fenv;
  if (fegetenv(&fenv))
  {
    return static_cast<fexcept_t>(-1);
  }

  // previous masks
  const fexcept_t old_excepts = (fenv & FM_ALL_EXCEPT) << FE_EXCEPT_SHIFT;

  fenv &= still_on;
  return (fesetenv(&fenv) ? static_cast<fexcept_t>(-1) : old_excepts);
}

#elif defined(__i386__) || defined(__x86_64__) // Intel

// Implementation for Intel

// Note: on Apple, ignore the result of the try_compiles that generate these
// ITK_HAS_STRUCT_FENV_T_CONTROL* defines because try_compile doesn't work
// with Universal Binaries where different architectures have different results.
// On Intel macOS, the fenv_t field in question is named __control

#  if defined(__APPLE__) || defined(ITK_HAS_STRUCT_FENV_T_CONTROL)
#    define _itk_control_word __control
#  elif defined(ITK_HAS_STRUCT_FENV_T_CONTROL_WORD)
#    define _itk_control_word __control_word
#  elif defined(ITK_HAS_STRUCT_FENV_T_CONTROL_CW)
#    define _itk_control_word __cw
#  else
#    error "Unknown name for 'fenv_t' struct control member"
#  endif

static fexcept_t
itk_feenableexcept(const fexcept_t excepts)
{
  const fexcept_t new_excepts = excepts & FE_ALL_EXCEPT;

  static fenv_t fenv;
  if (fegetenv(&fenv))
  {
    return static_cast<fexcept_t>(-1);
  }

  // previous masks
  const fexcept_t old_excepts = fenv._itk_control_word & FE_ALL_EXCEPT;

  // unmask
  fenv._itk_control_word &= static_cast<fexcept_t>(~new_excepts);
  fenv.__mxcsr &= ~(new_excepts << 7);

  return (fesetenv(&fenv) ? static_cast<fexcept_t>(-1) : old_excepts);
}

static fexcept_t
itk_fedisableexcept(const fexcept_t excepts)
{
  const fexcept_t new_excepts = excepts & FE_ALL_EXCEPT;

  static fenv_t fenv;
  if (fegetenv(&fenv))
  {
    return static_cast<fexcept_t>(-1);
  }

  // all previous masks
  fexcept_t old_excepts = fenv._itk_control_word & FE_ALL_EXCEPT;

  // mask
  fenv._itk_control_word |= new_excepts;
  fenv.__mxcsr |= new_excepts << 7;

  return (fesetenv(&fenv) ? static_cast<fexcept_t>(-1) : old_excepts);
}

#endif // PPC, INTEL
