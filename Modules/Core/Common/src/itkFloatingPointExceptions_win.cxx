/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
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

#include "itkFloatingPointExceptions.h"

namespace itk
{

#if defined(_MSC_VER)
#  include <cfloat>

void
FloatingPointExceptions ::Enable()
{
  itkInitGlobalsMacro(PimplGlobals);
  // enable floating point exceptions on MSVC
  _controlfp(_EM_DENORMAL | _EM_UNDERFLOW | _EM_INEXACT, _MCW_EM);
  FloatingPointExceptions::m_PimplGlobals->m_Enabled = true;
}

void
FloatingPointExceptions ::Disable()
{
  itkInitGlobalsMacro(PimplGlobals);
  // disable floating point exceptions on MSVC
  _controlfp(_EM_INVALID | _EM_DENORMAL | _EM_ZERODIVIDE | _EM_OVERFLOW | _EM_UNDERFLOW | _EM_INEXACT, _MCW_EM);
  FloatingPointExceptions::m_PimplGlobals->m_Enabled = false;
}

bool
FloatingPointExceptions ::HasFloatingPointExceptionsSupport()
{
  itkInitGlobalsMacro(PimplGlobals);
  return true;
}

#else // defined(_MSC_VER)

// MinGW has troubles include'ing float.h.

void
FloatingPointExceptions ::Enable()
{
  itkInitGlobalsMacro(PimplGlobals);
  itkFloatingPointExceptionsNotSupported();
}

void
FloatingPointExceptions ::Disable()
{
  itkInitGlobalsMacro(PimplGlobals);
  itkFloatingPointExceptionsNotSupported();
}

bool
FloatingPointExceptions ::HasFloatingPointExceptionsSupport()
{
  itkInitGlobalsMacro(PimplGlobals);
  return false;
}

#endif // defined(_MSC_VER)

} // namespace itk
