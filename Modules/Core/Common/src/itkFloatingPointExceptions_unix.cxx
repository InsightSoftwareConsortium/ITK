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

#include "itkConfigure.h"
#include "itkConfigurePrivate.h"
#include "itkFloatingPointExceptions.h"

#ifdef LINUX
/* BEGIN quote
http://graphviz.sourcearchive.com/documentation/2.16/gvrender__pango_8c-source.html
*/
/* _GNU_SOURCE is needed (supposedly) for the feenableexcept
 * prototype to be defined in fenv.h on GNU systems.
 * Presumably it will do no harm on other systems.
 */
#  ifndef _GNU_SOURCE
#    define _GNU_SOURCE
#  endif

/* We are not supposed to need __USE_GNU, but I can't see
 * how to get the prototype for fedisableexcept from
 * /usr/include/fenv.h without it.
 */
#  ifndef __USE_GNU
#    define __USE_GNU
#  endif
/* END quote */
#endif // LINUX

#include <cfenv>

//-----------------------------------------------------------------------------
// feenableexcept/fedisableexcept implementations
//-----------------------------------------------------------------------------

// Define ITK_FEENABLEEXCEPT_NOOP if feenableexcept and fedisableexcept functions
// should be do nothing (beside of returning zero).
#if defined(__sun) || defined(__EMSCRIPTEN__)
#  define ITK_FEENABLEEXCEPT_NOOP
#endif

// We do not have ITK_HAS_FEENABLEEXCEPT, and we do not have a workaround
// implemented, e.g. ARMv8 with MUSL
#if !defined(ITK_HAS_FEENABLEEXCEPT) && !defined(__ppc__) && !defined(__ppc64__) && !defined(__i386__) && \
  !defined(__x86_64__)
#  define ITK_FEENABLEEXCEPT_NOOP
#endif

#if defined(__APPLE__)
#  include "TargetConditionals.h"
#  if TARGET_OS_IPHONE || TARGET_IPHONE_SIMULATOR || TARGET_CPU_ARM64
#    define ITK_FEENABLEEXCEPT_NOOP
#  endif
#endif

// Define used macros with defaults if not available
#if defined(ITK_FEENABLEEXCEPT_NOOP)
#  if !defined(FE_DIVBYZERO)
#    define FE_DIVBYZERO 4
#  endif
#  if !defined(FE_INVALID)
#    define FE_INVALID 1
#  endif
#endif

// Considering the following macros:
//
// * ITK_FEENABLEEXCEPT_NOOP   : If it applies, defined above
// * ITK_HAS_FEENABLEEXCEPT    : If it applies, defined in itkConfigure.h based on try_compile
// * ITK_HAS_FEGETENV          : idem
// * ITK_HAS_FESETENV          : idem
//
// the logic below will
//
// (1) include different implementations for the feenableexcept and fedisableexcept
//     functions
//
// (2) and will also define the macros:
//
// * ITK_HAS_FPE_CAPABILITY      : (a) If defined, enable catching of the different types
//                                      of floating point exceptions.
//                                 (b) If not defined, display "FloatingPointExceptions are not supported on this
//                                 platform.".
//                                     Then, depending on the value of FloatingPointExceptions::GetExceptionAction(),
//                                     it aborts or exits
//
// * ITK_FPE_USE_SIGNAL_HANDLER  : (a) If defined, install handler that reports
//                                     information about the exception that invoked it.
//                                     Then, depending on the value of FloatingPointExceptions::GetExceptionAction(),
//                                     it aborts or exits
//                                 (b) If not defined
//
// which ultimately define the implementation of FloatingPointExceptions::Enable()
// and FloatingPointExceptions::Disable().

#if defined(ITK_FEENABLEEXCEPT_NOOP)
static int
itk_feenableexcept(unsigned int /*excepts*/)
{
  return 0;
}
static int
itk_fedisableexcept(unsigned int /*excepts*/)
{
  return 0;
}
#  define ITK_HAS_FPE_CAPABILITY

#elif defined(ITK_HAS_FEENABLEEXCEPT)
#  define itk_feenableexcept feenableexcept
#  define itk_fedisableexcept fedisableexcept
#  define ITK_HAS_FPE_CAPABILITY
#  define ITK_FPE_USE_SIGNAL_HANDLER

#elif defined(ITK_HAS_FEGETENV) && defined(ITK_HAS_FESETENV)
#  include "itkFloatingPointExceptions_unix_feenableexcept_using_fegetenv.cxx"
#  define ITK_HAS_FPE_CAPABILITY
#  define ITK_FPE_USE_SIGNAL_HANDLER

#endif

// Implementation of signal handler used below
#if defined(ITK_FPE_USE_SIGNAL_HANDLER)
#  include "itkFloatingPointExceptions_unix_signalhandler.cxx"
#endif

#include <cstring> // memset

namespace itk
{

void
FloatingPointExceptions::Enable()
{
  itkInitGlobalsMacro(PimplGlobals);
#if defined(ITK_HAS_FPE_CAPABILITY)
  itk_feenableexcept(FE_DIVBYZERO);
  itk_feenableexcept(FE_INVALID);
#  if defined(ITK_FPE_USE_SIGNAL_HANDLER)
  struct sigaction act;
  memset(&act, 0, sizeof(struct sigaction));
  act.sa_sigaction = fhdl;
  sigemptyset(&act.sa_mask);
  act.sa_flags = SA_SIGINFO;
  sigaction(SIGFPE, &act, nullptr);
#  endif
  FloatingPointExceptions::m_PimplGlobals->m_Enabled = true;
  (void)itkFloatingPointExceptionsNotSupported; // avoid unused-function warning
#else
  itkFloatingPointExceptionsNotSupported();
#endif
}

void
FloatingPointExceptions::Disable()
{
  itkInitGlobalsMacro(PimplGlobals);
#if defined(ITK_HAS_FPE_CAPABILITY)
  itk_fedisableexcept(FE_ALL_EXCEPT);
  FloatingPointExceptions::m_PimplGlobals->m_Enabled = false;
#else
  itkFloatingPointExceptionsNotSupported();
#endif
}

bool
FloatingPointExceptions::HasFloatingPointExceptionsSupport()
{
  itkInitGlobalsMacro(PimplGlobals);
#if defined(ITK_HAS_FPE_CAPABILITY)
  return true;
#else
  return false;
#endif
}

} // namespace itk
