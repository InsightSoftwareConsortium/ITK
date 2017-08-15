/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#include "itkFloatingPointExceptions.h"
#include <iostream>

//
// invariant over all targets -- set a preference for what
// happens when an exception occurs
//
// TODO: Actually implement controllable behavior on Windows.
//
// this is based on work by David N. Williams
// www-personal.umich.edu/~williams
// http://www-personal.umich.edu/~williams/archive/computation/fe-handling-example.c
#if !defined(_WIN32) && defined(ITK_HAVE_FENV_H) && defined(ITK_HAS_FEENABLEEXCEPT)

#include <iostream>
#include <string.h> // memcpy

#ifdef LINUX
/* BEGIN quote
http://graphviz.sourcearchive.com/documentation/2.16/gvrender__pango_8c-source.html
*/
/* _GNU_SOURCE is needed (supposedly) for the feenableexcept
 * prototype to be defined in fenv.h on GNU systems.
 * Presumably it will do no harm on other systems.
*/
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

/* We are not supposed to need __USE_GNU, but I can't see
 * how to get the prototype for fedisableexcept from
 * /usr/include/fenv.h without it.
*/
#ifndef __USE_GNU
#define __USE_GNU
#endif
/* END quote */
#endif // LINUX


#include <stdio.h> // needed on Solaris
#include <fenv.h>

#if defined(__ppc__) || defined(__ppc64__)
#define DEFINED_PPC 1
#else
#define DEFINED_PPC 0
#endif

#if defined(__i386__) || defined(__x86_64__)
#define DEFINED_INTEL 1
#else
#define DEFINED_INTEL 0
#endif

#if defined(__sun) || defined(__EMSCRIPTEN__)

#if defined(__sun)
#include <ieeefp.h>
#endif
/*
 * Based on information suggested in Solaris documentation.
 * See http://download.oracle.com/docs/cd/E19963-01/html/821-1465/fpgetmask-3c.html
 */
static int
feenableexcept (unsigned int /*excepts*/)
{
  // This code is what is suggested in Solaris docs
  // I'm guessing that was a cruel hoax
  //
  // fp_except e = fpgetmask();
  // if((excepts & FE_DIVBYZERO) != 0)
  //   {
  //   e |= FP_X_DZ;
  //   }
  // else if((excepts & FE_INVALID) != 0)
  //   {
  //   e |= FP_X_INV;
  //   }
  // else if((excepts & FPE_FLTOVF) != 0)
  //   {
  //   e |= FP_X_OFL;
  //   }
  // else if((excepts & FPE_FLTUND) != 0)
  //   {
  //   e |= FP_X_UFL;
  //   }
  // fpsetmask(e);
  return 0;
}

static int
fedisableexcept (unsigned int /*excepts*/)
{
  // This code is what is suggested in Solaris docs
  // I'm guessing that was a cruel hoax
  //
  // fp_except e = fpgetmask();
  // if((excepts & FE_DIVBYZERO) != 0)
  //   {
  //   e &= ~FP_X_DZ;
  //   }
  // else if((excepts & FE_INVALID) != 0)
  //   {
  //   e &= ~FP_X_INV;
  //   }
  // else if((excepts & FPE_FLTOVF) != 0)
  //   {
  //   e &= ~FP_X_OFL;
  //   }
  // else if((excepts & FPE_FLTUND) != 0)
  //   {
  //   e &= ~FP_X_UFL;
  //   }
  // fpsetmask(e);
  return 0;
}
#endif

#if defined(__APPLE__)

#if DEFINED_PPC

#define FE_EXCEPT_SHIFT 22  // shift flags right to get masks
#define FM_ALL_EXCEPT    FE_ALL_EXCEPT >> FE_EXCEPT_SHIFT

/* GNU C Library:
http://www.gnu.org/software/libc/manual/html_node/Control-Functions.html

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
feenableexcept (const fexcept_t excepts)
{
  const fexcept_t new_excepts = (excepts & FE_ALL_EXCEPT) >> FE_EXCEPT_SHIFT;

  static fenv_t fenv;
  if ( fegetenv (&fenv) ) return static_cast<fexcept_t>(-1);

  // all previous masks
  const fexcept_t old_excepts = (fenv & FM_ALL_EXCEPT) << FE_EXCEPT_SHIFT;

  fenv = (fenv & ~new_excepts) | new_excepts;
  return ( fesetenv (&fenv) ? static_cast<fexcept_t>(-1) : old_excepts );
}

static fexcept_t
fedisableexcept (const fexcept_t excepts)
{
  const fexcept_t still_on = ~( (excepts & FE_ALL_EXCEPT) >> FE_EXCEPT_SHIFT );

  static fenv_t fenv;
  if ( fegetenv (&fenv) ) return static_cast<fexcept_t>(-1);

  // previous masks
  const fexcept_t old_excepts = (fenv & FM_ALL_EXCEPT) << FE_EXCEPT_SHIFT;

  fenv &= still_on;
  return ( fesetenv (&fenv) ? static_cast<fexcept_t>(-1) : old_excepts );
}

#elif DEFINED_INTEL

static fexcept_t
feenableexcept (const fexcept_t excepts)
{
  const fexcept_t new_excepts = excepts & FE_ALL_EXCEPT;

  static fenv_t fenv;
  if ( fegetenv (&fenv) ) return static_cast<fexcept_t>(-1);

  // previous masks
  const fexcept_t old_excepts = fenv.__control & FE_ALL_EXCEPT;

  // unmask
  fenv.__control &= static_cast<fexcept_t>(~new_excepts);
  fenv.__mxcsr   &= ~(new_excepts << 7);

  return ( fesetenv (&fenv) ? static_cast<fexcept_t>(-1) : old_excepts );
}

static fexcept_t
fedisableexcept (const fexcept_t excepts)
{
  const fexcept_t new_excepts = excepts & FE_ALL_EXCEPT;

  static fenv_t fenv;
  if ( fegetenv (&fenv) ) return static_cast<fexcept_t>(-1);

  // all previous masks
  fexcept_t old_excepts = fenv.__control & FE_ALL_EXCEPT;

  // mask
  fenv.__control |= new_excepts;
  fenv.__mxcsr   |= new_excepts << 7;

  return ( fesetenv (&fenv) ? static_cast<fexcept_t>(-1) : old_excepts );
}

#else
#include "TargetConditionals.h"
#if defined(TARGET_OS_IPHONE) || defined(TARGET_IPHONE_SIMULATOR)
// Added for iOS
int feenableexcept(unsigned int)
{
  return 0;
}

int fedisableexcept(unsigned int)
{
  return 0;
}
#endif // iOS detection

#endif  // PPC, INTEL or iOS enabling


#endif  // not LINUX

#if DEFINED_PPC

#define getfpscr(x)    asm volatile ("mffs %0" : "=f" (x));
#define setfpscr(x)    asm volatile ("mtfsf 255,%0" : : "f" (x));

typedef union {
    struct {
        unsigned long hi;
        unsigned long lo;
    } i;
    double d;
} hexdouble;

#endif  // DEFINED_PPC

#if DEFINED_INTEL

// x87 fpu
#define getx87cr(x)    asm ("fnstcw %0" : "=m" (x));
#define setx87cr(x)    asm ("fldcw %0"  : "=m" (x));
#define getx87sr(x)    asm ("fnstsw %0" : "=m" (x));

// SIMD, gcc with Intel Core 2 Duo uses SSE2(4)
#define getmxcsr(x)    asm ("stmxcsr %0" : "=m" (x));
#define setmxcsr(x)    asm ("ldmxcsr %0" : "=m" (x));

#endif  // DEFINED_INTEL

#include <signal.h>


static const char *fe_code_name[] = {
  "FPE_NOOP",
  "FPE_FLTDIV", "FPE_FLTINV", "FPE_FLTOVF", "FPE_FLTUND",
  "FPE_FLTRES", "FPE_FLTSUB", "FPE_INTDIV", "FPE_INTOVF",
  "FPE_UNKNOWN"
};

/* SAMPLE ALTERNATE FP EXCEPTION HANDLER

The sample handler just reports information about the
exception that invoked it, and aborts.  It makes no attempt
to restore state and return to the application.

More sophisticated handling would have to confront at least
these issues:

* interface to the system context for restoring state
* imprecision of interrupts from hardware for the intel x87
* fpu (but not the SIMD unit, nor the ppc)
* imprecision of interrupts from system software
*/

extern "C"
{
  static void
  fhdl ( int sig, siginfo_t *sip, void * )
  {
    std::cout << "FPE Signal Caught" << std::endl;
    std::cout.flush();
    int fe_code = sip->si_code;
    unsigned int excepts = fetestexcept (FE_ALL_EXCEPT);

    std::stringstream msg;

    switch (fe_code)
      {
#ifdef FPE_NOOP  // occurs in OS X
      case FPE_NOOP:   fe_code = 0; break;
#endif
      case FPE_FLTDIV: fe_code = 1; break; // divideByZero
      case FPE_FLTINV: fe_code = 2; break; // invalid
      case FPE_FLTOVF: fe_code = 3; break; // overflow
      case FPE_FLTUND: fe_code = 4; break; // underflow
      case FPE_FLTRES: fe_code = 5; break; // inexact
      case FPE_FLTSUB: fe_code = 6; break; // invalid
      case FPE_INTDIV: fe_code = 7; break; // overflow
      case FPE_INTOVF: fe_code = 8; break; // underflow
      default: fe_code = 9;
      }

    if ( sig == SIGFPE )
      {
#if DEFINED_INTEL
      unsigned short x87cr,x87sr;
      unsigned int mxcsr;

      getx87cr (x87cr);
      getx87sr (x87sr);
      getmxcsr (mxcsr);
      msg << "X87CR: " << std::hex << x87cr << std::endl
          << "X87SR: " << std::hex << x87sr << std::endl
          << "MXCSR: " << std::hex << mxcsr << std::endl;
#endif

#if DEFINED_PPC
      hexdouble t;

      getfpscr (t.d);
      msg << "FPSCR: " << std::hex << t.i.lo << std::endl;
#endif

      msg << "signal:  SIGFPE with code "
          << fe_code_name[fe_code] << std::endl
          <<   "invalid flag: "
          << std::hex <<  (excepts & FE_INVALID) << std::endl
          << "divByZero flag: "
          << std::hex << (excepts & FE_DIVBYZERO) << std::endl;
      feclearexcept (FE_DIVBYZERO);
      feclearexcept (FE_INVALID);
      feclearexcept (FPE_FLTOVF);
      feclearexcept (FPE_FLTUND);
      feclearexcept (FPE_FLTRES);
      feclearexcept (FPE_FLTSUB);
      feclearexcept (FPE_INTDIV);
      feclearexcept (FPE_INTOVF);
      }
    else
      {
      msg << "Signal is not SIGFPE, it's " << sig << std::endl;
      }
    std::cerr << msg.str();
    if(itk::FloatingPointExceptions::GetExceptionAction() ==
       itk::FloatingPointExceptions::ABORT)
      {
      abort();
      }
    else
      {
      exit(255);
      }
    // it would be awesome if this worked but it doesn't
    //  itk::ExceptionObject e(__FILE__,__LINE__);
    //  e.SetDescription(msg.str().c_str());
    //  throw e;
  }
}
#endif // !defined(_WIN32)

namespace itk
{

FloatingPointExceptions::ExceptionAction
FloatingPointExceptions::m_ExceptionAction =
  FloatingPointExceptions::ABORT;
bool FloatingPointExceptions::m_Enabled(false);

void
FloatingPointExceptions
::SetExceptionAction(ExceptionAction a)
{
  FloatingPointExceptions::m_ExceptionAction = a;
}

FloatingPointExceptions::ExceptionAction
FloatingPointExceptions::GetExceptionAction()
{
  return FloatingPointExceptions::m_ExceptionAction;
}

bool
FloatingPointExceptions::
GetEnabled()
{
  return FloatingPointExceptions::m_Enabled;
}

void
FloatingPointExceptions::
SetEnabled(bool val)
{
  if(val)
    {
    FloatingPointExceptions::Enable();
    }
  else
    {
    FloatingPointExceptions::Disable();
    }
}

#if defined(_WIN32) || !defined(ITK_HAVE_FENV_H) || !defined(ITK_HAS_FEENABLEEXCEPT)

#if defined(_MSC_VER)
#include <float.h>

void FloatingPointExceptions
::Enable()
{
  // enable floating point exceptions on MSVC
  _controlfp(_EM_DENORMAL | _EM_UNDERFLOW | _EM_INEXACT, _MCW_EM);
  FloatingPointExceptions::m_Enabled = true;
}

void FloatingPointExceptions
::Disable()
{
  // disable floating point exceptions on MSVC
  _controlfp(_EM_INVALID | _EM_DENORMAL | _EM_ZERODIVIDE | _EM_OVERFLOW |
             _EM_UNDERFLOW | _EM_INEXACT, _MCW_EM);
  FloatingPointExceptions::m_Enabled = false;
}

#else // defined(_MSC_VER)

// MinGW has troubles include'ing float.h.
void FloatingPointExceptions
::Enable()
{
  std::cerr << "FloatingPointExceptions are not supported on this platform." << std::endl;
  if(itk::FloatingPointExceptions::GetExceptionAction() ==
     itk::FloatingPointExceptions::ABORT)
    {
    abort();
    }
  else
    {
    exit(255);
    }
}

void FloatingPointExceptions
::Disable()
{
  std::cerr << "FloatingPointExceptions are not supported on this platform." << std::endl;
  if(itk::FloatingPointExceptions::GetExceptionAction() ==
     itk::FloatingPointExceptions::ABORT)
    {
    abort();
    }
  else
    {
    exit(255);
    }
}

#endif // defined(_MSC_VER)

#else // defined(_WIN32) || !defined(ITK_HAVE_FENV_H) || !defined(ITK_HAS_FEENABLEEXCEPT)

void
FloatingPointExceptions
::Enable()
{
  feenableexcept (FE_DIVBYZERO);
  feenableexcept (FE_INVALID);
  feenableexcept (FPE_FLTOVF);
  feenableexcept (FPE_FLTUND);
  feenableexcept (FPE_FLTRES);
  feenableexcept (FPE_FLTSUB);
  feenableexcept (FPE_INTDIV);
  feenableexcept (FPE_INTOVF);

  struct sigaction act;
  memset(&act,0,sizeof(struct sigaction));
  act.sa_sigaction = fhdl;
  sigemptyset(&act.sa_mask);
  act.sa_flags = SA_SIGINFO;
  sigaction(SIGFPE,&act,ITK_NULLPTR);
  FloatingPointExceptions::m_Enabled = true;
}
void
FloatingPointExceptions
::Disable()
{
  fedisableexcept (FE_DIVBYZERO);
  fedisableexcept (FE_INVALID);
  fedisableexcept (FPE_FLTOVF);
  fedisableexcept (FPE_FLTUND);
  fedisableexcept (FPE_FLTRES);
  fedisableexcept (FPE_FLTSUB);
  fedisableexcept (FPE_INTDIV);
  fedisableexcept (FPE_INTOVF);
  FloatingPointExceptions::m_Enabled = false;
}

#endif // defined(_WIN32) || !defined(ITK_HAVE_FENV_H) || !defined(ITK_HAS_FEENABLEEXCEPT)

} // end of itk namespace
