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

#include "itkFloatingPointExceptions.h"

#include <iostream>

#if defined(__ppc__) || defined(__ppc64__)
#  define DEFINED_PPC 1
#else
#  define DEFINED_PPC 0
#endif

#if defined(__i386__) || defined(__x86_64__)
#  define DEFINED_INTEL 1
#else
#  define DEFINED_INTEL 0
#endif

#if DEFINED_PPC

#  define getfpscr(x) asm volatile("mffs %0" : "=f"(x));
#  define setfpscr(x) asm volatile("mtfsf 255,%0" : : "f"(x));

typedef union
{
  struct
  {
    unsigned long hi;
    unsigned long lo;
  } i;
  double d;
} hexdouble;

#endif // DEFINED_PPC

#if DEFINED_INTEL

// x87 fpu
#  define getx87cr(x) asm("fnstcw %0" : "=m"(x))
#  define setx87cr(x) asm("fldcw %0" : "=m"(x))
#  define getx87sr(x) asm("fnstsw %0" : "=m"(x))

// SIMD, gcc with Intel Core 2 Duo uses SSE2(4)
#  define getmxcsr(x) asm("stmxcsr %0" : "=m"(x))
#  define setmxcsr(x) asm("ldmxcsr %0" : "=m"(x))

#endif // DEFINED_INTEL

#include <csignal>

static const char * fe_code_name[] = { "FPE_NOOP",   "FPE_FLTDIV", "FPE_FLTINV", "FPE_FLTOVF", "FPE_FLTUND",
                                       "FPE_FLTRES", "FPE_FLTSUB", "FPE_INTDIV", "FPE_INTOVF", "FPE_UNKNOWN" };

extern "C"
{
  static void
  fhdl(int sig, siginfo_t * sip, void *)
  {
    std::cout << "FPE Signal Caught" << std::endl;
    std::cout.flush();
    int          fe_code = sip->si_code;
    unsigned int excepts = fetestexcept(FE_ALL_EXCEPT);

    std::stringstream msg;

    switch (fe_code)
    {
#ifdef FPE_NOOP // occurs in OS X
      case FPE_NOOP:
        fe_code = 0;
        break;
#endif
      case FPE_FLTDIV:
        fe_code = 1;
        break; // divideByZero
      case FPE_FLTINV:
        fe_code = 2;
        break; // invalid
      case FPE_FLTOVF:
        fe_code = 3;
        break; // overflow
      case FPE_FLTUND:
        fe_code = 4;
        break; // underflow
      case FPE_FLTRES:
        fe_code = 5;
        break; // inexact
      case FPE_FLTSUB:
        fe_code = 6;
        break; // invalid
      case FPE_INTDIV:
        fe_code = 7;
        break; // overflow
      case FPE_INTOVF:
        fe_code = 8;
        break; // underflow
      default:
        fe_code = 9;
    }

    if (sig == SIGFPE)
    {
#if DEFINED_INTEL
      unsigned short x87cr, x87sr;
      unsigned int   mxcsr;

      getx87cr(x87cr);
      getx87sr(x87sr);
      getmxcsr(mxcsr);
      msg << "X87CR: " << std::hex << x87cr << std::endl
          << "X87SR: " << std::hex << x87sr << std::endl
          << "MXCSR: " << std::hex << mxcsr << std::endl;
#endif

#if DEFINED_PPC
      hexdouble t;

      getfpscr(t.d);
      msg << "FPSCR: " << std::hex << t.i.lo << std::endl;
#endif

      msg << "signal:  SIGFPE with code " << fe_code_name[fe_code] << std::endl
          << "FE_INVALID flag: " << std::hex << (excepts & FE_INVALID) << std::endl
          << "FE_DIVBYZERO flag: " << std::hex << (excepts & FE_DIVBYZERO) << std::endl;
      feclearexcept(FE_DIVBYZERO);
      feclearexcept(FE_INVALID);
    }
    else
    {
      msg << "Signal is not SIGFPE, it's " << sig << std::endl;
    }
    std::cerr << msg.str();
    itkFloatingPointExceptionsAbortOrExit();
    // it would be awesome if this worked but it doesn't
    //  itk::ExceptionObject e(__FILE__,__LINE__);
    //  e.SetDescription(msg.str().c_str());
    //  throw e;
  }
}
