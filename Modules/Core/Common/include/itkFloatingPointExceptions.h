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
#ifndef itkFloatingPointExceptions_h
#define itkFloatingPointExceptions_h

#include "itkMacro.h" // for ITKCommon_EXPORT
#include "itkSingletonMacro.h"

namespace itk
{
/** \class itkFloatingPointExceptions
 *  \brief Allows floating point exceptions to be caught during program execution.
 *
 * Allows floating point exceptions to be caught during program execution.
 * \ingroup ITKCommon
 */

struct ExceptionGlobals;

class ITKCommon_EXPORT FloatingPointExceptions
{
public:
  /** defines what should happen when exceptions occur */
  using ExceptionAction = enum { ABORT, EXIT };

  /** Enable floating point exceptions.
   *
   * If floating point exceptions are not supported on the platform, the program
   * will either abort or exit displaying the error message `FloatingPointExceptions
   * are not supported on this platform.`.
   *
   * Choice between Exit or Abort is based on the value returned by
   * based GetExceptionAction().
   *
   * \sa Disable, SetEnabled, GetEnabled
   */
  static void Enable();

  /** Disable floating point exceptions.
   *
   * \sa Enable, SetEnabled, GetEnabled
   */
  static void Disable();

  /** Return the current state of FP Exceptions */
  static bool GetEnabled();

  /** Set the state to specified value.
   *
   * \sa Enable, Disable, GetEnabled
   */
  static void SetEnabled(bool val);

  /** Control whether exit(255) or abort() is called on an exception */
  static void SetExceptionAction(ExceptionAction a);

  /** Access current ExceptionAction */
  static ExceptionAction GetExceptionAction();

  /** Return if floating point exceptions are supported on this platform */
  static bool HasFloatingPointExceptionsSupport();

private:
  FloatingPointExceptions() = default;
  FloatingPointExceptions(const FloatingPointExceptions &) = delete;
  void operator=(const FloatingPointExceptions &) = delete;

  itkGetGlobalDeclarationMacro(ExceptionGlobals, PimplGlobals);
  /** static member that controls what happens during an exception */
  static ExceptionGlobals * m_PimplGlobals;
};
}

#endif
