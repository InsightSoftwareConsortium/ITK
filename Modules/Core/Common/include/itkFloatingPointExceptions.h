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

namespace itk
{
/** \class itkFloatingPointExceptions
 *  \brief Allows floating point exceptions to be caught during program execution.
 *
 * Allows floating point exceptions to be caught during program execution.
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT FloatingPointExceptions
{
public:
  /** defines what should happen when exceptions occur */
  typedef enum { ABORT, EXIT } ExceptionAction;
  /** Enable floating point exceptions */
  static void Enable();

  /** Disable floating point exceptions. */
  static void Disable();

  /** Return the current state of FP Exceptions */
  static bool GetEnabled();
  /** Set the state to specified value */
  static void SetEnabled(bool val);

  /** Control whether exit(255) or abort() is called on an exception */
  static void SetExceptionAction(ExceptionAction a);

  /** Access current ExceptionAction */
  static ExceptionAction GetExceptionAction();

private:
  FloatingPointExceptions();                                // Not implemented.
  FloatingPointExceptions(const FloatingPointExceptions &); // Not
                                                            // implemented.
  void operator=(const FloatingPointExceptions &);          // Not implemented.

  /** static member that controls what happens during an exception */
  static ExceptionAction m_ExceptionAction;
  static bool            m_Enabled;
};
}

#endif
