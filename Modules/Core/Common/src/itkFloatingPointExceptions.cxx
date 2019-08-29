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
#include "itkSingleton.h"

//
// invariant over all targets -- set a preference for what
// happens when an exception occurs
//
// TODO: Actually implement controllable behavior on Windows.
//
// this is based on work by David N. Williams
// www-personal.umich.edu/~williams
// http://www-personal.umich.edu/~williams/archive/computation/fe-handling-example.c

namespace itk
{

struct ExceptionGlobals
{
  ExceptionGlobals()
    : m_ExceptionAction(FloatingPointExceptions::ABORT)
    , m_Enabled(false){};
  FloatingPointExceptions::ExceptionAction m_ExceptionAction;
  bool                                     m_Enabled;
};

void
FloatingPointExceptions ::SetExceptionAction(ExceptionAction a)
{
  itkInitGlobalsMacro(PimplGlobals);
  FloatingPointExceptions::m_PimplGlobals->m_ExceptionAction = a;
}

FloatingPointExceptions::ExceptionAction
FloatingPointExceptions::GetExceptionAction()
{
  itkInitGlobalsMacro(PimplGlobals);
  return FloatingPointExceptions::m_PimplGlobals->m_ExceptionAction;
}

bool
FloatingPointExceptions::GetEnabled()
{
  itkInitGlobalsMacro(PimplGlobals);
  return FloatingPointExceptions::m_PimplGlobals->m_Enabled;
}

void
FloatingPointExceptions::SetEnabled(bool val)
{
  itkInitGlobalsMacro(PimplGlobals);
  if (val)
  {
    FloatingPointExceptions::Enable();
  }
  else
  {
    FloatingPointExceptions::Disable();
  }
}

itkGetGlobalSimpleMacro(FloatingPointExceptions, ExceptionGlobals, PimplGlobals);

ExceptionGlobals * FloatingPointExceptions::m_PimplGlobals;

} // namespace itk

namespace
{

void
itkFloatingPointExceptionsAbortOrExit()
{
  if (itk::FloatingPointExceptions::GetExceptionAction() == itk::FloatingPointExceptions::ABORT)
  {
    abort();
  }
  else
  {
    exit(255);
  }
}

void
itkFloatingPointExceptionsNotSupported()
{
  std::cerr << "FloatingPointExceptions are not supported on this platform." << std::endl;
  itkFloatingPointExceptionsAbortOrExit();
}

} // namespace

#if defined(_WIN32)
#  include "itkFloatingPointExceptions_win.cxx"
#else
#  include "itkFloatingPointExceptions_unix.cxx"
#endif
