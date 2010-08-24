/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFloatingPointExceptions.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFloatingPointExceptions_h
#define __itkFloatingPointExceptions_h

#include "itkWin32Header.h" // for ITK_EXPORT
namespace itk
{
/** \class itkFloatingPointExceptions
 * Allows floating point exceptions to be caught during program execution.
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
};
}

#endif
