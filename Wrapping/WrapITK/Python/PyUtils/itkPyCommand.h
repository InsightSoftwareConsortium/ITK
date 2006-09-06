/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPyCommand.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPyCommand_h
#define _itkPyCommand_h

#include "itkCommand.h"

// The python header defines _POSIX_C_SOURCE without a preceding #undef
#undef _POSIX_C_SOURCE
#include <Python.h>

namespace itk
{

/** \Class PyCommand
 *  \brief Command subclass that calls a Python callable object, e.g.
 *  a Python function.
 * 
 * With this class, arbitrary Python callable objects (e.g. functions)
 * can be associated with an instance to be used in AddObserver calls.
 * This is analogous to itk::TclCommand, but then a tad more flexible. ;)
 *
 * This class was contributed by Charl P. Botha <cpbotha |AT| ieee.org>
 */
class PyCommand : public Command
{
public:
  ///! Standard "Self" typedef.
  typedef PyCommand         Self;

  ///! Smart pointer typedef support.
  typedef SmartPointer<Self>  Pointer;

  ///! Run-time type information (and related methods).
  itkTypeMacro(PyCommand,Command);

  ///! Method for creation through the object factory.
  itkNewMacro(Self);

  /** 
   * Assign a Python callable object to be used.  You don't have to keep
   * a binding to the callable, PyCommand will also take out a reference
   * to make sure the Callable sticks around.
   */
  void SetCommandCallable(PyObject *obj);

  void Execute(Object *, const EventObject&);
  void Execute(const Object *, const EventObject&);

protected:
  PyCommand();
  ~PyCommand();
  void PyExecute();
  PyCommand(const Self&);     // Not implemented.
  void operator=(const Self&); // Not implemented.

private:
  PyObject *obj;
};


} // namespace itk

#endif // _itkPyCommand_h

