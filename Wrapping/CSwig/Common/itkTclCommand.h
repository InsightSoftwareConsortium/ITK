/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTclCommand.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkTclCommand_h
#define _itkTclCommand_h

#include "itkCommand.h"

#include <tcl.h>

namespace itk
{

/** \Class TclCommand
 *  \brief Command subclass that calls back to a Tcl interpreter.
 *
 * TclCommand can be given a string and a Tcl interpreter.  When it is
 * invoked, it will invoke the string in the Tcl interpreter as a
 * command.  This can be used to create arbitrary Tcl event callbacks
 * in ITK Tcl scripts.
 */

class TclCommand : public Command
{
public:
  ///! Standard "Self" typedef.
  typedef TclCommand         Self;

  ///! Smart pointer typedef support.
  typedef SmartPointer<Self>  Pointer;

  ///! Run-time type information (and related methods).
  itkTypeMacro(TclCommand,Command);

  ///! Method for creation through the object factory.
  itkNewMacro(Self);

  void SetInterpreter(Tcl_Interp*);
  Tcl_Interp* GetInterpreter() const;
  void SetCommandString(const char*);
  const char* GetCommandString() const;
  void Execute(Object*, const EventObject & );
  void Execute(const Object*, const EventObject & );

protected:
  TclCommand();
  ~TclCommand() {}
  TclCommand(const Self&);     // Not implemented.
  void operator=(const Self&); // Not implemented.

  void TclExecute() const;
  
private:
  ///! The Tcl interpreter in which the command will be invoked.
  Tcl_Interp* m_Interpreter;
  
  ///! The command to invoke in the Tcl interpreter.
  std::string m_CommandString;
};


} // namespace itk

#endif // _itkTclCommand_h

