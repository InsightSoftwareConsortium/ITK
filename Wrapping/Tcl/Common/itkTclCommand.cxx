/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTclCommand.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkTclCommand.h"

namespace itk
{

TclCommand::TclCommand()
{
  m_Interpreter = 0;
}


///! Set the interpreter in which the command is to be invoked.
void TclCommand::SetInterpreter(Tcl_Interp* interp)
{
  m_Interpreter = interp;
}


///! Get the interpreter in which the command will be invoked.
Tcl_Interp* TclCommand::GetInterpreter() const
{
  return m_Interpreter;
}


///! Set the command to invoke in the interpreter.
void TclCommand::SetCommandString(const char* commandString)
{
  m_CommandString = commandString;
}


///! Get the command that will be invoked in the interpreter.
const char* TclCommand::GetCommandString() const
{
  return m_CommandString.c_str();
}


///! Execute the callback to the Tcl interpreter.
void TclCommand::Execute(Object*, const EventObject &)
{
  this->TclExecute();
}


///! Execute the callback to the Tcl interpreter with a const LightObject
void TclCommand::Execute(const Object*, const EventObject & )
{
  this->TclExecute();
}


/**
 * Invokes the registered command in the Tcl interpreter.  Reports
 * command errors as ITK warnings.
 */
void TclCommand::TclExecute() const
{
  // Make sure an interpreter has been assigned.
  if(!m_Interpreter)
    {
    itkWarningMacro("Error in itk/tcl callback:\n" <<
                    m_CommandString.c_str() << std::endl <<
                    "invoked with no interpreter!");
    return;
    }

  // Try to evaluate the command in the interpreter.
  if(Tcl_GlobalEval(m_Interpreter,
                    const_cast<char*>(m_CommandString.c_str())) == TCL_ERROR)
    {
    const char* errorInfo = Tcl_GetVar(m_Interpreter, "errorInfo", 0);
    if(!errorInfo) { errorInfo = ""; }
    itkWarningMacro("Error returned from itk/tcl callback:\n" <<
                    m_CommandString.c_str() << std::endl << errorInfo <<
                    " at line number " << m_Interpreter->errorLine);
    }
}

} // namespace itk
