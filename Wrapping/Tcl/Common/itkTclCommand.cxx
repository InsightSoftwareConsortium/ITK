/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTclCommand.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
