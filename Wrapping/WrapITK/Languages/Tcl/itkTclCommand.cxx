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
