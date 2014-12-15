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

#ifndef itkTclCommand_h
#define itkTclCommand_h

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
  TclCommand & operator=(const Self&); // Not implemented.

  void TclExecute() const;

private:
  ///! The Tcl interpreter in which the command will be invoked.
  Tcl_Interp* m_Interpreter;

  ///! The command to invoke in the Tcl interpreter.
  std::string m_CommandString;
};


} // namespace itk

#endif // _itkTclCommand_h
