/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTclCommand.h
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
  void Execute(LightObject*, unsigned long);
  void Execute(const LightObject*, unsigned long);

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

#endif _itkTclCommand_h
