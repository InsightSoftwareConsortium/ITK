/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapInit.cxx
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
#include "wrapUtils.h"
#include "wrapTypeInfo.h"

#include <set>

namespace _wrap_
{
extern Tcl_ObjType TclPointerType;
extern Tcl_ObjType TclReferenceType;


/**
 * Initialization class to setup the facility for a Tcl interpreter.
 */
class TclWrapperFacility
{
public:
  static int InitializeInterpreter(Tcl_Interp*);
private:
  static std::set<Tcl_Interp*> initializedInterpreters;
};


/**
 * The set of interpreters that have been initialized.
 */
std::set<Tcl_Interp*> TclWrapperFacility::initializedInterpreters;


/**
 * Initialization function for a Tcl interpreter.   This will only execute
 * exactly once for a given interpreter.
 *
 * This just registers the Pointer and Reference Tcl object types needed
 * by the wrappers with the interpreter.
 */
int TclWrapperFacility::InitializeInterpreter(Tcl_Interp* interp)
{
  // Initialize predefined type information.  This only executes once,
  // no matter how many interpreters are using it.
  _wrap_::TypeInfo::Initialize();

  // Only register our object types if the interpreter has not been
  // initialized.
  if(initializedInterpreters.find(interp) != initializedInterpreters.end())
    {
      Tcl_RegisterObjType(&_wrap_::TclPointerType);
      Tcl_RegisterObjType(&_wrap_::TclReferenceType);
    }
  
  initializedInterpreters.insert(interp);

  return TCL_OK;
}


} // namespace _wrap_


// Make sure wrapper initialization function has external C-style linkage.
extern "C" { _wrap_EXPORT int Wrap_Init(Tcl_Interp*); }

/**
 * When the WrapTclFacilitator library is loaded by a Tcl interpreter,
 * this is called to initialize it.  This just passes the call to
 * TclWrapperFacility::InitializeInterpreter.
 */
_wrap_EXPORT int Wrap_Init(Tcl_Interp* interp)
{
  return _wrap_::TclWrapperFacility::InitializeInterpreter(interp);
}
