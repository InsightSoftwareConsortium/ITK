/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapTclCxxObject.h
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
#ifndef _wrapTclCxxObject_h
#define _wrapTclCxxObject_h

#include "wrapUtils.h"
#include "wrapCxxObject.h"

namespace _wrap_
{

class WrapperFacility;

/**
 * Implement the Tcl object type "CxxObject", which refers to an
 * instance of the CxxObject class.  This will be used to pass Tcl
 * command arguments and results that refer to C++ object.
 */
class _wrap_EXPORT TclCxxObject
{
public:  
  typedef TclCxxObject Self;
  static bool TclObjTypeMatches(Tcl_Obj*);
  static Tcl_Obj* NewObj(CxxObject*);
  static void SetObj(Tcl_Obj*, CxxObject*);
  static int GetFromObj(Tcl_Interp*, Tcl_Obj*, CxxObject**);
protected:
  static void SetTclObj(Tcl_Obj*, CxxObject*);
  static CxxObject* Cast(Tcl_Obj*);
  static void DeleteOldRepresentation(Tcl_Obj*);

  static void FreeInternalRep(Tcl_Obj*);
  static void DupInternalRep(Tcl_Obj*, Tcl_Obj*);
  static void UpdateString(Tcl_Obj*);
  static int SetFromAny(Tcl_Interp*, Tcl_Obj*);

  static Tcl_ObjType TclObjTypeStruct;
  
private:
  static void ClassInitialize();
  friend class WrapperFacility;
};  

// Standard Tcl interface for its object types.
// This one is for the Pointer object.
_wrap_EXPORT int Tcl_GetCxxObjectFromObj(Tcl_Interp*, Tcl_Obj*, CxxObject**);
_wrap_EXPORT void Tcl_SetCxxObjectObj(Tcl_Obj*, CxxObject*);
_wrap_EXPORT Tcl_Obj* Tcl_NewCxxObjectObj(CxxObject*);
  
// A couple useful utility functions for the type.
_wrap_EXPORT bool TclObjectTypeIsCxxObject(Tcl_Obj*);
_wrap_EXPORT bool StringRepIsCxxObject(const String&);
  
} // namespace _wrap_

#endif
