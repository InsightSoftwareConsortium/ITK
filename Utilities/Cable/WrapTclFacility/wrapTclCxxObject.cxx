/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapTclCxxObject.cxx
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
#include "wrapTclCxxObject.h"
#include "wrapWrapperFacility.h"

#include <string.h>

namespace _wrap_
{


/**
 * Given a Tcl_Obj, test whether its internal representation type
 * matches that implemented by this class.
 */
bool TclCxxObject::TclObjTypeMatches(Tcl_Obj* objPtr)
{
  return (objPtr->typePtr == &Self::TclObjTypeStruct);
}


/**
 * Tcl object interface:
 * Create a new Tcl object which represents the given object.
 */
Tcl_Obj* TclCxxObject::NewObj(CxxObject* obj)
{
  Tcl_Obj* objPtr = Tcl_NewObj();
  _wrap_DEBUG_OUTPUT(obj->GetWrapperFacility(),
                     "Creating Tcl_Obj at " << objPtr
                     << " for CxxObject at " << obj << std::endl);
  Self::SetTclObj(objPtr, obj);
  return objPtr;
}


/**
 * Tcl object interface:
 * Set a Tcl object to represent a given object.
 */
void TclCxxObject::SetObj(Tcl_Obj* objPtr, CxxObject* obj)
{
  _wrap_DEBUG_OUTPUT(obj->GetWrapperFacility(),
                     "Setting Tcl_Obj at " << objPtr
                     << " to CxxObject at " << obj << std::endl);
  // If this is a shared object, we are not supposed to be setting it.
  if(Tcl_IsShared(objPtr))
    {
    panic("Tcl_SetCxxObjectObj called with shared object");
    }
  Self::DeleteOldRepresentation(objPtr);
  Self::SetTclObj(objPtr, obj);
}


/**
 * Tcl object interface:
 * Get a pointer to CxxObject from the given Tcl object.  Convert
 * the Tcl object's representation if necessary.
 */
int TclCxxObject::GetFromObj(Tcl_Interp* interp, Tcl_Obj* objPtr,
                             CxxObject** obj)
{
  if(!Self::TclObjTypeMatches(objPtr))
    {
    int error;
    if((error = Self::SetFromAny(interp, objPtr)) != TCL_OK) { return error; }
    }
  if(obj) { *obj = Self::Cast(objPtr); }
  return TCL_OK;
}


// Protected implementation members.

/**
 * Sets objPtr's internal representation to point at the given object.
 */
void TclCxxObject::SetTclObj(Tcl_Obj* objPtr, CxxObject* obj)
{
  obj->Increment();
  objPtr->internalRep.otherValuePtr = static_cast<VOID*>(obj);
  objPtr->typePtr = &Self::TclObjTypeStruct;
  Tcl_InvalidateStringRep(objPtr);
}


/**
 * Cast the objPtr's internal representation pointer to a CxxObject
 * pointer.
 */
CxxObject* TclCxxObject::Cast(Tcl_Obj* objPtr)
{
  return static_cast<CxxObject*>(objPtr->internalRep.otherValuePtr);
}


/**
 * Given a Tcl object, delete any existing internal representation.
 */
void TclCxxObject::DeleteOldRepresentation(Tcl_Obj* objPtr)
{
  Tcl_ObjType* oldTypePtr = objPtr->typePtr;
  if((oldTypePtr != NULL) && (oldTypePtr->freeIntRepProc != NULL))
    {
    oldTypePtr->freeIntRepProc(objPtr);
    }
}


/**
 * Tcl object implementation:
 * Free the memory used by the internal representation of the type.
 */
void TclCxxObject::FreeInternalRep(Tcl_Obj* objPtr)
{
  CxxObject* cxxObject = Self::Cast(objPtr);
  _wrap_DEBUG_OUTPUT(cxxObject->GetWrapperFacility(),
                     "Freeing rep for Tcl_Obj at " << objPtr
                     << " (cxxObject=" << cxxObject
                     << ",refCount=" << objPtr->refCount << ")" << std::endl);
  cxxObject->Decrement();
  
  // There is one simple rule that controls when a CxxObject is
  // deleted.  When the last Tcl_Obj still referencing the CxxObject
  // in binary form is destroyed, the CxxObject is destroyed.
  if((cxxObject->GetReferenceCount() <= 0) && (objPtr->refCount <= 0))
    {
    cxxObject->Delete();
    }
}


/**
 * Tcl object implementation:
 * Set the internal representation of destPtr to a copy of that of srcPtr.
 */
void TclCxxObject::DupInternalRep(Tcl_Obj* srcPtr, Tcl_Obj* destPtr)
{
  _wrap_DEBUG_OUTPUT(Self::Cast(srcPtr)->GetWrapperFacility(),
                     "Duplicating rep of Tcl_Obj from " << srcPtr
                     << " to " << destPtr << std::endl);
  Self::SetTclObj(destPtr, Self::Cast(srcPtr));
}


/**
 * Tcl object implementation:
 * Generate the string representation of the Tcl object from the internal
 * representation.
 */
void TclCxxObject::UpdateString(Tcl_Obj *objPtr)
{
  String stringRep = Self::Cast(objPtr)->GetStringRepresentation();
  objPtr->bytes = Tcl_Alloc(stringRep.length()+1);
  objPtr->length = stringRep.length();
  strcpy(objPtr->bytes, stringRep.c_str());
}


/**
 * Tcl object implementation:
 * Attempt to construct the internal representation from the string
 * representation.
 */
int TclCxxObject::SetFromAny(Tcl_Interp* interp, Tcl_Obj* objPtr)
{
  const char* stringRep = Tcl_GetStringFromObj(objPtr, NULL);
  CxxObject* cxxObject = CxxObject::GetFromStringRepresentation(stringRep);
  
  if(!cxxObject)
    {
    if(interp)
      {
      Tcl_ResetResult(interp);
      Tcl_AppendResult(interp,
                       "Expected CxxObject, but got \"",
                       const_cast<char*>(stringRep), "\"", 0);
      }
    return TCL_ERROR;
    }
  else
    {
    _wrap_DEBUG_OUTPUT(cxxObject->GetWrapperFacility(),
                       "Setting rep for Tcl_Obj at " << objPtr
                       << " (refCount=" << objPtr->refCount << ")"
                       << " from \"" << stringRep << "\"" << std::endl);
    }
  
  Self::DeleteOldRepresentation(objPtr);
  Self::SetTclObj(objPtr, cxxObject);
  return TCL_OK;
}


/**
 * This will be called when the program begins.  It registers the Tcl
 * object type implemented by this class.
 */
void TclCxxObject::ClassInitialize()
{
  Tcl_RegisterObjType(&Self::TclObjTypeStruct);
}


/**
 * The Tcl_ObjType structure instance that is registered with Tcl.
 */
Tcl_ObjType TclCxxObject::TclObjTypeStruct =
{
  "CxxObject",            // name
  &Self::FreeInternalRep, // freeIntRepProc
  &Self::DupInternalRep,  // dupIntRepProc
  &Self::UpdateString,    // updateStringProc
  &Self::SetFromAny       // setFromAnyProc
};


/**
 * Tcl object interface:
 * Get a pointer to CxxObject from the given Tcl object.  Convert
 * the Tcl object's representation if necessary.
 */
int Tcl_GetCxxObjectFromObj(Tcl_Interp* interp, Tcl_Obj* objPtr,
                            CxxObject** obj)
{
  return TclCxxObject::GetFromObj(interp, objPtr, obj);
}


/**
 * Tcl object interface:
 * Set a Tcl object to represent a given CxxObject pointer.
 */
void Tcl_SetCxxObjectObj(Tcl_Obj* objPtr, CxxObject* obj)
{
  TclCxxObject::SetObj(objPtr, obj);
}


/**
 * Tcl object interface:
 * Create a new Tcl object which represents the given CxxObject pointer.
 */
Tcl_Obj* Tcl_NewCxxObjectObj(CxxObject* obj)
{
  return TclCxxObject::NewObj(obj);
}


/**
 * Check whether the given object's type is "CxxObject".
 */
bool TclObjectTypeIsCxxObject(Tcl_Obj* o)
{
  return TclCxxObject::TclObjTypeMatches(o);
}


/**
 * Check whether the given string representation is that of a CxxObject.
 */
bool StringRepIsCxxObject(const String& s)
{
  return (((s).substr(0, 4)) == "_cxx");
}
  
} // namespace _wrap_
