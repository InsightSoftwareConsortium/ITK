/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapReference.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "wrapReference.h"

namespace _wrap_
{

/**
 * Implement a new object type for Tcl.
 * It is just a Reference object.
 */

/**
 * The object type interface functions for Tcl.
 */
static void FreeReferenceInternalRep(Tcl_Obj* objPtr);
static void DupReferenceInternalRep(Tcl_Obj* srcPtr, Tcl_Obj* copyPtr);
static void UpdateStringOfReference(Tcl_Obj *objPtr);
static int SetReferenceFromAny(Tcl_Interp* interp, Tcl_Obj* objPtr);

/**
 * A "Reference" type defined for Tcl scripts.
 * It can store any reference type defined for the "Reference" class.
 */
Tcl_ObjType ReferenceType = {
  "Reference",                        /* name */
  FreeReferenceInternalRep,           /* freeIntRepProc */
  DupReferenceInternalRep,            /* dupIntRepProc */
  UpdateStringOfReference,            /* updateStringProc */
  SetReferenceFromAny                 /* setFromAnyProc */
};


/**
 * Internal use: Easy access to the Reference object stored in the Tcl_Obj..
 */
inline Reference& ToReference(Tcl_Obj* objPtr)
{
  return *(Reference*)objPtr->internalRep.otherValuePtr;
}

/**
 * Internal use: Easy setting of a Tcl_Obj to be a Reference object.
 */
inline void SetReference(Reference* ptr, Tcl_Obj* objPtr)
{
  objPtr->internalRep.otherValuePtr = (VOID*)ptr;
  objPtr->typePtr = &ReferenceType;
  UpdateStringOfReference(objPtr);
}


/**
 * Tcl object implementation:
 * Free the memory used by the internal representation of the Reference type.
 */
static void FreeReferenceInternalRep(Tcl_Obj* objPtr)
{
  delete (&ToReference(objPtr));
}


/**
 * Tcl object implementation:
 * Make a copy of the given Reference object.
 */
static void DupReferenceInternalRep(Tcl_Obj* srcPtr, Tcl_Obj* destPtr)
{
  SetReference(new Reference(ToReference(srcPtr)), destPtr);
}


/**
 * Tcl object implementation:
 * Setup the string representation of the Tcl object ("bytes" member) to be
 * that of the contained Reference object.
 */
static void UpdateStringOfReference(Tcl_Obj *objPtr)
{
  String stringRep = ToReference(objPtr).StringRep();
  objPtr->bytes = Tcl_Alloc(stringRep.length()+1);
  objPtr->length = stringRep.length();
  
  strcpy(objPtr->bytes, stringRep.c_str());
}


/**
 * Tcl object implementation:
 * Attempt to construct a Reference object from any other Tcl object by
 * converting from the string representation.
 */
static int SetReferenceFromAny(Tcl_Interp* interp, Tcl_Obj* objPtr)
{
  const char* stringRep = Tcl_GetStringFromObj(objPtr, NULL);

  /**
   * Make a new Reference object.
   */
  Reference* newReference = new Reference;
  
  /**
   * Try to convert from the string representation.
   * If the conversion fails, report error.
   */
  if(!newReference->SetFromStringRep(stringRep))
    {    
    if(interp != NULL)
      {
      char buf[100];
      sprintf(buf, "Expected Reference, but got \"%.50s\"", stringRep);
      Tcl_ResetResult(interp);
      Tcl_AppendToObj(Tcl_GetObjResult(interp), buf, -1);
      }
    return TCL_ERROR;
    }

  /**
   * Delete the old object representation.
   */
  Tcl_ObjType *oldTypePtr = objPtr->typePtr;
  if((oldTypePtr != NULL) && (oldTypePtr->freeIntRepProc != NULL))
    {
    oldTypePtr->freeIntRepProc(objPtr);
    }
  
  /**
   * Replace the old object representation with the Reference object
   * representation.
   */
  SetReference(newReference, objPtr);
  
  return TCL_OK;
}  


/**
 * Tcl object interface:
 * Get a Reference object from the given Tcl object.  If the internal
 * representation of the Tcl object is not already a Reference object, try
 * to convert it.
 */
int Tcl_GetReferenceFromObj(Tcl_Interp* interp, Tcl_Obj* objPtr, Reference* ptr)
{
  /**
   * If conversion is needed, try it.
   */
  if(objPtr->typePtr != &ReferenceType)
    {
    int error;
    if((error = SetReferenceFromAny(interp, objPtr)) != TCL_OK) { return error; }
    }
  
  /**
   * If there is a destination for the Reference object, put it there.
   */
  if(ptr != NULL)
    {
    *ptr = ToReference(objPtr);
    }
  return TCL_OK;
}


/**
 * Tcl object interface:
 * Set a Tcl object to represent a Reference object.
 */
void Tcl_SetReferenceObj(Tcl_Obj* objPtr, const Reference& ptr)
{
  SetReference(new Reference(ptr), objPtr);
}


/**
 * Tcl object interface:
 * Create a new Tcl object which represents a Reference object.
 */
Tcl_Obj* Tcl_NewReferenceObj(const Reference& ptr)
{
  Tcl_Obj* objPtr = new Tcl_Obj;
  Tcl_SetReferenceObj(objPtr, ptr);
  return objPtr;
}

} // namespace _wrap_

