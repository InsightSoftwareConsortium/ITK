/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapPointer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "wrapPointer.h"

#include <stdio.h>
#include <string.h>

namespace _wrap_
{

/**
 * Convert the pointer to a string representation.
 */
String Pointer::GetStringRep() const
{
  char addrBuf[(sizeof(m_Object)*2+2+sizeof(const Type*)*2+2)+10];
  const Type* type = m_Type.GetType();
  int pcv = (int(m_Const) << 1) | int(m_Volatile);
  int ocv = (int(m_Type.IsConst()) << 1) | int(m_Type.IsVolatile());
  sprintf(addrBuf, "_ptr%d_%p_%d_%p", pcv, type, ocv, m_Object);
  return String(addrBuf);
}


/**
 * Try to set the pointer by converting from a string representation.
 * Return whether the conversion succeeded.
 */
bool Pointer::SetFromStringRep(const String& ptrStr)
{
  m_Object = NULL;
  Type* type = NULL;
  int pcv = 0; // cv-qualifier flags of pointer.
  int ocv = 0; // cv-qualifier flags of object.
  sscanf(ptrStr.c_str(), "_ptr%d_%p_%d_%p", &pcv, &type, &ocv, &m_Object);
  m_Const =  ((pcv >> 1) & 1) == 1;
  m_Volatile = (pcv & 1) == 1;
  if(type)
    {
    bool isConst = ((ocv >> 1) & 1) == 1;
    bool isVolatile = (ocv & 1) == 1;
    m_Type = type->GetCvQualifiedType(isConst, isVolatile);
    }
  return ((m_Object != NULL) && (type != NULL));
}  

// Implement a new object type for Tcl.  It is just a Pointer object.

/**
 * The object type interface functions for Tcl.
 */
static void FreePointerInternalRep(Tcl_Obj* objPtr);
static void DupPointerInternalRep(Tcl_Obj* srcPtr, Tcl_Obj* copyPtr);
static void UpdateStringOfPointer(Tcl_Obj *objPtr);
static int SetPointerFromAny(Tcl_Interp* interp, Tcl_Obj* objPtr);

/**
 * A "Pointer" type defined for Tcl scripts.
 * It can store any pointer type defined for the "Pointer" class.
 */
Tcl_ObjType TclPointerType =
{
  "Pointer",                        /* name */
  FreePointerInternalRep,           /* freeIntRepProc */
  DupPointerInternalRep,            /* dupIntRepProc */
  UpdateStringOfPointer,            /* updateStringProc */
  SetPointerFromAny                 /* setFromAnyProc */
};


/**
 * Internal use: Easy access to the Pointer object stored in the Tcl_Obj..
 */
inline Pointer& ToPointer(Tcl_Obj* objPtr)
{
  return *(Pointer*)objPtr->internalRep.otherValuePtr;
}

/**
 * Internal use: Easy setting of a Tcl_Obj to be a Pointer object.
 */
inline void SetPointer(Pointer* ptr, Tcl_Obj* objPtr)
{
  objPtr->internalRep.otherValuePtr = static_cast<VOID*>(ptr);
  objPtr->typePtr = &TclPointerType;
  UpdateStringOfPointer(objPtr);
}


/**
 * Tcl object implementation:
 * Free the memory used by the internal representation of the Pointer type.
 */
static void FreePointerInternalRep(Tcl_Obj* objPtr)
{
  delete (&ToPointer(objPtr));
}


/**
 * Tcl object implementation:
 * Make a copy of the given Pointer object.
 */
static void DupPointerInternalRep(Tcl_Obj* srcPtr, Tcl_Obj* destPtr)
{
  SetPointer(new Pointer(ToPointer(srcPtr)), destPtr);
}


/**
 * Tcl object implementation:
 * Setup the string representation of the Tcl object ("bytes" member) to be
 * that of the contained Pointer object.
 */
static void UpdateStringOfPointer(Tcl_Obj *objPtr)
{
  String stringRep = ToPointer(objPtr).GetStringRep();
  objPtr->bytes = Tcl_Alloc(stringRep.length()+1);
  objPtr->length = stringRep.length();
  
  strcpy(objPtr->bytes, stringRep.c_str());
}


/**
 * Tcl object implementation:
 * Attempt to construct a Pointer object from any other Tcl object by
 * converting from the string representation.
 */
static int SetPointerFromAny(Tcl_Interp* interp, Tcl_Obj* objPtr)
{
  const char* stringRep = Tcl_GetStringFromObj(objPtr, NULL);

  /**
   * Make a new Pointer object.
   */
  Pointer* newPointer = new Pointer;
  
  /**
   * Try to convert from the string representation.
   * If the conversion fails, report error.
   */
  if(!newPointer->SetFromStringRep(stringRep))
    {    
    if(interp != NULL)
      {
      char buf[100];
      sprintf(buf, "Expected Pointer, but got \"%.50s\"", stringRep);
      Tcl_ResetResult(interp);
      Tcl_AppendToObj(Tcl_GetObjResult(interp), buf, -1);
      }
    
    delete newPointer;
    
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
   * Replace the old object representation with the Pointer object
   * representation.
   */
  SetPointer(newPointer, objPtr);
  
  return TCL_OK;
}  


/**
 * Tcl object interface:
 * Get a Pointer object from the given Tcl object.  If the internal
 * representation of the Tcl object is not already a Pointer object, try
 * to convert it.
 */
int Tcl_GetPointerFromObj(Tcl_Interp* interp, Tcl_Obj* objPtr, Pointer* ptr)
{
  /**
   * If conversion is needed, try it.
   */
  if(objPtr->typePtr != &TclPointerType)
    {
    int error;
    if((error = SetPointerFromAny(interp, objPtr)) != TCL_OK)
      { return error; }
    }
  
  /**
   * If there is a destination for the Pointer object, put it there.
   */
  if(ptr != NULL)
    {
    *ptr = ToPointer(objPtr);
    }
  return TCL_OK;
}


/**
 * Tcl object interface:
 * Set a Tcl object to represent a Pointer object.
 */
void Tcl_SetPointerObj(Tcl_Obj* objPtr, const Pointer& ptr)
{
  SetPointer(new Pointer(ptr), objPtr);
}


/**
 * Tcl object interface:
 * Create a new Tcl object which represents a Pointer object.
 */
Tcl_Obj* Tcl_NewPointerObj(const Pointer& ptr)
{
  Tcl_Obj* objPtr = new Tcl_Obj;
  Tcl_SetPointerObj(objPtr, ptr);
  return objPtr;
}



/**
 * Check whether the given object's type is "Pointer".
 */
bool TclObjectTypeIsPointer(Tcl_Obj* o)
{
  return (!TclObjectTypeIsNULL(o)
          && (strcmp("Pointer", (o)->typePtr->name)==0));
}


/**
 * Check whether the given string representation is that of a Pointer.
 */
bool StringRepIsPointer(const String& s)
{
  return (((s).substr(0, 4)) == "_ptr");
}


} // namespace _wrap_
