/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapPointer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _wrapPointer_h
#define _wrapPointer_h

#include "wrapUtils.h"

#include <cstdio>
#include <cstring>

namespace _wrap_
{

/** \class Pointer
 * Represent a pointer with its type.
 */
class Pointer
{
public:
  Pointer(): m_Object(NULL), m_Type("<null object pointer>") {}
  Pointer(void* object, const String& type):
    m_Object(object), m_Type(type) {}
  
  /**
   * Get the pointer to the object.
   */
  void* Object(void) { return m_Object; }
  
  /**
   * Get the type of the object.
   */
  const String& Type(void) { return m_Type; }

  /**
   * Convert the pointer to a string representation.
   */
  String StringRep(void)
    {
    char addrBuf[(sizeof(m_Object)*2)+5];
    sprintf(addrBuf, "_ptr%p_", m_Object);
    return (String(addrBuf)+m_Type);
    }
  
  /**
   * Try to set the pointer by converting from a string representation.
   * Return whether the conversion succeeded.
   */
  bool SetFromStringRep(const String& ptrStr)
    {
    char type[256]="";
    m_Object = NULL;
    m_Type = "";
    sscanf(ptrStr.c_str(), "_ptr%p_%s",&m_Object, type);
    m_Type = type;
    return ((m_Object != NULL) && (m_Type != ""));
    }
  
private:
  /**
   * The pointer to the object.
   */
  void* m_Object;
  
  /**
   * The type of the object.
   */
  String m_Type;
};

/**
 * Standard Tcl interface for its object types.
 * This one is for the Pointer object.
 */
_wrap_EXPORT int Tcl_GetPointerFromObj(Tcl_Interp*, Tcl_Obj*, Pointer*);
_wrap_EXPORT void Tcl_SetPointerObj(Tcl_Obj*, const Pointer&);
_wrap_EXPORT Tcl_Obj* Tcl_NewPointerObj(const Pointer&);

#define ObjectTypeIsPointer(o)  (((o)->typePtr != NULL) \
                                 && (strcmp("Pointer", (o)->typePtr->name)==0))
#define StringRepIsPointer(s)    (((s).substr(0, 4)) == "_ptr")


} // namespace _wrap_


#endif
