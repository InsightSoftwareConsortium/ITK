/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapReference.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _wrapReference_h
#define _wrapReference_h

#include "wrapUtils.h"

#include <cstdio>

namespace _wrap_
{

/** \class Reference
 * Represent a reference with its type.
 */
class Reference
{
public:
  Reference(): m_Object(NULL), m_Type("<null object reference>") {}
  Reference(void* object, const String& type):
    m_Object(object), m_Type(type) {}
  
  /**
   * Get a pointer to the object.
   */
  void* Object(void) { return m_Object; }
  
  /**
   * Get the type of the object.
   */
  const String& Type(void) { return m_Type; }

  /**
   * Convert the reference to a string representation.
   */
  String StringRep(void)
    {
    char addrBuf[(sizeof(m_Object)*2)+5];
    sprintf(addrBuf, "_ref%p_", m_Object);
    return (String(addrBuf)+m_Type);
    }
  
  /**
   * Try to set the reference by converting from a string representation.
   * Return whether the conversion succeeded.
   */
  bool SetFromStringRep(const String& ptrStr)
    {
    char type[256]="";
    m_Object = NULL;
    m_Type = "";
    sscanf(ptrStr.c_str(), "_ref%p_%s",&m_Object, type);
    m_Type = type;
    return ((m_Object != NULL) && (m_Type != ""));
    }
  
private:
  /**
   * The reference to the object.
   */
  void* m_Object;
  
  /**
   * The type of the object.
   */
  String m_Type;
};

/**
 * Standard Tcl interface for its object types.
 * This one is for the Reference object.
 */
_wrap_EXPORT int Tcl_GetReferenceFromObj(Tcl_Interp*, Tcl_Obj*, Reference*);
_wrap_EXPORT void Tcl_SetReferenceObj(Tcl_Obj*, const Reference&);
_wrap_EXPORT Tcl_Obj* Tcl_NewReferenceObj(const Reference&);
  
#define ObjectTypeIsReference(o)  (((o)->typePtr != NULL) \
                                   && (strcmp("Reference", (o)->typePtr->name)==0))
#define StringRepIsReference(s)    (((s).substr(0, 4)) == "_ref")

} // namespace _wrap_

#endif
