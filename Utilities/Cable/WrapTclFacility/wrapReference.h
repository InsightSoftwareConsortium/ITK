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

namespace _wrap_
{

/** \class Reference
 * Represent a reference with its type.
 */
class Reference
{
public:
  Reference(): m_Object(NULL), m_Type(NULL) {}
  Reference(void* object, Type* type):
    m_Object(object), m_Type(type) {}
  
  /**
   * Get a pointer to the object.
   */
  void* GetObject() const { return m_Object; }
  
  /**
   * Get the type of the object.
   */
  const Type* GetType() const { return m_Type; }

  String GetStringRep() const;
  bool SetFromStringRep(const String&);
  
private:
  /**
   * The reference to the object.
   */
  void* m_Object;
  
  /**
   * The type of the object.
   */
  Type* m_Type;
};


// Standard Tcl interface for its object types.
// This one is for the Reference object.
_wrap_EXPORT int Tcl_GetReferenceFromObj(Tcl_Interp*, Tcl_Obj*, Reference*);
_wrap_EXPORT void Tcl_SetReferenceObj(Tcl_Obj*, const Reference&);
_wrap_EXPORT Tcl_Obj* Tcl_NewReferenceObj(const Reference&);

// A couple useful utility functions for the type.
_wrap_EXPORT bool TclObjectTypeIsReference(Tcl_Obj*);
_wrap_EXPORT bool StringRepIsReference(const String&);
  
} // namespace _wrap_

#endif
