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

namespace _wrap_
{

/** \class Pointer
 * Represent a pointer with its type.
 */
class Pointer
{
public:
  Pointer(): m_Object(NULL), m_Type(NULL),
             m_Const(false), m_Volatile(false) {}
  Pointer(void* object, const CvQualifiedType& type,
          bool isConst, bool isVolatile):
    m_Object(object), m_Type(type),
    m_Const(isConst), m_Volatile(isVolatile) {}
  
  /**
   * Get the pointer to the object.
   */
  void* GetObject() const { return m_Object; }
  
  /**
   * Get the type of the object.
   */
  const CvQualifiedType& GetCvQualifiedType() const { return m_Type; }
  
  /**
   * Return whether this pointer type is const-qualified.
   */
  bool IsConst() const { return m_Const; }
  
  /**
   * Return whether this pointer type is volatile-qualified.
   */
  bool IsVolatile() const { return m_Volatile; }
  
  String GetStringRep() const;
  bool SetFromStringRep(const String& ptrStr);
  
private:
  /**
   * The pointer to the object.
   */
  void* m_Object;
  
  /**
   * The type of the object.
   */
  CvQualifiedType m_Type;
  
  /**
   * Whether this pointer type has a const qualifier.
   */
  bool m_Const;
  
  /**
   * Whether this pointer type has a volatile qualifier.
   */
  bool m_Volatile;
};

/**
 * Standard Tcl interface for its object types.
 * This one is for the Pointer object.
 */
_wrap_EXPORT int Tcl_GetPointerFromObj(Tcl_Interp*, Tcl_Obj*, Pointer*);
_wrap_EXPORT void Tcl_SetPointerObj(Tcl_Obj*, const Pointer&);
_wrap_EXPORT Tcl_Obj* Tcl_NewPointerObj(const Pointer&);

_wrap_EXPORT bool TclObjectTypeIsReference(Tcl_Obj*);
_wrap_EXPORT bool StringRepIsReference(const String&);

} // namespace _wrap_

#endif
