/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapWrapperBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _wrapWrapperBase_h
#define _wrapWrapperBase_h

#include "wrapUtils.h"
#include "wrapTypeInfo.h"
#include "wrapConversionTable.h"
#include "wrapPointer.h"
#include "wrapReference.h"
#include "wrapInstanceTable.h"
#include "wrapWrapperTable.h"
#include "wrapException.h"

namespace _wrap_
{

/**
 * An individual wrapper class is a specialization of this template.
 */
template <class T>
class Wrapper;

/**
 * Implement functionality common to all wrapper classes.  An individual
 * wrapper class should inherit from this class.
 */
class _wrap_EXPORT WrapperBase
{
public:
  WrapperBase(Tcl_Interp* interp, const String& wrappedTypeName);
  virtual ~WrapperBase();
  
  Tcl_Interp* GetInterpreter() const;
  void CreateResultCommand(const String& name, const Type* type) const;
  String CreateTemporary(void* object, const CvQualifiedType&) const;
  
  CvQualifiedType GetObjectType(Tcl_Obj* obj) const;
  bool InstanceExists(const String& name) const;
  void* GetInstanceObject(const String& name) const;
  CvQualifiedType GetInstanceType(const String& name) const;
  ConversionFunction GetConversionFunction(const CvQualifiedType& from,
                                           const Type* to) const;
  
  /**
   * The type of a wrapper function for a Tcl interpreter call-back.
   */
  typedef int (*WrapperFunction)(ClientData, Tcl_Interp*, int, Tcl_Obj* CONST[]);
  
  /**
   * Get the wrapper function for a Tcl interpreter call-back for commands
   * referencing the name of the wrapped type.
   *
   * This function should handle instance creation and the calling of
   * static member functions of the type.
   */
  virtual WrapperFunction GetClassWrapperFunction() const =0;
  
  /**
   * Get the wrapper function for a Tcl interpreter call-back for commands
   * referencing instances of the wrapped type.
   *
   * This function should handle method calls on instances, references
   * to instances, and pointers to instances of the type.
   */
  virtual WrapperFunction GetObjectWrapperFunction() const =0;
  
protected:
  int ChainMethod(const String& methodName, ClientData clientData,
                  int objc, Tcl_Obj* CONST objv[]) const;
  void NoMethodSpecified() const;
  void UnknownMethod(const String& methodName, int objc, Tcl_Obj*CONST objv[]) const;
  void UnknownInstance(const String& objectName) const;
  void ReportErrorMessage(const String& errorMessage) const;
  void FreeTemporaries(int objc, Tcl_Obj*CONST objv[]) const;

protected:
  /**
   * The Tcl interpreter to which this wrapper is attached.
   */
  Tcl_Interp*    m_Interpreter;
  
  /**
   * The name of the wrapped type.
   */
  const String   m_WrappedTypeName;
  
  /**
   * The table of conversion functions for this wrapper's interpreter.
   */
  ConversionTable* m_ConversionTable;
  
  /**
   * The table of object instances for this wrapper's interpreter.
   */
  InstanceTable* m_InstanceTable;
  
  /**
   * The table of registered wrappers for this wrapper's interpreter.
   */
  WrapperTable*  m_WrapperTable;
  
  /**
   * The TypeSystem's representation for this wrapped type.
   */
  const Type*    m_WrappedTypeRepresentation;
};

} // namespace _wrap_

#endif
