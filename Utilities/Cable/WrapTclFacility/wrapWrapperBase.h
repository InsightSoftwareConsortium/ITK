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

#include <vector>
#include <map>

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
  class MethodBase;
  typedef std::vector<MethodBase*> CandidateMethods;
  
  void AddMethod(MethodBase*);
  MethodBase* ResolveOverload(const CvQualifiedType& objectType,
                              const CvQualifiedTypes& argumentTypes,
                              const CandidateMethods& candidates) const;
  void NoMethodSpecified() const;
  void UnknownMethod(const CvQualifiedType& objectType,
                     const String& methodName,
                     const CvQualifiedTypes& argumentTypes,
                     const CandidateMethods& = CandidateMethods()) const;
  void UnknownInstance(const String& objectName) const;
  void ReportErrorMessage(const String& errorMessage) const;
  void FreeTemporaries(int objc, Tcl_Obj*CONST objv[]) const;  
  int ObjectWrapperDispatch(ClientData ,int, Tcl_Obj* CONST[]) const;

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
  
  /**
   * Map from method name to method wrapper.  Duplicate names are
   * allowed.
   */
  typedef std::multimap<String, MethodBase*> MethodMap;
  
  /**
   * The method dispatch function needs to know about all possible methods.
   * This is defined here, but must be filled in by calls from a subclass
   * to AddMethod.
   */
  MethodMap m_MethodMap;
};


/**
 * Base class for all method wrappers.  A subclass of WrapperBase can
 * define a subclass of this to define a method wrapper corresponding
 * to the type it wraps.
 */
class WrapperBase::MethodBase
{
public:
  typedef std::vector<const Type*> ArgumentTypes;
  MethodBase(const String& name,
             const CvQualifiedType& implicit,
             const CvQualifiedType& returnType,
             const ArgumentTypes& argumentTypes);
  virtual ~MethodBase();

  const String& GetName() const;
  String GetMethodPrototype() const;
  
  /**
   * This is called by the overload resolution algorithm when this
   * method wrapper has been selected.  It must be defined by
   * a subclass to actually call a wrapped method.
   */
  virtual void Call(int objc, Tcl_Obj* CONST objv[]) const =0;
protected:
  /**
   * The name of the method.
   */
  String m_Name;
  
  /**
   * The type to which the method's "this" pointer points.
   */
  CvQualifiedType m_This;
  
  /**
   * The return type of the method.
   */
  CvQualifiedType m_ReturnType;
  
  /**
   * The argument types of the method.  These may be needed for
   * overload resolution.
   */
  ArgumentTypes m_ArgumentTypes;
  
  // We want the WrapperBase to be able to look at our members for
  // doing overload resolution.
  friend WrapperBase;
};


} // namespace _wrap_

#endif
