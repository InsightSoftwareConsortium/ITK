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
  
  const Type* GetWrappedTypeRepresentation() const;
  
  Tcl_Interp* GetInterpreter() const;
  void CreateResultCommand(const String& name, const Type* type) const;
  void AddInstance(const String& name, void* object) const;
  String CreateTemporary(void* object, const CvQualifiedType&) const;
  
  class Argument;
  CvQualifiedType GetObjectType(Tcl_Obj* obj) const;
  Argument GetObjectArgument(Tcl_Obj* obj) const;
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
  
  class FunctionBase;
  typedef std::vector<FunctionBase*> CandidateFunctions;
  
protected:
  void AddFunction(FunctionBase*);
  FunctionBase* ResolveOverload(const CvQualifiedTypes& argumentTypes,
                                const CandidateFunctions& candidates) const;
  void NoNameSpecified() const;
  void NoMethodSpecified() const;
  void UnknownConstructor(const String& methodName,
                          const CvQualifiedTypes& argumentTypes,
                          const CandidateFunctions& = CandidateFunctions()) const;
  void UnknownMethod(const String& methodName,
                     const CvQualifiedTypes& argumentTypes,
                     const CandidateFunctions& = CandidateFunctions()) const;
  void UnknownInstance(const String& objectName) const;
  void ReportErrorMessage(const String& errorMessage) const;
  void FreeTemporaries(int objc, Tcl_Obj*CONST objv[]) const;
  int ObjectWrapperDispatch(ClientData ,int, Tcl_Obj* CONST[]) const;
  const WrapperBase* FindMethodWrapper(const String& name) const;
  bool HasMethod(const String& name) const;
  int ClassWrapperDispatch(ClientData, int, Tcl_Obj* CONST[]) const;
  int CallWrappedFunction(int, Tcl_Obj* CONST[]) const;

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
  const ClassType*    m_WrappedTypeRepresentation;
  
  /**
   * Map from method name to method wrapper.  Duplicate names are
   * allowed.
   */
  typedef std::multimap<String, FunctionBase*> FunctionMap;
  
  /**
   * The method dispatch function needs to know about all possible methods.
   * This is defined here, but must be filled in by calls from a subclass
   * to AddMethod.
   */
  FunctionMap m_FunctionMap;
};


/**
 * Holds an argument after extraction from a Tcl object, but before
 * passing to the final conversion function.  This is necessary because
 * the memory to which the argument refers may not be an InstanceTable
 * object.  It may be a pointer or a fundamental type.
 */
class _wrap_EXPORT WrapperBase::Argument
{
public:
  Argument();
  Argument(const Argument&);
  Argument& operator=(const Argument&);

  typedef Anything::ObjectType   ObjectType;
  typedef Anything::FunctionType FunctionType;
  
  Anything GetValue() const;
  const CvQualifiedType& GetType() const;
  void SetToObject(ObjectType object, const CvQualifiedType& type);
  void SetToBool(bool);
  void SetToInt(int);
  void SetToLong(long);
  void SetToDouble(double);
  void SetToPointer(ObjectType v, const CvQualifiedType& pointerType);
  void SetToFunction(FunctionType f,
                     const CvQualifiedType& functionPointerType);

private:
  enum ArgumentId { Uninitialized_id=0, Object_id, Pointer_id, Function_id,
                    bool_id, int_id, long_id, double_id };
  
  /**
   * The pointer to the actual object.
   */
  Anything m_Anything;

  /**
   * The type of the object.
   */
  CvQualifiedType m_Type;

  /**
   * Which type of Argument this is.
   */
  ArgumentId m_ArgumentId;
  
  /**
   * If a temporary is needed to hold the value extracted from the
   * Tcl object, this will hold it.
   */
  union
  {
    bool m_bool;
    int m_int;
    long m_long;
    double m_double;
  } m_Temp;
};


/**
 * Base class for all method wrappers.  A subclass of WrapperBase can
 * define a subclass of this to define a method wrapper corresponding
 * to the type it wraps.
 */
class _wrap_EXPORT WrapperBase::FunctionBase
{
public:
  typedef std::vector<const Type*> ParameterTypes;
  FunctionBase(const String& name,
               const ParameterTypes& parameterTypes);
  virtual ~FunctionBase();

  const String& GetName() const;
  virtual String GetPrototype() const =0;
  unsigned long GetNumberOfParameters() const;
  ParameterTypes::const_iterator ParametersBegin() const;
  ParameterTypes::const_iterator ParametersEnd() const;
  
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
   * The parameter types of the method.  These may be needed for
   * overload resolution.
   */
  ParameterTypes m_ParameterTypes;
};


} // namespace _wrap_

#endif
