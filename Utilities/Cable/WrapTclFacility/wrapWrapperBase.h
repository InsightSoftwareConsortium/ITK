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
#include "wrapTypeSystemTable.h"
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
 * Every type invovled in a wrapper should have a specialization of this
 * class with the following member holding its name:
 * static const char* const name;
 */
template <class T>
struct TypeInfo;

/**
 * Implement functionality common to all wrapper classes.  An individual
 * wrapper class should inherit from this class.
 */
class _wrap_EXPORT WrapperBase
{
public:
  WrapperBase(Tcl_Interp* interp, const String& wrappedTypeName);
  virtual ~WrapperBase();
  
  typedef const char* TypeKey;

  Tcl_Interp* GetInterpreter() const;
  CvQualifiedType GetType(TypeKey typeKey) const;
  void SetType(TypeKey typeKey, const CvQualifiedType&);
  void CreateResultCommand(const String& name, const Type* type) const;
  
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

  typedef std::vector<TypeKey> TypeKeys;
  CvQualifiedType GetArrayType(TypeKey elementType, unsigned long size) const;
  CvQualifiedType GetClassType(const String& name,
                               bool isConst, bool isVolatile,
                               const ClassTypes& parents = ClassTypes()) const;
  CvQualifiedType GetFunctionType(TypeKey returnType,
                                  const TypeKeys& argumentTypes,
                                  bool isConst, bool isVolatile) const;
  CvQualifiedType GetFundamentalType(FundamentalType::Id,
                                     bool isConst, bool isVolatile) const;
  CvQualifiedType GetPointerType(TypeKey referencedType,
                                 bool isConst, bool isVolatile) const;
  CvQualifiedType GetPointerToMemberType(TypeKey referencedType,
                                         const ClassType* classScope,
                                         bool isConst, bool isVolatile) const;
  CvQualifiedType GetReferenceType(TypeKey referencedType) const;

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
   * The TypeSystem used to handle type information for this wrapper's
   * interpreter.
   */
  TypeSystem*    m_TypeSystem;
  
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
  
  typedef std::map<const char*, CvQualifiedType,
                   PointerCompare<const char> >  TypeMap;
private:
  /**
   * Map from const char* to CvQualifiedType.  This allows
   * wrappers to refer to types through TypeInfo<T>::name keys.
   * Must be private (with access routines) to prevent DLL address problems.
   */
  TypeMap        m_TypeMap;

public:
  
  /**
   * When a method returns a pointer type, the wrapper function calls this to
   * prepare the return.  This creates the PointerType Tcl object for the
   * Tcl object result.
   */
  template <typename T>
  struct ReturnPointerTo
  {
    static void From(WrapperBase* wrapper, T* result)
      {
      Tcl_Interp* interp = wrapper->GetInterpreter();
      CvQualifiedType referencedType = wrapper->GetType(TypeInfo<T>::name);
      
      // Set the Tcl result to a PointerType object representing this
      // pointer return value.
      Tcl_SetPointerObj(Tcl_GetObjResult(interp),
                        Pointer(const_cast<T*>(result), referencedType));
      
      // Create the command to allow methods to be invoked on the
      // resulting value.
      wrapper->CreateResultCommand(Tcl_GetString(Tcl_GetObjResult(interp)),
                                   referencedType.GetType());
      }
  };
  
  /**
   * When a method returns a reference type, the wrapper function calls this to
   * prepare the return.  This creates the ReferenceType Tcl object for the
   * Tcl object result.
   */
  template <typename T>
  struct ReturnReferenceTo
  {
    static void From(WrapperBase* wrapper, T* result)
      {
      Tcl_Interp* interp = wrapper->GetInterpreter();
      CvQualifiedType referencedType = wrapper->GetType(TypeInfo<T>::name);
      
      // Set the Tcl result to a ReferenceType object representing this
      // reference return value.
      Tcl_SetReferenceObj(Tcl_GetObjResult(interp),
                          Reference(const_cast<T*>(result), referencedType));
      
      // Create the command to allow methods to be invoked on the
      // resulting value.
      wrapper->CreateResultCommand(Tcl_GetString(Tcl_GetObjResult(interp)),
                                   referencedType.GetType());
      }
  };
};

} // namespace _wrap_

#endif
