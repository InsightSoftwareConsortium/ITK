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
  
  typedef const char* TypeKey;

  Tcl_Interp* GetInterpreter();
  CvQualifiedType GetType(TypeKey typeKey);
  void CreateResultCommand(const String& name,
                           const Type* type);
  
protected:
  int ChainMethod(const String& methodName, ClientData clientData,
                  int objc, Tcl_Obj* CONST objv[]);
  void UnknownMethod(const String& methodName, int objc, Tcl_Obj*CONST objv[]);
  void UnknownInstance(const String& objectName);
  void ReportErrorMessage(const String& errorMessage);
  void FreeTemporaries(int objc, Tcl_Obj*CONST objv[]);

  typedef std::vector<TypeKey> TypeKeys;
  CvQualifiedType GetArrayType(TypeKey elementType, unsigned long size);
  CvQualifiedType GetClassType(const String& name,
                               bool isConst, bool isVolatile,
                               const ClassTypes& parents = ClassTypes());
  CvQualifiedType GetFunctionType(TypeKey returnType,
                                  const TypeKeys& argumentTypes,
                                  bool isConst, bool isVolatile);
  CvQualifiedType GetFundamentalType(FundamentalType::Id,
                                     bool isConst, bool isVolatile);
  CvQualifiedType GetPointerType(TypeKey referencedType,
                                 bool isConst, bool isVolatile);
  CvQualifiedType GetPointerToMemberType(TypeKey referencedType,
                                         const ClassType* classScope,
                                         bool isConst, bool isVolatile);
  CvQualifiedType GetReferenceType(TypeKey referencedType);

  /**
   * The Tcl interpreter to which this wrapper is attached.
   */
  Tcl_Interp*    m_Interpreter;
  
  /**
   * The name of the wrapped type.
   */
  String         m_WrappedTypeName;
  
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
      Tcl_SetPointerObj(Tcl_GetObjResult(interp),
                        Pointer(const_cast<T*>(result), referencedType,
                                false, false));
      wrapper->CreateResultCommand(Tcl_GetString(Tcl_GetObjResult(interp)),
                                   referencedType.GetType());
      }
  };
};

} // namespace _wrap_

#endif
