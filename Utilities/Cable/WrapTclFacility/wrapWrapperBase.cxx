/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapWrapperBase.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "wrapWrapperBase.h"

namespace _wrap_
{


/**
 * The constructor links this wrapper to the given interpreter.  It
 * also gets the interpreter's TypeSystem, InstanceTable, and WrapperTable
 * for this wrapper's use.
 */
WrapperBase::WrapperBase(Tcl_Interp* interp, const String& wrappedTypeName):
  m_Interpreter(interp),
  m_WrappedTypeName(wrappedTypeName),
  m_TypeSystem(TypeSystemTable::GetForInterpreter(m_Interpreter)),
  m_InstanceTable(InstanceTable::GetForInterpreter(m_Interpreter)),
  m_WrapperTable(WrapperTable::GetForInterpreter(m_Interpreter)),
  m_WrappedTypeRepresentation(NULL)
{
}


/**
 * Virtual destructor.
 */
WrapperBase::~WrapperBase()
{
}


/**
 * Get the interpreter to which this wrapper is attached.
 */
Tcl_Interp* WrapperBase::GetInterpreter() const
{
  return m_Interpreter;
}


/**
 * Get the TypeMap entry with the given key.
 */
CvQualifiedType WrapperBase::GetType(TypeKey typeKey) const
{
  TypeMap::const_iterator i = m_TypeMap.find(typeKey);
  if(i == m_TypeMap.end())
    {
    String errorMessage = "Type \"";
    errorMessage += typeKey;
    errorMessage += "\" has no entry in type map of wrapper for ";
    errorMessage += m_WrappedTypeName;
    this->ReportErrorMessage(errorMessage);
    // TODO: Throw exception?
    return CvQualifiedType();
    }
  return i->second;
}


/**
 * Set the TypeMap entry with the given key to the given value.
 */
void WrapperBase::SetType(TypeKey typeKey, const CvQualifiedType& type)
{
  m_TypeMap[typeKey] = type;
}


/**
 * When an object is returned, this creates the command to allow wrapper
 * calls to be made to the object.  The command name is that given.
 *
 * If no wrapper exists for the type of the object, methods cannot be
 * invoked on it, but it can still be passed as an argument to other
 * wrapper calls.
 */
void WrapperBase::CreateResultCommand(const String& name,
                                      const Type* type) const
{
  // Try to get the wrapper for the given type.
  WrapperBase* wrapper = m_WrapperTable->GetWrapper(type);
  if(wrapper)
    {
    // Found a wrapper.  Create the command to use it.
    Tcl_CreateObjCommand(m_Interpreter,
                         const_cast<char*>(name.c_str()),
                         wrapper->GetObjectWrapperFunction(),
                         (ClientData)NULL, (Tcl_CmdDeleteProc*)NULL);
    }
  else
    {
    // TODO: Create a dummy wrapper that reports that the wrapper
    // for the type has not been loaded.
    }
}


/**
 * When an object instance is returned from a wrapped call, this is called
 * to create the temporary.  This just passes the call through to the
 * wrapper's InstanceTable.
 */
String WrapperBase::CreateTemporary(void* object, const CvQualifiedType& type)
{
  return m_InstanceTable->CreateTemporary(object, type);
}


/**
 * When an individual wrapper cannot find a method, this is called to
 * try to chain up the hierarchy from the wrapper's class.
 */
int WrapperBase::ChainMethod(const String& methodName,
                              ClientData clientData,
                              int objc, Tcl_Obj* CONST objv[]) const
{
  // MUST BE IMPLEMENTED.
  // For now, just report method not found.
  this->UnknownMethod(methodName, objc-2, objv+2);
  return TCL_ERROR;
}


/**
 * When an object name is given as a command with no arguments, no method
 * name has been specified.  This is called to generate the error message.
 */
void WrapperBase::NoMethodSpecified() const
{
  String errorMessage =
    "No method specified: "+m_WrappedTypeName+"::???";
  this->ReportErrorMessage(errorMessage);
}


/**
 * When an unknown method is encountered by the wrapper, this is called
 * to generate the error message.
 */
void WrapperBase::UnknownMethod(const String& methodName,
                                int argc, Tcl_Obj*CONST objv[]) const
{
  String errorMessage = 
    "Unknown method: " + m_WrappedTypeName + "::" + methodName + "(";

  // TODO: Chain up hierarchy here??
//  for(int i=0; i < (argc-1); ++i)
//    errorMessage += GetObjType(interp, objv[i]) + " , ";
//  if(argc > 0) errorMessage += GetObjType(interp, objv[argc-1]);
  
  errorMessage += ")";  
  this->ReportErrorMessage(errorMessage);
}


/**
 * When an object used as a command name no longer exists, but is still
 * a command in the interpreter, this is called to report the error.
 * This should never happen.
 */
void WrapperBase::UnknownInstance(const String& objectName) const
{
  String errorMessage =
    "No instance of \""+m_WrappedTypeName+"\" named \""+objectName+"\".";
  this->ReportErrorMessage(errorMessage);
}


/**
 * This is called to report an error message to the Tcl interpreter.
 */
void WrapperBase::ReportErrorMessage(const String& errorMessage) const
{
  Tcl_ResetResult(m_Interpreter);
  Tcl_AppendToObj(Tcl_GetObjResult(m_Interpreter),
                  const_cast<char*>(errorMessage.c_str()), -1);
}


/**
 * Any objects which are temporaries are deleted.
 * It is assumed that objv[1] is the method name, and is ignored.
 * The command (instance) name itself, and any arguments to the methods are
 * possibly temporary objects.
 */
void WrapperBase::FreeTemporaries(int objc, Tcl_Obj*CONST objv[]) const
{
  for(int i=0; i < objc; ++i)
    {
    if(i == 1) continue;
    if(TclObjectTypeIsNULL(objv[i])
       || TclObjectTypeIsString(objv[i])
       || TclObjectTypeIsCmdName(objv[i]))
      {
      String objectName = Tcl_GetStringFromObj(objv[i], NULL);
      
      // If there is a temporary instance by this name, delete it.
      if(m_InstanceTable->Exists(objectName))
        {
        m_InstanceTable->DeleteIfTemporary(objectName);
        }
      
      // TODO: Delete pointer/reference command names?
      }
    }
}


/**
 * Try to figure out the name of the type of the given Tcl object.
 * If the type cannot be determined, a default of "char*" is returned.
 * Used for type-based overload resolution.
 */
CvQualifiedType WrapperBase::GetObjectType(Tcl_Obj* obj) const
{
  // First try to use type information from Tcl.
  if(TclObjectTypeIsPointer(obj))
    {
    Pointer p;
    Tcl_GetPointerFromObj(m_Interpreter, obj, &p);
    return m_TypeSystem->GetPointerType(p.GetCvQualifiedType())
      ->GetCvQualifiedType(false, false);
    }
  else if(TclObjectTypeIsReference(obj))
    {
    Reference r;
    Tcl_GetReferenceFromObj(m_Interpreter, obj, &r);
    return m_TypeSystem->GetReferenceType(r.GetCvQualifiedType())
      ->GetCvQualifiedType(false, false);
    }
  else if(TclObjectTypeIsBoolean(obj))
    {
    return m_TypeSystem->GetFundamentalType(FundamentalType::Bool)
      ->GetCvQualifiedType(false, false);
    }
  else if(TclObjectTypeIsInt(obj))
    {
    return m_TypeSystem->GetFundamentalType(FundamentalType::Int)
      ->GetCvQualifiedType(false, false);
    }
  else if(TclObjectTypeIsLong(obj))
    {
    return m_TypeSystem->GetFundamentalType(FundamentalType::LongInt)
      ->GetCvQualifiedType(false, false);
    }
  else if(TclObjectTypeIsDouble(obj))
    {
    return m_TypeSystem->GetFundamentalType(FundamentalType::Double)
      ->GetCvQualifiedType(false, false);
    }
  // No Tcl type information.  Try converting from string representation.
  else
    {
    String objectName = Tcl_GetString(obj);
    if(m_InstanceTable->Exists(objectName))
      {
      return m_InstanceTable->GetType(objectName);
      }
    else if(StringRepIsPointer(objectName))
      {
      Pointer p;
      if(Tcl_GetPointerFromObj(m_Interpreter, obj, &p) == TCL_OK)
        {
        return m_TypeSystem->GetPointerType(p.GetCvQualifiedType())
          ->GetCvQualifiedType(false, false);
        }
      }
    else if(StringRepIsReference(objectName))
      {
      Reference r;
      if(Tcl_GetReferenceFromObj(m_Interpreter, obj, &r) == TCL_OK)
        {
        return m_TypeSystem->GetReferenceType(r.GetCvQualifiedType())
          ->GetCvQualifiedType(false, false);
        }
      }
    else
      {
      // No wrapping type information available.  Try to convert to
      // some basic types.
      long l;
      double d;
      if(Tcl_GetLongFromObj(m_Interpreter, obj, &l) == TCL_OK)
        {
        return m_TypeSystem->GetFundamentalType(FundamentalType::LongInt)
          ->GetCvQualifiedType(false, false);
        }
      else if(Tcl_GetDoubleFromObj(m_Interpreter, obj, &d) == TCL_OK)
        {
        return m_TypeSystem->GetFundamentalType(FundamentalType::Double)
          ->GetCvQualifiedType(false, false);
        }
      }
    }
  
  // Could not determine the type.  Default to char*.
  CvQualifiedType charType =
    m_TypeSystem->GetFundamentalType(FundamentalType::Char)
    ->GetCvQualifiedType(false, false);
  return m_TypeSystem->GetPointerType(charType)
    ->GetCvQualifiedType(false, false);
}


/**
 * Register an ArrayType having the given element type and size with this
 * wrapper's TypeSystem.
 */
CvQualifiedType
WrapperBase::GetArrayType(TypeKey elementType, unsigned long size) const
{
  return m_TypeSystem->GetArrayType(this->GetType(elementType), size)
    ->GetCvQualifiedType(false, false);
}


/**
 * Register a ClassType having the given name, cv-qualifiers, and
 * (optionally) parents with this wrapper's TypeSystem.
 */
CvQualifiedType
WrapperBase::GetClassType(const String& name,
                          bool isConst, bool isVolatile,
                          const ClassTypes& parents) const
{
  return m_TypeSystem->GetClassType(name, parents)
    ->GetCvQualifiedType(isConst, isVolatile);
}


/**
 * Register a FunctionType having the given return type, argument types,
 * and cv-qualifiers with this wrapper's TypeSystem.
 */
CvQualifiedType
WrapperBase::GetFunctionType(TypeKey returnType,
                             const TypeKeys& argumentTypes,
                             bool isConst, bool isVolatile) const
{
  CvQualifiedTypes cvQualifiedTypes;
  for(TypeKeys::const_iterator k = argumentTypes.begin();
      k != argumentTypes.end(); ++k)
    {
    cvQualifiedTypes.push_back(this->GetType(*k));
    }
  
  return m_TypeSystem->GetFunctionType(this->GetType(returnType),
                                       cvQualifiedTypes)
    ->GetCvQualifiedType(isConst, isVolatile);
}


/**
 * Register a FundamentalType having the given type id and cv-qualifiers
 * with this wrapper's TypeSystem.
 */
CvQualifiedType
WrapperBase::GetFundamentalType(FundamentalType::Id id,
                                bool isConst, bool isVolatile) const
{
  return m_TypeSystem->GetFundamentalType(id)
    ->GetCvQualifiedType(isConst, isVolatile);
}


/**
 * Register a PointerType pointing to the given type and having the
 * given cv-qualifiers with this wrapper's TypeSystem.
 */
CvQualifiedType
WrapperBase::GetPointerType(TypeKey referencedType,
                            bool isConst, bool isVolatile) const
{
  return m_TypeSystem->GetPointerType(this->GetType(referencedType))
    ->GetCvQualifiedType(isConst, isVolatile);
}


/**
 * Register a PointerToMemberType pointing to the given type inside the
 * given ClassType and having the given cv-qualifiers with this wrapper's
 * TypeSystem.
 */
CvQualifiedType
WrapperBase::GetPointerToMemberType(TypeKey referencedType,
                                    const ClassType* classScope,
                                    bool isConst, bool isVolatile) const
{
  return m_TypeSystem->GetPointerToMemberType(this->GetType(referencedType),
                                              classScope)
    ->GetCvQualifiedType(isConst, isVolatile);
}


/**
 * Register a ReferenceType referencing the given type with this wrapper's
 * TypeSystem.
 */
CvQualifiedType
WrapperBase::GetReferenceType(TypeKey referencedType) const
{
  return m_TypeSystem->GetReferenceType(this->GetType(referencedType))
    ->GetCvQualifiedType(false, false);
}

} // namespace _wrap_

