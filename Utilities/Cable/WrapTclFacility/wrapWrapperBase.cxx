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

Tcl_Interp* WrapperBase::GetInterpreter()
{
  return m_Interpreter;
}

CvQualifiedType WrapperBase::GetType(TypeKey typeKey)
{
  return m_TypeMap[typeKey];
}


void WrapperBase::CreateResultCommand(const String& name,
                                      const Type* type)
{
  if(m_WrapperTable->Exists(type))
    {
    Tcl_CreateObjCommand(m_Interpreter,
                         const_cast<char*>(name.c_str()),
                         m_WrapperTable->GetFunction(type),
                         (ClientData)NULL, (Tcl_CmdDeleteProc*)NULL);
    }
  else
    {
    // TODO: Create a dummy wrapper that reports that the wrapper
    // for the type has not been loaded.
    }
}


/**
 * When an individual wrapper cannot find a method, this is called to
 * try to chain up the hierarchy from the wrapper's class.
 */
int WrapperBase::ChainMethod(const String& methodName,
                              ClientData clientData,
                              int objc, Tcl_Obj* CONST objv[])
{
  // MUST BE IMPLEMENTED.
  // For now, just report method not found.
  this->UnknownMethod(methodName, objc-2, objv+2);
  return TCL_ERROR;
}


/**
 * When an unknown method is encountered by the wrapper, this is called
 * to generate the error message.
 */
void WrapperBase::UnknownMethod(const String& methodName,
                                int argc, Tcl_Obj*CONST objv[])
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
void WrapperBase::UnknownInstance(const String& objectName)
{
  String errorMessage =
    "No instance of \""+m_WrappedTypeName+"\" named \""+objectName+"\".";
  this->ReportErrorMessage(errorMessage);
}


/**
 * This is called to report an error message to the Tcl interpreter.
 */
void WrapperBase::ReportErrorMessage(const String& errorMessage)
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
void WrapperBase::FreeTemporaries(int objc, Tcl_Obj*CONST objv[])
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

CvQualifiedType
WrapperBase::GetArrayType(TypeKey elementType, unsigned long size)
{
  return m_TypeSystem->GetArrayType(m_TypeMap[elementType], size)
    ->GetCvQualifiedType(false, false);
}

CvQualifiedType
WrapperBase::GetClassType(const String& name,
                          bool isConst, bool isVolatile,
                          const ClassTypes& parents)
{
  return m_TypeSystem->GetClassType(name, parents)
    ->GetCvQualifiedType(isConst, isVolatile);
}

CvQualifiedType
WrapperBase::GetFunctionType(TypeKey returnType,
                             const TypeKeys& argumentTypes,
                             bool isConst, bool isVolatile)
{
  CvQualifiedTypes cvQualifiedTypes;
  for(TypeKeys::const_iterator k = argumentTypes.begin();
      k != argumentTypes.end(); ++k)
    {
    cvQualifiedTypes.push_back(m_TypeMap[*k]);
    }
  
  return m_TypeSystem->GetFunctionType(m_TypeMap[returnType],
                                       cvQualifiedTypes)
    ->GetCvQualifiedType(isConst, isVolatile);
}

CvQualifiedType
WrapperBase::GetFundamentalType(FundamentalType::Id id,
                                bool isConst, bool isVolatile)
{
  return m_TypeSystem->GetFundamentalType(id)
    ->GetCvQualifiedType(isConst, isVolatile);
}


CvQualifiedType
WrapperBase::GetPointerType(TypeKey referencedType,
                            bool isConst, bool isVolatile)
{
  return m_TypeSystem->GetPointerType(m_TypeMap[referencedType])
    ->GetCvQualifiedType(isConst, isVolatile);
}

CvQualifiedType
WrapperBase::GetPointerToMemberType(TypeKey referencedType,
                                    const ClassType* classScope,
                                    bool isConst, bool isVolatile)
{
  return m_TypeSystem->GetPointerToMemberType(m_TypeMap[referencedType],
                                              classScope)
    ->GetCvQualifiedType(isConst, isVolatile);
}


CvQualifiedType
WrapperBase::GetReferenceType(TypeKey referencedType)
{
  return m_TypeSystem->GetReferenceType(m_TypeMap[referencedType])
    ->GetCvQualifiedType(false, false);
}


} // namespace _wrap_

