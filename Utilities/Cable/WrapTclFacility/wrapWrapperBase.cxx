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

#include <iostream>

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
  m_ConversionTable(ConversionTable::GetForInterpreter(m_Interpreter)),
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
  // Free all the wrapped methods.
  for(MethodMap::const_iterator i = m_MethodMap.begin();
      i != m_MethodMap.end(); ++i)
    {
    delete i->second;
    }
}


/**
 * Get the interpreter to which this wrapper is attached.
 */
Tcl_Interp* WrapperBase::GetInterpreter() const
{
  return m_Interpreter;
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
String WrapperBase::CreateTemporary(void* object,
                                    const CvQualifiedType& type) const
{
  return m_InstanceTable->CreateTemporary(object, type);
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
    return TypeInfo::GetPointerType(p.GetCvQualifiedType(), false, false);
    }
  else if(TclObjectTypeIsReference(obj))
    {
    Reference r;
    Tcl_GetReferenceFromObj(m_Interpreter, obj, &r);
    return TypeInfo::GetReferenceType(r.GetCvQualifiedType());
    }
  else if(TclObjectTypeIsBoolean(obj))
    {
    return CvType<bool>::type;
    }
  else if(TclObjectTypeIsInt(obj))
    {
    return CvType<int>::type;
    }
  else if(TclObjectTypeIsLong(obj))
    {
    return CvType<long>::type;
    }
  else if(TclObjectTypeIsDouble(obj))
    {
    return CvType<double>::type;
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
        return TypeInfo::GetPointerType(p.GetCvQualifiedType(), false, false);
        }
      }
    else if(StringRepIsReference(objectName))
      {
      Reference r;
      if(Tcl_GetReferenceFromObj(m_Interpreter, obj, &r) == TCL_OK)
        {
        return TypeInfo::GetReferenceType(r.GetCvQualifiedType());
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
        return CvType<long>::type;
        }
      else if(Tcl_GetDoubleFromObj(m_Interpreter, obj, &d) == TCL_OK)
        {
        return CvType<double>::type;
        }
      }
    }
  
  // Could not determine the type.  Default to char*.
  return CvType<char*>::type;
}


/**
 * Return whether an InstanceTable entry with the given name exists.
 */
bool WrapperBase::InstanceExists(const String& name) const
{
  return m_InstanceTable->Exists(name);
}


/**
 * Get a pointer to the InstanceTable entry with the given name.
 */
void* WrapperBase::GetInstanceObject(const String& name) const
{
  return m_InstanceTable->GetObject(name);
}


/**
 * Get the type of the InstanceTable entry with the given name.
 */
CvQualifiedType WrapperBase::GetInstanceType(const String& name) const
{
  return m_InstanceTable->GetType(name);
}


/**
 * Get the conversion function from the wrapper's ConversionTable for
 * the specified conversion.  The table will automatically try to add
 * cv-qualifiers to the "from" type to find a conversion.
 */
ConversionFunction
WrapperBase::GetConversionFunction(const CvQualifiedType& from,
                                   const Type* to) const
{
  return m_ConversionTable->GetConversion(from, to);
}


/**
 * A subclass calls this to add a wrapped method.
 */
void WrapperBase::AddMethod(MethodBase* method)
{
  String name = method->GetName();
  m_MethodMap.insert(MethodMap::value_type(name, method));
}


/**
 * ObjectWrapperDispatch calls this to select the candidate function best
 * matching the given argument types and implicit object parameter type.
 *
 * If no suitable candidate is found, NULL is returned.
 *
 * THIS IS A HACK VERSION!  It should be reimplemented to do full
 * overload resolution.
 */
WrapperBase::MethodBase*
WrapperBase::ResolveOverload(const CvQualifiedType& objectType,
                             const CvQualifiedTypes& argumentTypes,
                             const CandidateMethods& candidates) const
{
  return NULL;
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
void WrapperBase::UnknownMethod(const CvQualifiedType& objectType,
                                const String& methodName,
                                const CvQualifiedTypes& argumentTypes,
                                const CandidateMethods& candidates) const
{
  String errorMessage = "No method matches ";
  errorMessage += objectType.GetType()->Name() + "::" + methodName + "(";
  CvQualifiedTypes::const_iterator arg = argumentTypes.begin();
  while(arg != argumentTypes.end())
    {
    errorMessage += arg->GetName();
    if(++arg != argumentTypes.end())
      { errorMessage += ", "; }
    }
  errorMessage += ")";
  
  if(objectType.IsConst())
    {
    errorMessage += " const";
    }
  
  // If there are any candidates, list them.
  if(!candidates.empty())
    {
    errorMessage += "\nCandidates are:";
    for(CandidateMethods::const_iterator i = candidates.begin();
        i != candidates.end(); ++i)
      {
      String candidate = (*i)->GetMethodPrototype();
      errorMessage += "\n  "+candidate;
      }
    }
  
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
 * Dispatch function to select a wrapped method called through an object.
 */
int WrapperBase::ObjectWrapperDispatch(ClientData clientData,
                                       int objc, Tcl_Obj* CONST objv[]) const
{
  if(objc < 2)
    {
    this->NoMethodSpecified();
    return TCL_ERROR;
    }  
  
  // Determine the type of the object.  This should be the wrapped
  // type or a subclass of it, but possibly with cv-qualifiers added.
  CvQualifiedType objectType = this->GetObjectType(objv[0]);

  // Determine the argument types.  
  CvQualifiedTypes argumentTypes;  
  for(int i=2; i < objc; ++i)
    {
    argumentTypes.push_back(this->GetObjectType(objv[i]));
    }
  
  // Get the method name.
  String methodName = Tcl_GetString(objv[1]);
  
  // See if this wrapper knows about a method with this name.
  if(m_MethodMap.count(methodName) > 0)
    {
    // Prepare the set of candidate functions.
    CandidateMethods candidates;    
    MethodMap::const_iterator first = m_MethodMap.lower_bound(methodName);
    MethodMap::const_iterator last = m_MethodMap.upper_bound(methodName);
    for(MethodMap::const_iterator i = first; i != last; ++i)
      {
      candidates.push_back(i->second);
      }
    
    // See if any candidates match the given arguments.
    MethodBase* method = this->ResolveOverload(objectType, argumentTypes, 
                                               candidates);
    
    // Make sure we have a matching candidate.  If not, we do not chain
    // up the class hierarchy because of name hiding.
    if(!method)
      {
      this->UnknownMethod(objectType, methodName, argumentTypes, candidates);
      return TCL_ERROR;
      }
    
    // Try to call the wrapped method.
    try
      {
      method->Call(objc, objv);
      }
    catch (TclException e)
      {
      this->ReportErrorMessage(e.GetMessage());
      return TCL_ERROR;
      }
    // We must catch any C++ exception to prevent it from unwinding the
    // call stack back through the Tcl interpreter's C code.
    catch (...)
      {
      this->ReportErrorMessage("Caught unknown exception!!");
      return TCL_ERROR;
      }
    }
  else
    {
    // We don't have a method by this name, try to get a wrapper for
    // chaining the call up the hierarchy.
    // MUST BE IMPLEMENTED
    // For now, just report that the method is not known.
    this->UnknownMethod(objectType, methodName, argumentTypes);
    return TCL_ERROR;
    }
  return TCL_OK;  
}



/**
 * Constructor just initializes all members.  This is only called from
 * a subclass's constructor, which is only called by a member of a subclass
 * of WrapperBase.
 */
WrapperBase::MethodBase::MethodBase(const String& name,
                                    const CvQualifiedType& implicit,
                                    const CvQualifiedType& returnType,
                                    const ArgumentTypes& argumentTypes):
  m_Name(name),
  m_This(implicit),
  m_ReturnType(returnType),
  m_ArgumentTypes(argumentTypes)
{
}


/**
 * Need a virtual destructor.
 */
WrapperBase::MethodBase::~MethodBase()
{
}


/**
 * Get the name of the wrapped method.
 */
const String& WrapperBase::MethodBase::GetName() const
{
  return m_Name;
}


/**
 * Get a string representation of the method's function prototype.
 */
String WrapperBase::MethodBase::GetMethodPrototype() const
{
  String prototype = m_ReturnType.GetName() + " ";
  prototype += m_This.GetType()->Name() + "::" + m_Name + "(";
  ArgumentTypes::const_iterator arg = m_ArgumentTypes.begin();
  while(arg != m_ArgumentTypes.end())
    {
    prototype += (*arg)->Name();
    if(++arg != m_ArgumentTypes.end())
      { prototype += ", "; }
    }
  prototype += ")";
  
  if(m_This.IsConst())
    {
    prototype += " const";
    }
  
  return prototype;
}


} // namespace _wrap_

