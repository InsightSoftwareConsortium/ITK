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

#include <queue>

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
  String noTemplate = m_WrappedTypeName.substr(0, m_WrappedTypeName.find_first_of("<"));
  m_ConstructorName = noTemplate.substr(noTemplate.find_last_of(":") + 1);
}


/**
 * Virtual destructor.
 */
WrapperBase::~WrapperBase()
{
  // Free all the wrapped functions.
  for(FunctionMap::const_iterator i = m_FunctionMap.begin();
      i != m_FunctionMap.end(); ++i)
    {
    delete i->second;
    }
}


/**
 * Get the representation of the type that is wrapped by this instance.
 */
const Type* WrapperBase::GetWrappedTypeRepresentation() const
{
  return m_WrappedTypeRepresentation;
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
 * Add an instance of the wrapped object to the InstanceTable.
 */
void WrapperBase::AddInstance(const String& name, void* object) const
{
  // The instance has no cv-qualifiers.
  CvQualifiedType type = m_WrappedTypeRepresentation->GetCvQualifiedType(false, false);
  
  // Add the instance to the InstanceTable.
  m_InstanceTable->SetObject(name, object, type);
  
  // Create the Tcl command corresponding to the instance.
  Tcl_CreateObjCommand(m_Interpreter, const_cast<char*>(name.c_str()),
                       this->GetObjectWrapperFunction(),
                       (ClientData)NULL, (Tcl_CmdDeleteProc*)NULL);
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
    return TypeInfo::GetPointerType(p.GetPointedToType(), false, false);
    }
  else if(TclObjectTypeIsReference(obj))
    {
    Reference r;
    Tcl_GetReferenceFromObj(m_Interpreter, obj, &r);
    return TypeInfo::GetReferenceType(r.GetReferencedType());
    }
  else if(TclObjectTypeIsBoolean(obj))
    {
    return CvPredefinedType<bool>::type;
    }
  else if(TclObjectTypeIsInt(obj))
    {
    return CvPredefinedType<int>::type;
    }
  else if(TclObjectTypeIsLong(obj))
    {
    return CvPredefinedType<long>::type;
    }
  else if(TclObjectTypeIsDouble(obj))
    {
    return CvPredefinedType<double>::type;
    }
  // No Tcl type information.  Try converting from string representation.
  else
    {
    String objectName = Tcl_GetStringFromObj(obj, NULL);
    if(m_InstanceTable->Exists(objectName))
      {
      return m_InstanceTable->GetType(objectName);
      }
    else if(StringRepIsPointer(objectName))
      {
      Pointer p;
      if(Tcl_GetPointerFromObj(m_Interpreter, obj, &p) == TCL_OK)
        {
        return TypeInfo::GetPointerType(p.GetPointedToType(), false, false);
        }
      }
    else if(StringRepIsReference(objectName))
      {
      Reference r;
      if(Tcl_GetReferenceFromObj(m_Interpreter, obj, &r) == TCL_OK)
        {
        return TypeInfo::GetReferenceType(r.GetReferencedType());
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
        return CvPredefinedType<long>::type;
        }
      else if(Tcl_GetDoubleFromObj(m_Interpreter, obj, &d) == TCL_OK)
        {
        return CvPredefinedType<double>::type;
        }
      }
    }
  
  // Could not determine the type.  Default to char*.
  return CvPredefinedType<char*>::type;
}


/**
 * Try to figure out how to extract a C++ object from the given Tcl
 * object.  If the object type cannot be determined, char* is assumed.
 * In either case, an Argument which refers to the object is returned.
 */
WrapperBase::Argument WrapperBase::GetObjectArgument(Tcl_Obj* obj) const
{
  // Need a location to hold the Argument until returned.
  Argument argument;
  
  // First, see if Tcl has given us the type information.
  if(TclObjectTypeIsPointer(obj))
    {
    Pointer p;
    Tcl_GetPointerFromObj(m_Interpreter, obj, &p);
    argument.SetToPointer(p.GetObject(),
                          TypeInfo::GetPointerType(p.GetPointedToType(),
                                                   false, false));
    }
  else if(TclObjectTypeIsReference(obj))
    {
    Reference r;
    Tcl_GetReferenceFromObj(m_Interpreter, obj, &r);
    argument.SetToObject(r.GetObject(), r.GetReferencedType());
    }
  else if(TclObjectTypeIsBoolean(obj))
    {
    int i;
    Tcl_GetBooleanFromObj(m_Interpreter, obj, &i);
    bool b = (i!=0);
    argument.SetToBool(b);
    }
  else if(TclObjectTypeIsInt(obj))
    {
    int i;
    Tcl_GetIntFromObj(m_Interpreter, obj, &i);
    argument.SetToInt(i);
    }
  else if(TclObjectTypeIsLong(obj))
    {
    long l;
    Tcl_GetLongFromObj(m_Interpreter, obj, &l);
    argument.SetToLong(l);
    }
  else if(TclObjectTypeIsDouble(obj))
    {
    double d;
    Tcl_GetDoubleFromObj(m_Interpreter, obj, &d);
    argument.SetToDouble(d);
    }
  else
    {
    // Tcl has not given us the type information.  Try converting from
    // string representation.
    Pointer p;
    Reference r;

    // See if it the name of an instance.
    String objectName = Tcl_GetStringFromObj(obj, NULL);
    if(m_InstanceTable->Exists(objectName))
      {        
      argument.SetToObject(m_InstanceTable->GetObject(objectName),
                           m_InstanceTable->GetType(objectName));
      }
    else if(StringRepIsPointer(objectName)
            && (Tcl_GetPointerFromObj(m_Interpreter, obj, &p) == TCL_OK))
      {
      argument.SetToPointer(p.GetObject(),
                            TypeInfo::GetPointerType(p.GetPointedToType(),
                                                     false, false));
      }
    else if(StringRepIsReference(objectName)
            && (Tcl_GetReferenceFromObj(m_Interpreter, obj, &r) == TCL_OK))
      {
      argument.SetToObject(r.GetObject(), r.GetReferencedType());
      }
    else
      {
      // No type information available from string representation.
      // Try to convert to some basic Tcl types.
      long l;
      double d;
      if(Tcl_GetLongFromObj(m_Interpreter, obj, &l) == TCL_OK)
        {
        argument.SetToLong(l);
        }
      else if(Tcl_GetDoubleFromObj(m_Interpreter, obj, &d) == TCL_OK)
        {
        argument.SetToDouble(d);
        }
      else
        {
        // Can't identify the object type.  We will have to assume char*.
        argument.SetToPointer(Tcl_GetStringFromObj(obj, NULL),
                              CvPredefinedType<char*>::type);
        }
      }
    }
  
  // Return the result.
  return argument;
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
 * A subclass calls this to add a wrapped function.
 */
void WrapperBase::AddFunction(FunctionBase* function)
{
  m_FunctionMap.insert(FunctionMap::value_type(function->GetName(), function));
}


/**
 * When a type name is given as a command with no arguments, no instance
 * name has been specified.  This is called to generate the error message.
 */
void WrapperBase::NoNameSpecified() const
{
  String errorMessage =
    "No name specified for new object of "+m_WrappedTypeName;
  this->ReportErrorMessage(errorMessage);
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
 * When an unknown constructor is encountered by the wrapper, this is called
 * to generate the error message.
 */
void WrapperBase::UnknownConstructor(const String& methodName,
                                     const CvQualifiedTypes& argumentTypes,
                                     const CandidateFunctions& candidates) const
{
  String errorMessage = "No constructor matches ";
  errorMessage += m_WrappedTypeName + "::" + methodName + "(";
  CvQualifiedTypes::const_iterator arg = argumentTypes.begin();
  while(arg != argumentTypes.end())
    {
    errorMessage += arg->GetName();
    if(++arg != argumentTypes.end())
      { errorMessage += ", "; }
    }
  errorMessage += ")";
  
  // If there are any candidates, list them.
  if(!candidates.empty())
    {
    errorMessage += "\nCandidates are:";
    for(CandidateFunctions::const_iterator i = candidates.begin();
        i != candidates.end(); ++i)
      {
      String candidate = (*i)->GetPrototype();
      errorMessage += "\n  "+candidate;
      }
    }
  
  this->ReportErrorMessage(errorMessage);
}


/**
 * When an unknown method is encountered by the wrapper, this is called
 * to generate the error message.
 */
void WrapperBase::UnknownMethod(const String& methodName,
                                const CvQualifiedTypes& argumentTypes,
                                const CandidateFunctions& candidates) const
{
  String errorMessage = "No method matches ";
  CvQualifiedTypes::const_iterator arg = argumentTypes.begin();
  CvQualifiedType objectType = (*arg++);
  if(objectType.GetType()->IsReferenceType())
    {
    const ReferenceType* objectReference =
      ReferenceType::SafeDownCast(objectType.GetType());
    objectType = objectReference->GetReferencedType();
    }
  errorMessage += objectType.GetType()->Name() + "::" + methodName + "(";
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
    for(CandidateFunctions::const_iterator i = candidates.begin();
        i != candidates.end(); ++i)
      {
      String candidate = (*i)->GetPrototype();
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
 * Dispatch function to select a wrapped constructor called through
 * a command associated with the wrapped class name.
 */
int
WrapperBase
::ClassWrapperDispatch(ClientData, int objc, Tcl_Obj* CONST objv[]) const
{
  if(objc < 2)
    {
    this->NoNameSpecified();
    return TCL_ERROR;
    }  
  
  // See if any wrapper in our class hierarchy knows about a static method
  // with this name.
  String methodName = Tcl_GetStringFromObj(objv[1], NULL);
  const WrapperBase* wrapper = this->FindMethodWrapper(methodName);
  if(wrapper)
    {
    // We have found a wrapper that knows about the method.  Call it.
    return wrapper->CallWrappedFunction(objc, objv, true);
    }
  
  // We must select our own constructor.
  FunctionSelector functionSelector(this, objc, objv);
  
  // Prepare the set of candidate functions.
  CandidateFunctions candidates;    
  FunctionMap::const_iterator first = m_FunctionMap.lower_bound(m_ConstructorName);
  FunctionMap::const_iterator last = m_FunctionMap.upper_bound(m_ConstructorName);
  for(FunctionMap::const_iterator i = first; i != last; ++i)
    {
    functionSelector.AddCandidateConstructor(i->second);
    candidates.push_back(i->second);
    }
    
  // See if any candidates match the given arguments.
  FunctionBase* constructor = functionSelector.SelectConstructor();
  
  // Make sure we have a matching candidate.
  if(constructor)
    {
    constructor->Call(objc, objv);
    Tcl_SetStringObj(Tcl_GetObjResult(m_Interpreter), "", -1);  
    return TCL_OK;
    }
  else
    {
    this->UnknownConstructor(m_ConstructorName,
                             functionSelector.GetArgumentTypes(),
                             candidates);
    return TCL_ERROR;
    }
}



/**
 * Dispatch function to find a wrapper that knows about the method called.
 */
int WrapperBase::ObjectWrapperDispatch(ClientData clientData,
                                       int objc, Tcl_Obj* CONST objv[]) const
{
  // Make sure we have a method name.
  if(objc < 2)
    {
    this->NoMethodSpecified();
    return TCL_ERROR;
    }  
  
  // Get the method name.
  String methodName = Tcl_GetStringFromObj(objv[1], NULL);
  
  // See if any wrapper in our class hierarchy knows about a method
  // with this name.
  const WrapperBase* wrapper = this->FindMethodWrapper(methodName);
  if(wrapper)
    {
    // We have found a wrapper that knows about the method.  Call it.
    return wrapper->CallWrappedFunction(objc, objv, false);
    }
  else
    {
    // We don't know about the method.  Determine the argument types
    // of the method call to report the error.
    CvQualifiedTypes argumentTypes;
    // Add the implicit object argument.
    argumentTypes.push_back(this->GetObjectType(objv[0]));
    // Add the normal arguments.
    for(int i=2; i < objc; ++i)
      {
      argumentTypes.push_back(this->GetObjectType(objv[i]));
      }
    // Report that the method is unknown.
    this->UnknownMethod(methodName, argumentTypes);
    return TCL_ERROR;
    }
}


/**
 * Find a Wrapper that knows about a method with the given name.  This
 * may involve chaining up the class hierarchy.  If no wrapper knows about
 * the method, NULL is returned.
 */
const WrapperBase* WrapperBase::FindMethodWrapper(const String& name) const
{  
  // A queue to do a BFS of this class and its parents.
  std::queue<const ClassType*> classQueue;
  
  // Start with the search at this class.
  classQueue.push(ClassType::SafeDownCast(m_WrappedTypeRepresentation));
  while(!classQueue.empty())
    {
    // Get the next class off the queue.
    const ClassType* curClass = classQueue.front(); classQueue.pop();
    
    // If the class has a wrapper, see if it knows about the method.
    const WrapperBase* wrapper = m_WrapperTable->GetWrapper(curClass);
    if(wrapper && wrapper->HasMethod(name))
      {
      return wrapper;
      }
    
    // The class did not know about the method.  Add its parents to the queue.
    for(ClassTypes::const_iterator parent = curClass->ParentsBegin();
        parent != curClass->ParentsEnd(); ++parent)
      {
      classQueue.push(*parent);
      }
    }
  
  // We didn't find a wrapper that knows about the method.
  return NULL;
}


/**
 * Return whether this Wrapper knows about a method with the given name.
 */
bool WrapperBase::HasMethod(const String& name) const
{
  return (m_FunctionMap.count(name) > 0);
}


/**
 * This is called when this wrapper has been selected as knowing about
 * the method being invoked.  Here we use a FunctionSelector to get
 * the correct method wrapper, and call it.
 */
int WrapperBase::CallWrappedFunction(int objc, Tcl_Obj* CONST objv[],
                                     bool staticOnly) const
{  
  FunctionSelector functionSelector(this, objc, objv);
  
  // Get the method name.
  String methodName = Tcl_GetStringFromObj(objv[1], NULL);

  // Prepare the set of candidate functions.
  CandidateFunctions candidates;    
  FunctionMap::const_iterator first = m_FunctionMap.lower_bound(methodName);
  FunctionMap::const_iterator last = m_FunctionMap.upper_bound(methodName);
  for(FunctionMap::const_iterator i = first; i != last; ++i)
    {
    functionSelector.AddCandidateMethod(i->second);
    candidates.push_back(i->second);
    }
  
  // See if any candidates match the given arguments.
  FunctionBase* method = functionSelector.SelectMethod(staticOnly);
  
  // Make sure we have a matching candidate.  If not, we do not chain
  // up the class hierarchy because of name hiding.
  if(!method)
    {
    this->UnknownMethod(methodName, functionSelector.GetArgumentTypes(),
                        candidates);
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
  return TCL_OK;  
}


/**
 * Constructor just sets argument as uninitialized.
 */
WrapperBase::Argument::Argument():
  m_ArgumentId(Uninitialized_id)
{
}


/**
 * Copy constructor ensures m_Anything still points to the right place.
 */
WrapperBase::Argument::Argument(const Argument& a):
  m_Anything(a.m_Anything),
  m_Type(a.m_Type),
  m_ArgumentId(a.m_ArgumentId),
  m_Temp(a.m_Temp)
{
  // Make sure m_Anything points to the right place in the copy.
  switch (m_ArgumentId)
    {
    case bool_id:    m_Anything.object = &m_Temp.m_bool; break;
    case int_id:     m_Anything.object = &m_Temp.m_int; break;
    case long_id:    m_Anything.object = &m_Temp.m_long; break;
    case double_id:  m_Anything.object = &m_Temp.m_double; break;
    case Object_id:
    case Pointer_id:
    case Function_id:
    case Uninitialized_id:
    default: break;
    }
}


/**
 * Assignment operator just duplicates functionality of copy constructor.
 */
WrapperBase::Argument& WrapperBase::Argument::operator=(const Argument& a)
{
  // Copy the values.
  m_Anything = a.m_Anything;
  m_Type = a.m_Type;
  m_ArgumentId = a.m_ArgumentId;
  m_Temp = a.m_Temp;
  
  // Make sure m_Anything points to the right place in the copy.
  switch (m_ArgumentId)
    {
    case bool_id:    m_Anything.object = &m_Temp.m_bool; break;
    case int_id:     m_Anything.object = &m_Temp.m_int; break;
    case long_id:    m_Anything.object = &m_Temp.m_long; break;
    case double_id:  m_Anything.object = &m_Temp.m_double; break;
    case Object_id:
    case Pointer_id:
    case Function_id:
    case Uninitialized_id:
    default: break;
    }
  
  return *this;
}


/**
 * Get the value of the Argument for passing to the conversion function.
 */
Anything WrapperBase::Argument::GetValue() const
{
  // TODO: Throw exception for uninitalized argument.
  return m_Anything;
}


/**
 * Get the type of the Argument for selecting a conversion function.
 */
const CvQualifiedType& WrapperBase::Argument::GetType() const
{
  // TODO: Throw exception for uninitalized argument.
  return m_Type;
}


/**
 * Set the type of the Argument for selecting a conversion function.
 * This should only be used to dereference the implicit object argument.
 */
void WrapperBase::Argument::SetType(const CvQualifiedType& type)
{
  m_Type = type;
}


/**
 * Set the Argument to be the object pointed to by the given pointer.
 */
void WrapperBase::Argument::SetToObject(ObjectType object,
                                        const CvQualifiedType& type)
{
  m_Anything.object = object;
  m_Type = type;
  m_ArgumentId = Object_id;
}


/**
 * Set the Argument to be the given bool value.
 */
void WrapperBase::Argument::SetToBool(bool b)
{
  m_Temp.m_bool = b;
  m_Anything.object = &m_Temp.m_bool;
  m_Type = CvPredefinedType<bool>::type;
  m_ArgumentId = bool_id;
}


/**
 * Set the Argument to be the given int value.
 */
void WrapperBase::Argument::SetToInt(int i)
{
  m_Temp.m_int = i;
  m_Anything.object = &m_Temp.m_int;
  m_Type = CvPredefinedType<int>::type;
  m_ArgumentId = int_id;
}


/**
 * Set the Argument to be the given long value.
 */
void WrapperBase::Argument::SetToLong(long l)
{
  m_Temp.m_long = l;
  m_Anything.object = &m_Temp.m_long;
  m_Type = CvPredefinedType<long>::type;
  m_ArgumentId = long_id;
}


/**
 * Set the Argument to be the given double value.
 */
void WrapperBase::Argument::SetToDouble(double d)
{
  m_Temp.m_double = d;
  m_Anything.object = &m_Temp.m_double;
  m_Type = CvPredefinedType<double>::type;
  m_ArgumentId = double_id;
}


/**
 * Set the Argument to be the given pointer value.
 */
void WrapperBase::Argument::SetToPointer(ObjectType v,
                                         const CvQualifiedType& pointerType)
{
  m_Anything.object = v;
  m_Type = pointerType;
  m_ArgumentId = Pointer_id;
}


/**
 * Set the Argument to be the given function pointer value.
 */
void WrapperBase::Argument::SetToFunction(FunctionType f,
                                          const CvQualifiedType& functionPointerType)
{
  m_Anything.function = f;
  m_Type = functionPointerType;
  m_ArgumentId = Function_id;
}


/**
 * The constructor passes the function name and pararmeter types down to
 * the FunctionBase.
 */
WrapperBase::Constructor::Constructor(WrapperBase* wrapper,
                                      ConstructorWrapper constructorWrapper,
                                      const String& name,
                                      const ParameterTypes& parameterTypes):
  FunctionBase(name, parameterTypes),
  m_Wrapper(wrapper),
  m_ConstructorWrapper(constructorWrapper)
{
}


/**
 * Get a string representation of the constructor's function prototype.
 */
String WrapperBase::Constructor::GetPrototype() const
{
  String prototype = m_Wrapper->GetWrappedTypeRepresentation()->Name() + "::" + m_Name + "(";
  ParameterTypes::const_iterator arg = m_ParameterTypes.begin();
  while(arg != m_ParameterTypes.end())
    {
    prototype += (*arg)->Name();
    if(++arg != m_ParameterTypes.end())
      { prototype += ", "; }
    }
  prototype += ")";
  
  return prototype;
}


/**
 * Invokes a wrapped constructor.  This actually extracts the C++ objects
 * from the Tcl objects given as arguments and calls the constructor wrapper.
 *
 * If construction succeeds, this also adds the object to the Wrapper's
 * instance table.
 */
void WrapperBase::Constructor::Call(int objc, Tcl_Obj*CONST objv[]) const
{
  // Prepare the list of arguments for the method wrapper to convert and pass
  // to the real method.
  Arguments arguments;
  for(int i=2; i < objc; ++i)
    {
    arguments.push_back(m_Wrapper->GetObjectArgument(objv[i]));
    }
  
  // Call the constructor wrapper.
  void* object = m_ConstructorWrapper(m_Wrapper, arguments);
  
  // TODO: Make sure object != NULL
  
  // Get the name of the instance.
  char* instanceName = Tcl_GetStringFromObj(objv[1], NULL);
  
  // Insert the object into the instance table.
  m_Wrapper->AddInstance(instanceName, object);
}


/**
 * The constructor passes the function name and pararmeter types down to
 * the FunctionBase.  It then adds the implicit object parameter to the
 * front of the parameter list.
 */
WrapperBase::Method::Method(WrapperBase* wrapper,
                            MethodWrapper methodWrapper,
                            const String& name,
                            bool isConst,
                            const CvQualifiedType& returnType,
                            const ParameterTypes& parameterTypes):
  FunctionBase(name, parameterTypes),
  m_Wrapper(wrapper),
  m_MethodWrapper(methodWrapper),
  m_ReturnType(returnType)
{
  // Add the implicit object parameter to the front of the parameter list.
  CvQualifiedType wrappedType = wrapper->GetWrappedTypeRepresentation()
    ->GetCvQualifiedType(isConst, false);
  const Type* implicit = TypeInfo::GetReferenceType(wrappedType).GetType();
  m_ParameterTypes.insert(m_ParameterTypes.begin(), implicit);
}


/**
 * Get a string representation of the method's function prototype.
 */
String WrapperBase::Method::GetPrototype() const
{
  String prototype = m_ReturnType.GetName() + " ";
  ParameterTypes::const_iterator arg = m_ParameterTypes.begin();
  CvQualifiedType implicit = ReferenceType::SafeDownCast(*arg++)->GetReferencedType();
  prototype += m_Wrapper->GetWrappedTypeRepresentation()->Name() + "::" + m_Name + "(";
  while(arg != m_ParameterTypes.end())
    {
    prototype += (*arg)->Name();
    if(++arg != m_ParameterTypes.end())
      { prototype += ", "; }
    }
  prototype += ")";
  
  if(implicit.IsConst())
    {
    prototype += " const";
    }
  
  return prototype;
}


/**
 * Invokes a wrapped method.  This actually extracts the C++ objects
 * from the Tcl objects given as arguments and calls the method wrapper.
 */
void WrapperBase::Method::Call(int objc, Tcl_Obj*CONST objv[]) const
{
  // Prepare the implicit object argument for the method wrapper to use
  // to call the real method.
  Argument implicit = m_Wrapper->GetObjectArgument(objv[0]);
  
  if(implicit.GetType().IsPointerType())
    {
    implicit.SetType(PointerType::SafeDownCast(implicit.GetType().GetType())->GetPointedToType());
    }
  
  // Prepare the list of arguments for the method wrapper to convert and pass
  // to the real method.
  Arguments arguments;
  for(int i=2; i < objc; ++i)
    {
    arguments.push_back(m_Wrapper->GetObjectArgument(objv[i]));
    }
  
  // Call the method wrapper.
  m_MethodWrapper(m_Wrapper, implicit, arguments);
}


/**
 * The constructor passes the function name and pararmeter types down to
 * the FunctionBase.  It then adds the implicit object parameter to the
 * front of the parameter list.  This implicit object parameter for
 * a static method wrapper is of a special type that matches any object.
 */
WrapperBase
::StaticMethod::StaticMethod(WrapperBase* wrapper,
                             StaticMethodWrapper staticMethodWrapper,
                             const String& name,
                             const CvQualifiedType& returnType,
                             const ParameterTypes& parameterTypes):
  FunctionBase(name, parameterTypes),
  m_Wrapper(wrapper),
  m_StaticMethodWrapper(staticMethodWrapper),
  m_ReturnType(returnType)
{
  // Add the implicit object parameter to the front of the parameter list.
  const Type* implicit = TypeInfo::GetFundamentalType(FundamentalType::Void, false, false).GetType();
  m_ParameterTypes.insert(m_ParameterTypes.begin(), implicit);
}


/**
 * Get a string representation of the static method's function prototype.
 */
String
WrapperBase
::StaticMethod::GetPrototype() const
{
  String prototype = m_ReturnType.GetName() + " ";
  ParameterTypes::const_iterator arg = m_ParameterTypes.begin();
  ++arg; // Skip past implicit object parameter.
  prototype += m_Wrapper->GetWrappedTypeRepresentation()->Name() + "::" + m_Name + "(";
  while(arg != m_ParameterTypes.end())
    {
    prototype += (*arg)->Name();
    if(++arg != m_ParameterTypes.end())
      { prototype += ", "; }
    }
  prototype += ")";
  
  return prototype;
}


/**
 * Invokes a wrapped static method.  This actually extracts the C++ objects
 * from the Tcl objects given as arguments and calls the static method wrapper.
 */
void
WrapperBase
::StaticMethod::Call(int objc, Tcl_Obj*CONST objv[]) const
{
  // Prepare the list of arguments for the method wrapper to convert and pass
  // to the real method.
  Arguments arguments;
  for(int i=2; i < objc; ++i)
    {
    arguments.push_back(m_Wrapper->GetObjectArgument(objv[i]));
    }
  
  // Call the static method wrapper.
  m_StaticMethodWrapper(m_Wrapper, arguments);
}


} // namespace _wrap_

