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
    String objectName = Tcl_GetString(obj);
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
        // When the conversion function dereferences its pointer, it must
        // get a pointer to char, not a char.
        argument.SetToPointer(Tcl_GetString(obj), CvType<char*>::type); 
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
 * ObjectWrapperDispatch calls this to select the candidate function best
 * matching the given argument types.
 *
 * If no suitable candidate is found, NULL is returned.
 *
 * THIS IS A HACK VERSION!  It should be reimplemented to do full
 * overload resolution.  Perhaps it should be moved into its own class.
 */
WrapperBase::FunctionBase*
WrapperBase::ResolveOverload(const CvQualifiedTypes& argumentTypes,
                             const CandidateFunctions& candidates) const
{
  // Construct a set of viable functions from the set of candidates.
  CandidateFunctions viableFunctions;
  
  // 13.3.2 Viable Functions
  for(CandidateFunctions::const_iterator i = candidates.begin();
      i != candidates.end(); ++i)
    {
    // 13.3.2/2
    // First, to be a viable function, a candidate function shall have enough
    // parameters to agree in number with the arguments in the list.
    if((*i)->GetNumberOfParameters() != argumentTypes.size())
      {
      continue;
      }
    
    // 13.3.2/3
    // Second, for F to be a viable function, there shall exist for each
    // argument an implicit conversion sequence (13.3.3.1) that converts
    // that argument to the corresponding parameter of F.
    CvQualifiedTypes::const_iterator argument = argumentTypes.begin();
    FunctionBase::ParameterTypes::const_iterator parameter = (*i)->ParametersBegin();
    for(;argument != argumentTypes.end(); ++argument, ++parameter)
      {
      CvQualifiedType from = *argument;
      const Type* to = *parameter;
      // If the type is a reference, see if it can be bound.
      if(to->IsReferenceType())
        {
        if(Conversions::ReferenceCanBindAsIdentity(
             from, ReferenceType::SafeDownCast(to))
           || (this->GetConversionFunction(from, to) != NULL))
          {
          // This conversion can be done, move on to the next
          // argument/parameter pair.
          continue;
          }
        // This conversion cannot be done.
        break;
        }
      // If the types are identical, the argument/parameter pair is valid.
      else if(to->Id() == from.GetType()->Id())
        {
        // This conversion can be done, move on to the next
        // argument/parameter pair.
        continue;
        }
      else if((to->IsEitherPointerType() && from.GetType()->IsEitherPointerType())
              && Conversions::IsValidQualificationConversion(PointerType::SafeDownCast(from.GetType()),
                                                             PointerType::SafeDownCast(to)))
        {
        // This conversion can be done, move on to the next
        // argument/parameter pair.
        continue;
        }
      else if(this->GetConversionFunction(from, to) == NULL)
        {
        // This conversion cannot be done.
        break;
        }
      }
    // See if we made it through all the argument/parameter pairs.
    if(argument != argumentTypes.end())
      {
      // This conversion cannot be done.
      continue;
      }
    
    // The candidate is a viable function.
    viableFunctions.push_back(*i);
    }
  
  // If no viable functions remain, return NULL.
  if(viableFunctions.empty())
    {
    return NULL;
    }
  
  // Skip the rest of overload resolution.  Just take the first viable
  // function.  
  return viableFunctions[0];
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
  
  // Determine the argument types of the constructor call.
  CvQualifiedTypes argumentTypes;  
  for(int i=2; i < objc; ++i)
    {
    argumentTypes.push_back(this->GetObjectType(objv[i]));
    }
  
  // See if this wrapper knows about a constructor.
  String methodName = m_WrappedTypeName;
  if(m_FunctionMap.count(methodName) > 0)
    {
    // Prepare the set of candidate functions.
    CandidateFunctions candidates;    
    FunctionMap::const_iterator first = m_FunctionMap.lower_bound(methodName);
    FunctionMap::const_iterator last = m_FunctionMap.upper_bound(methodName);
    for(FunctionMap::const_iterator i = first; i != last; ++i)
      {
      candidates.push_back(i->second);
      }
    
    // See if any candidates match the given arguments.
    FunctionBase* constructor =
      this->ResolveOverload(argumentTypes, candidates);
    
    // Make sure we have a matching candidate.
    if(!constructor)
      {
      this->UnknownConstructor(methodName, argumentTypes, candidates);
      return TCL_ERROR;
      }
    
    // Try to call the wrapped constructor.
    try
      {
      constructor->Call(objc, objv);
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
    // We don't have a constructor.
    this->UnknownConstructor(methodName, argumentTypes);
    return TCL_ERROR;
    }  
  return TCL_OK;
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

  // Determine the argument types of the method call.
  CvQualifiedTypes argumentTypes;
  // Add the implicit object argument.
  argumentTypes.push_back(objectType);
  // Add the normal arguments.
  for(int i=2; i < objc; ++i)
    {
    argumentTypes.push_back(this->GetObjectType(objv[i]));
    }
  
  // Get the method name.
  String methodName = Tcl_GetString(objv[1]);
  
  // See if this wrapper knows about a method with this name.
  if(m_FunctionMap.count(methodName) > 0)
    {
    // Prepare the set of candidate functions.
    CandidateFunctions candidates;    
    FunctionMap::const_iterator first = m_FunctionMap.lower_bound(methodName);
    FunctionMap::const_iterator last = m_FunctionMap.upper_bound(methodName);
    for(FunctionMap::const_iterator i = first; i != last; ++i)
      {
      candidates.push_back(i->second);
      }
    
    // See if any candidates match the given arguments.
    FunctionBase* method =
      this->ResolveOverload(argumentTypes, candidates);
    
    // Make sure we have a matching candidate.  If not, we do not chain
    // up the class hierarchy because of name hiding.
    if(!method)
      {
      this->UnknownMethod(methodName, argumentTypes, candidates);
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
    this->UnknownMethod(methodName, argumentTypes);
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
  m_Type = CvType<bool>::type;
  m_ArgumentId = bool_id;
}


/**
 * Set the Argument to be the given int value.
 */
void WrapperBase::Argument::SetToInt(int i)
{
  m_Temp.m_int = i;
  m_Anything.object = &m_Temp.m_int;
  m_Type = CvType<int>::type;
  m_ArgumentId = int_id;
}


/**
 * Set the Argument to be the given long value.
 */
void WrapperBase::Argument::SetToLong(long l)
{
  m_Temp.m_long = l;
  m_Anything.object = &m_Temp.m_long;
  m_Type = CvType<long>::type;
  m_ArgumentId = long_id;
}


/**
 * Set the Argument to be the given double value.
 */
void WrapperBase::Argument::SetToDouble(double d)
{
  m_Temp.m_double = d;
  m_Anything.object = &m_Temp.m_double;
  m_Type = CvType<double>::type;
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
 * Constructor just initializes all members.  This is only called from
 * a subclass's constructor, which is only called by a member of a subclass
 * of WrapperBase.
 */
WrapperBase::FunctionBase::FunctionBase(const String& name,
                                        const ParameterTypes& parameterTypes):
  m_Name(name),
  m_ParameterTypes(parameterTypes)
{
}


/**
 * Need a virtual destructor.
 */
WrapperBase::FunctionBase::~FunctionBase()
{
}


/**
 * Get the name of the wrapped method.
 */
const String& WrapperBase::FunctionBase::GetName() const
{
  return m_Name;
}


/**
 * Get the number of arguments that the method takes.
 */
unsigned long WrapperBase::FunctionBase::GetNumberOfParameters() const
{
  return m_ParameterTypes.size();
}


/**  
 * Get a begin iterator to the method's parameter types.
 */
WrapperBase::FunctionBase::ParameterTypes::const_iterator
WrapperBase::FunctionBase::ParametersBegin() const
{
  return m_ParameterTypes.begin();
}


/**  
 * Get an end iterator to the method's parameter types.
 */
WrapperBase::FunctionBase::ParameterTypes::const_iterator
WrapperBase::FunctionBase::ParametersEnd() const
{
  return m_ParameterTypes.end();
}


} // namespace _wrap_

