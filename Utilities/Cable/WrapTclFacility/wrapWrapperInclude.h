/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapWrapperInclude.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include <set>
#include <map>

namespace _wrap_
{

/**
 * A specialization of the Wrapper class for actaully wrapping a
 * particular type.  This file, "wrapWrapperInclude.h" should be
 * included by a C++ source implementing a Wrapper.  The following
 * macros must be defined before including this source:
 *
 * _wrap_WRAPPED_TYPE
 *   should be the type to be wrapped.
 *
 * _wrap_WRAPPED_TYPE_NAME
 *   should be a string holding the name of the type to be wrapped.
 *
 * _wrap_METHOD_WRAPPER_PROTOTYPES
 *   should list the method wrapper prototypes.  The last one should not
 *   end in a semicolon.
 *
 * These methods must be defined by the rest of the source after
 * including this file.  They must be registered in the definition of
 * RegisterMethodWrappers().
 *
 * The prototypes of the method wrappers should look like this:
 *
 *   void Method_name(const Argument&, const Arguments&) const;
 *
 * Where "name" is the name of the method being wrapped.  In the case of
 * overloaded methods, a count of the form "#_" should be added before
 * the method name.  If the method is an operator, it should instead
 * begin with "Operator_" instead of "Method_".  Here are some examples:
 *
 * void Method_0_SetV(const Argument&, const Arguments&) const;
 * void Method_1_SetV(const Argument&, const Arguments&) const;
 * void Method_GetVector(const Argument&, const Arguments&) const;
 * void Operator_Plus(const Argument&, const Arguments&) const;
 * void Operator_PlusEquals(const Argument&, const Arguments&) const;
 *
 * This naming scheme should avoid name collisions.
 */
template <>
class Wrapper< _wrap_WRAPPED_TYPE >: public WrapperBase
{
public:
  /**
   * The actual type being wrapped.
   */
  typedef _wrap_WRAPPED_TYPE WrappedType;
  
  // Pull this typedef from our superclass.
  typedef WrapperBase::WrapperFunction WrapperFunction;
  
  // A few methods common to every Wrapper.  These are implemented below.
  Wrapper(Tcl_Interp* interp);  
  virtual WrapperFunction GetClassWrapperFunction() const;
  virtual WrapperFunction GetObjectWrapperFunction() const;
  void InitializeForInterpreter();  
  static Wrapper* GetForInterpreter(Tcl_Interp*);
  
private:
  // A few methods common to every Wrapper.  These are implemented below.
  static int ClassWrapperDispatchFunction(ClientData, Tcl_Interp*,
                                          int, Tcl_Obj*CONST[]);  
  static int ObjectWrapperDispatchFunction(ClientData, Tcl_Interp*,
                                           int, Tcl_Obj*CONST[]);
  
  /**
   * A set of arguments to be passed to a method wrapper.
   */
  typedef std::vector<Argument> Arguments;
  
  /**
   * The constructor wrapper pointer to member function type for constructor
   * wrappers in this Wrapper.
   */
  typedef void* (Wrapper<WrappedType>::*ConstructorWrapper)(const Arguments&) const;
  
  /**
   * The method wrapper pointer to member function type for method
   * wrappers in this Wrapper.
   */
  typedef void (Wrapper<WrappedType>::*MethodWrapper)(const Argument&,
                                                      const Arguments&) const;
  
  /**
   * Map from a Tcl interpreter to the Wrapper of _wrap_WRAPPED_TYPE for it.
   */
  typedef std::map<const Tcl_Interp*, Wrapper*>  InterpreterWrapperMap;  
  static InterpreterWrapperMap interpreterWrapperMap;
  
  /**
   * The subclass of WrapperBase::FunctionBase which is used for constructor
   * wrappers for this Wrapper.
   */
  class Constructor: public WrapperBase::FunctionBase
  {
  public:
    // Pull a typedef out of the superclass.
    typedef FunctionBase::ParameterTypes ParameterTypes;
    
    Constructor(Wrapper<WrappedType>* wrapper,
                ConstructorWrapper constructorWrapper,
                const String& name,
                const ParameterTypes& parameterTypes = ParameterTypes());
    virtual String GetPrototype() const;
    virtual void Call(int objc, Tcl_Obj*CONST objv[]) const;
  private:
    const Wrapper<WrappedType>* m_Wrapper;
    ConstructorWrapper m_ConstructorWrapper;
    const Type* m_Class;
  };
  
  /**
   * The subclass of WrapperBase::FunctionBase which is used for method
   * wrappers for this Wrapper.
   */
  class Method: public WrapperBase::FunctionBase
  {
  public:
    // Pull a typedef out of the superclass.
    typedef FunctionBase::ParameterTypes ParameterTypes;
    
    Method(Wrapper<WrappedType>* wrapper,
           MethodWrapper methodWrapper,
           const String& name,
           bool isConst,
           const CvQualifiedType& returnType,
           const ParameterTypes& parameterTypes = ParameterTypes());
    virtual String GetPrototype() const;
    virtual void Call(int objc, Tcl_Obj*CONST objv[]) const;
  private:
    const Wrapper<WrappedType>* m_Wrapper;
    MethodWrapper m_MethodWrapper;
    CvQualifiedType m_ReturnType;
  };
  
private:
  // Everything in the class below here must be implemented by the
  // file that includes this one.
  
  // Declare the method wrappers for this Wrapper.
  _wrap_METHOD_WRAPPER_PROTOTYPES;
  
  /**
   * Register the method wrappers for this Wrapper with its WrapperBase.
   */
  void RegisterMethodWrappers();
};


/**
 * Constructor just initializes the Wrapper to work with the given
 * interpreter.  It passes down some information to the WrapperBase.
 */
Wrapper< _wrap_WRAPPED_TYPE >:: 
Wrapper(Tcl_Interp* interp):
  WrapperBase(interp, _wrap_WRAPPED_TYPE_NAME)
{
  // Setup our superclass's record of the representation of the
  // wrapped type.
  m_WrappedTypeRepresentation =
    ClassType::SafeDownCast(CvType<WrappedType>::type.GetType());
  
  // Register our method wrappers with the superclass.
  this->RegisterMethodWrappers();
  
  // Setup this instance of the Wrapper to work with its interpreter.
  this->InitializeForInterpreter();
}


/**
 * Get the wrapper dispatch function to assign to a Tcl command referring
 * to the wrapped type.
 */
Wrapper< _wrap_WRAPPED_TYPE >::WrapperFunction
Wrapper< _wrap_WRAPPED_TYPE >
::GetClassWrapperFunction() const
{
  return &Wrapper::ClassWrapperDispatchFunction;
}


/**
 * Get the wrapper dispatch function to assign to a Tcl command referring
 * to an instance of the wrapped type or a pointer to it.
 */
Wrapper< _wrap_WRAPPED_TYPE >::WrapperFunction
Wrapper< _wrap_WRAPPED_TYPE >
::GetObjectWrapperFunction() const
{
  return &Wrapper::ObjectWrapperDispatchFunction;
}


/**
 * Setup this wrapper to work with its interpreter.
 */
void
Wrapper< _wrap_WRAPPED_TYPE >
::InitializeForInterpreter()
{
  m_WrapperTable->SetWrapper(m_WrappedTypeRepresentation, this);
  m_InstanceTable->SetDeleteFunction(m_WrappedTypeRepresentation,
                                     &OldObjectOf<WrappedType>::Delete);
  Tcl_CreateObjCommand(m_Interpreter,
                       const_cast<char*>(m_WrappedTypeName.c_str()),
                       this->GetClassWrapperFunction(),
                       (ClientData)NULL, (Tcl_CmdDeleteProc*)NULL);  
}


/**
 * The function called back from a Tcl interpreter when a command
 * referring to the wrapped type is invoked.
 *
 * This dispatches the call to the Wrapper of this type for the interpreter
 * that made the call-back.
 */
int
Wrapper< _wrap_WRAPPED_TYPE >
::ClassWrapperDispatchFunction(ClientData clientData, Tcl_Interp* interp,
                          int objc, Tcl_Obj* CONST objv[])
{
  // Get the Wrapper instance for this interpreter.
  Wrapper* wrapper = Wrapper::GetForInterpreter(interp);
  
  // Call the Wrapper's dispatch function.
  int result = wrapper->ClassWrapperDispatch(clientData, objc, objv);
  
  // Free any temporary objects that were used for the command.
  wrapper->FreeTemporaries(objc, objv);
  
  // Return the result code from the Wrapper to the interpreter.
  return result;
}


/**
 * The function called back from a Tcl interpreter when a command
 * referring to an instance of or pointer to the wrapped type is invoked.
 *
 * This dispatches the call to the Wrapper of this type for the interpreter
 * that made the call-back.
 */
int
Wrapper< _wrap_WRAPPED_TYPE >
::ObjectWrapperDispatchFunction(ClientData clientData, Tcl_Interp* interp,
                                int objc, Tcl_Obj* CONST objv[])
{
  // Get the Wrapper instance for this interpreter.
  Wrapper* wrapper = Wrapper::GetForInterpreter(interp);
    
  try
    {
    // Call the Wrapper's dispatch function.
    int result = wrapper->ObjectWrapperDispatch(clientData, objc, objv);
    
    // Free any temporary objects that were used for the command.
    wrapper->FreeTemporaries(objc, objv);
  
    // Return the result code from the Wrapper to the interpreter.
    return result;
    }
  catch (TclException e)
    {
    wrapper->ReportErrorMessage(e.GetMessage());
    return TCL_ERROR;
    }
  // We must catch any C++ exception to prevent it from unwinding the
  // call stack back through the Tcl interpreter's C code.
  catch (...)
    {
    wrapper->ReportErrorMessage("Caught unknown exception!!");
    return TCL_ERROR;
    }
}


/**
 * Get a Wrapper configured to work with the given interpreter.  If one
 * does not exist, it will be created.
 */
Wrapper< _wrap_WRAPPED_TYPE >*
Wrapper< _wrap_WRAPPED_TYPE >::GetForInterpreter(Tcl_Interp* interp)
{
  // See if a Wrapper exists for the given interpreter.
  if(interpreterWrapperMap.count(interp) == 0)
    {
    // No, we must create a new Wrapper for this interpreter.
    interpreterWrapperMap[interp] = new Wrapper(interp);
    }
  
  // Return the Wrapper.
  return interpreterWrapperMap[interp];
}


/**
 * Map from a Tcl interpreter to the Wrapper of _wrap_WRAPPED_TYPE for it.
 */
Wrapper< _wrap_WRAPPED_TYPE >::InterpreterWrapperMap
Wrapper< _wrap_WRAPPED_TYPE >::interpreterWrapperMap;


/**
 * The constructor passes the function name and pararmeter types down to
 * the FunctionBase.
 */
Wrapper< _wrap_WRAPPED_TYPE >
::Constructor::Constructor(Wrapper<WrappedType>* wrapper,
                           ConstructorWrapper constructorWrapper,
                           const String& name,
                           const ParameterTypes& parameterTypes):
  FunctionBase(name, parameterTypes),
  m_Wrapper(wrapper),
  m_ConstructorWrapper(constructorWrapper),
  m_Class(wrapper->GetWrappedTypeRepresentation())
{
}


/**
 * Get a string representation of the constructor's function prototype.
 */
String
Wrapper< _wrap_WRAPPED_TYPE >
::Constructor::GetPrototype() const
{
  String prototype = m_Class->Name() + "::" + m_Name + "(";
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
void
Wrapper< _wrap_WRAPPED_TYPE >
::Constructor::Call(int objc, Tcl_Obj*CONST objv[]) const
{
  // Prepare the list of arguments for the method wrapper to convert and pass
  // to the real method.
  Arguments arguments;
  for(int i=2; i < objc; ++i)
    {
    arguments.push_back(m_Wrapper->GetObjectArgument(objv[i]));
    }
  
  // Call the constructor wrapper.
  void* object = (m_Wrapper->*m_ConstructorWrapper)(arguments);
  
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
Wrapper< _wrap_WRAPPED_TYPE >
::Method::Method(Wrapper<WrappedType>* wrapper,
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
String
Wrapper< _wrap_WRAPPED_TYPE >
::Method::GetPrototype() const
{
  String prototype = m_ReturnType.GetName() + " ";
  ParameterTypes::const_iterator arg = m_ParameterTypes.begin();
  CvQualifiedType implicit = ReferenceType::SafeDownCast(*arg++)->GetReferencedType();
  prototype += implicit.GetType()->Name() + "::" + m_Name + "(";
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
void
Wrapper< _wrap_WRAPPED_TYPE >
::Method::Call(int objc, Tcl_Obj*CONST objv[]) const
{
  // Prepare the implicit object argument for the method wrapper to use
  // to call the real method.
  Argument implicit = m_Wrapper->GetObjectArgument(objv[0]);
  
  // Prepare the list of arguments for the method wrapper to convert and pass
  // to the real method.
  Arguments arguments;
  for(int i=2; i < objc; ++i)
    {
    arguments.push_back(m_Wrapper->GetObjectArgument(objv[i]));
    }
  
  // Call the method wrapper.
  (m_Wrapper->*m_MethodWrapper)(implicit, arguments);
}

// We only want the Initialization functions once per file that
// includes this header.
#ifndef _wrap_INITIALIZE_FUNCTION
#define _wrap_INITIALIZE_FUNCTION
namespace
{
/**
 * Initializes this file's CvType<> specializations.
 */
void InitializeTypeRepresentations();

/**
 * Initialize the given interpreter to do conversions for this file's
 * CvType<> specializations.
 */
void InitializeConversions(Tcl_Interp* interp);

/**
 * Makes sure that InitializeTypeRepresentations is called exactly once.
 */
void InitializeGroup()
{
  static bool initialized = false;
  if(initialized) { return; }
  InitializeTypeRepresentations();
  initialized = true;
}


/**
 * Makes sure that InitializeConversions is called exactly once per
 * interpreter.
 */
void InitializeGroupForInterpreter(Tcl_Interp* interp)
{
  InitializeGroup();
  static std::set<Tcl_Interp*> interpreters;
  if(interpreters.find(interp) == interpreters.end())
    {
    InitializeConversions(interp);
    interpreters.insert(interp);
    }
}

} // anonymous namespace
#endif


} // namespace _wrap_
