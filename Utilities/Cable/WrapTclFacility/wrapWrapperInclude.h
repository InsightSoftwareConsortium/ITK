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
  int ClassWrapperDispatch(ClientData, int, Tcl_Obj*CONST[]);
  static void Initialize();  
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
   * The subclass of WrapperBase::MethodBase which is used for method
   * wrappers for this Wrapper.
   */
  class Method: public WrapperBase::MethodBase
  {
  public:
    // Pull a typedef out of the superclass.
    typedef MethodBase::ParameterTypes ParameterTypes;
    
    Method(Wrapper<WrappedType>* wrapper,
           MethodWrapper methodWrapper,
           const String& name,
           const CvQualifiedType& implicit,
           const CvQualifiedType& returnType,
           const ParameterTypes& parameterTypes = ParameterTypes());
    virtual void Call(int objc, Tcl_Obj*CONST objv[]) const;
  private:
    const Wrapper<WrappedType>* m_Wrapper;
    MethodWrapper m_MethodWrapper;
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
  
  /**
   * Initialize the CvType<> specializations needed by this Wrapper.
   */
  static void InitializeTypeRepresentations();  
};


/**
 * Constructor just initializes the Wrapper to work with the given
 * interpreter.  It passes down some information to the WrapperBase.
 */
Wrapper< _wrap_WRAPPED_TYPE >:: 
Wrapper(Tcl_Interp* interp):
  WrapperBase(interp, _wrap_WRAPPED_TYPE_NAME)
{
  // Be sure the type representations have been initialized.
  Wrapper::Initialize();
  
  // Setup our superclass's record of the representation of the
  // wrapped type.
  m_WrappedTypeRepresentation = CvType<WrappedType>::type.GetType();
  
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
                       ClassWrapperDispatchFunction,
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
  
  // Call the Wrapper's dispatch function.
  int result = wrapper->ObjectWrapperDispatch(clientData, objc, objv);
  
  // Free any temporary objects that were used for the command.
  wrapper->FreeTemporaries(objc, objv);
  
  // Return the result code from the Wrapper to the interpreter.
  return result;
}


/**
 * This is from ClassWrapperDispatchFunction to handle a call-back from
 * the Tcl interpreter to invoke a command referring to the wrapped type.
 */
int
Wrapper< _wrap_WRAPPED_TYPE >
::ClassWrapperDispatch(ClientData, int objc, Tcl_Obj* CONST objv[])
{
  char* instanceName = Tcl_GetString(objv[1]);
  
  // Create an instance of the wrapped type and put it in the InstanceTable
  // with the specified name.
  m_InstanceTable->SetObject(instanceName,
                             NewObjectOf<WrappedType>::Create(),
                             m_WrappedTypeRepresentation->GetCvQualifiedType(false, false));
  
  // Create the Tcl command corresponding to the instance.
  Tcl_CreateObjCommand(m_Interpreter, instanceName,
                       ObjectWrapperDispatchFunction,
                       (ClientData)NULL, (Tcl_CmdDeleteProc*)NULL);
  return TCL_OK;
}


/**
 * Makes sure that InitializeTypeRepresentations is called exactly once.
 */
void Wrapper< _wrap_WRAPPED_TYPE >::Initialize()
{
  static bool initialized = false;
  if(initialized) { return; }
  Wrapper::InitializeTypeRepresentations();
  initialized = true;
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
 * The constructor passes everything but the Wrapper and MethodWrapper
 * through to the MethodBase constructor.  The Wrapper and MethodWrapper
 * are needed to actually call the method wrapper this represents.
 */
Wrapper< _wrap_WRAPPED_TYPE >
::Method::Method(Wrapper<WrappedType>* wrapper,
                 MethodWrapper methodWrapper,
                 const String& name,
                 const CvQualifiedType& implicit,
                 const CvQualifiedType& returnType,
                 const ParameterTypes& parameterTypes):
  MethodBase(name, implicit, returnType, parameterTypes),
  m_Wrapper(wrapper),
  m_MethodWrapper(methodWrapper)
{
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


} // namespace _wrap_
