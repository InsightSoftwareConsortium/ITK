/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapWrapperInclude.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
// Note: This header is inteded for multiple inclusion!  The include
// blockers are missing on purpose.

// Any program that includes windows.h gets this macro defined.
#if defined(GetMessage)
#undef GetMessage
#endif

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
 *   static void Method_name(const Arguments&) const; ??
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
   * Map from a Tcl interpreter to the Wrapper of _wrap_WRAPPED_TYPE for it.
   */
  typedef std::map<const Tcl_Interp*, Wrapper*>  InterpreterWrapperMap;  
  static InterpreterWrapperMap interpreterWrapperMap;  
  
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
  m_WrapperFacility->SetWrapper(m_WrappedTypeRepresentation, this);
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
    
  try
    {
    // Call the Wrapper's dispatch function.
    int result = wrapper->ClassWrapperDispatch(clientData, objc, objv);
    
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
