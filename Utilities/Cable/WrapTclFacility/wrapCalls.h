/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapCalls.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _wrapCalls_h
#define _wrapCalls_h

#include "wrapWrapperBase.h"

namespace _wrap_
{

/**
 * When a method returns an object, the wrapper function calls this to
 * prepare the return.  This creates the temporary instance, sets the
 * Tcl object result, and creates the Tcl command in case of nested
 * command calls.
 */
template <typename T>
struct Return
{
  static void From(const T& result, WrapperBase* wrapper)
    {
    Tcl_Interp* interp = wrapper->GetInterpreter();
    CvQualifiedType type = wrapper->GetType(TypeInfo<T>::name);
    
    // Create a temporary instance, and set its value to the result object.
    String name = wrapper->CreateTemporary(NewObjectOf<T>::CreateCopy(&result),
                                           type);

    // The return object to the Tcl interpreter is just a string which
    // refers to an instance.
    Tcl_SetStringObj(Tcl_GetObjResult(wrapper->GetInterpreter()),
                     const_cast<char*>(name.c_str()),
                     name.length());
    
    // Create the command to allow methods to be invoked on the
    // resulting value.
    wrapper->CreateResultCommand(Tcl_GetString(Tcl_GetObjResult(interp)),
                                 type.GetType());
    }
};


/*@{
 * A specialization for returning an object that can be treated as a
 * predefined Tcl object type.
 */
template <>
struct _wrap_EXPORT Return<bool>
{ static void From(bool result, WrapperBase* wrapper); };

template <>
struct _wrap_EXPORT  Return<short>
{ static void From(short result, WrapperBase* wrapper); };

template <>
struct _wrap_EXPORT  Return<unsigned short>
{ static void From(unsigned short result, WrapperBase* wrapper); };

template <>
struct _wrap_EXPORT  Return<int>
{ static void From(int result, WrapperBase* wrapper); };

template <>
struct _wrap_EXPORT  Return<unsigned int>
{ static void From(unsigned int result, WrapperBase* wrapper); };

template <>
struct _wrap_EXPORT  Return<long>
{ static void From(long result, WrapperBase* wrapper); };

template <>
struct _wrap_EXPORT Return<unsigned long>
{ static void From(unsigned long result, WrapperBase* wrapper); };

template <>
struct _wrap_EXPORT  Return<float>
{ static void From(float result, WrapperBase* wrapper); };

template <>
struct _wrap_EXPORT  Return<double>
{ static void From(double result, WrapperBase* wrapper); };
//@}
  
  
/**
 * When a method returns a pointer type, the wrapper function calls this to
 * prepare the return.  This creates the PointerType Tcl object for the
 * Tcl object result.
 */
template <typename T>
struct ReturnPointerTo
{
  static void From(T* result, WrapperBase* wrapper)
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
 * A specialization of ReturnPointerTo for char* to convert to a Tcl String
 * object.
 */
template <>
struct _wrap_EXPORT ReturnPointerTo<char>
{ static void From(char* result, WrapperBase* wrapper); };


/**
 * A specialization of ReturnPointerTo for const char* to convert to a
 * Tcl String object.
 */
template <>
struct _wrap_EXPORT ReturnPointerTo<const char>
{ static void From(const char* result, WrapperBase* wrapper); };


/**
 * When a method returns a reference type, the wrapper function calls this to
 * prepare the return.  This creates the ReferenceType Tcl object for the
 * Tcl object result.
 */
template <typename T>
struct ReturnReferenceTo
{
  static void From(T& result, WrapperBase* wrapper)
    {
    Tcl_Interp* interp = wrapper->GetInterpreter();
    CvQualifiedType referencedType = wrapper->GetType(TypeInfo<T>::name);
    
    // Set the Tcl result to a ReferenceType object representing this
    // reference return value.
    Tcl_SetReferenceObj(Tcl_GetObjResult(interp),
                        Reference(const_cast<T*>(&result), referencedType));
    
    // Create the command to allow methods to be invoked on the
    // resulting value.
    wrapper->CreateResultCommand(Tcl_GetString(Tcl_GetObjResult(interp)),
                                 referencedType.GetType());
    }
};

} // namespace _wrap_

#endif
