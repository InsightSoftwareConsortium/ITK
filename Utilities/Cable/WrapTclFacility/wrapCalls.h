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
  static void From(const T& result, const WrapperBase* wrapper)
    {
    Tcl_Interp* interp = wrapper->GetInterpreter();
    
    // Create a temporary instance, and set its value to the result object.
    String name = wrapper->CreateTemporary(NewObjectOf<T>::CreateCopy(&result),
                                           CvType<T>::type);

    // The return object to the Tcl interpreter is just a string which
    // refers to an instance.
    Tcl_SetStringObj(Tcl_GetObjResult(wrapper->GetInterpreter()),
                     const_cast<char*>(name.c_str()),
                     name.length());
    
    // Create the command to allow methods to be invoked on the
    // resulting value.
    wrapper->CreateResultCommand(Tcl_GetString(Tcl_GetObjResult(interp)),
                                 CvType<T>::type.GetType());
    }
};


/**
 * A specialization for a method with no return type.
 */
template <>
struct _wrap_EXPORT Return<void>
{ static void From(const WrapperBase* wrapper); };


/*@{
 * A specialization for returning an object that can be treated as a
 * predefined Tcl object type.
 */
template <>
struct _wrap_EXPORT Return<bool>
{ static void From(bool result, const WrapperBase* wrapper); };

template <>
struct _wrap_EXPORT  Return<short>
{ static void From(short result, const WrapperBase* wrapper); };

template <>
struct _wrap_EXPORT  Return<unsigned short>
{ static void From(unsigned short result, const WrapperBase* wrapper); };

template <>
struct _wrap_EXPORT  Return<int>
{ static void From(int result, const WrapperBase* wrapper); };

template <>
struct _wrap_EXPORT  Return<unsigned int>
{ static void From(unsigned int result, const WrapperBase* wrapper); };

template <>
struct _wrap_EXPORT  Return<long>
{ static void From(long result, const WrapperBase* wrapper); };

template <>
struct _wrap_EXPORT Return<unsigned long>
{ static void From(unsigned long result, const WrapperBase* wrapper); };

template <>
struct _wrap_EXPORT  Return<float>
{ static void From(float result, const WrapperBase* wrapper); };

template <>
struct _wrap_EXPORT  Return<double>
{ static void From(double result, const WrapperBase* wrapper); };
//@}
  
  
/**
 * When a method returns a pointer type, the wrapper function calls this to
 * prepare the return.  This creates the PointerType Tcl object for the
 * Tcl object result.
 */
template <typename T>
struct ReturnPointerTo
{
  static void From(T* result, const WrapperBase* wrapper)
    {
    Tcl_Interp* interp = wrapper->GetInterpreter();
    
    // Set the Tcl result to a PointerType object representing this
    // pointer return value.
    Tcl_SetPointerObj(Tcl_GetObjResult(interp),
                      Pointer(result, CvType<T>::type));
    
    // Create the command to allow methods to be invoked on the
    // resulting value.
    wrapper->CreateResultCommand(Tcl_GetString(Tcl_GetObjResult(interp)),
                                 CvType<T>::type.GetType());
    }
};
 

/**
 * A specialization of ReturnPointerTo for char* to convert to a Tcl String
 * object.
 */
template <>
struct _wrap_EXPORT ReturnPointerTo<char>
{ static void From(char* result, const WrapperBase* wrapper); };


/**
 * A specialization of ReturnPointerTo for const char* to convert to a
 * Tcl String object.
 */
template <>
struct _wrap_EXPORT ReturnPointerTo<const char>
{ static void From(const char* result, const WrapperBase* wrapper); };


/**
 * When a method returns a reference type, the wrapper function calls this to
 * prepare the return.  This creates the ReferenceType Tcl object for the
 * Tcl object result.
 */
template <typename T>
struct ReturnReferenceTo
{
  static void From(T& result, const WrapperBase* wrapper)
    {
    Tcl_Interp* interp = wrapper->GetInterpreter();
    
    // Set the Tcl result to a ReferenceType object representing this
    // reference return value.
    Tcl_SetReferenceObj(Tcl_GetObjResult(interp),
                        Reference(&result, CvType<T>::type));
    
    // Create the command to allow methods to be invoked on the
    // resulting value.
    wrapper->CreateResultCommand(Tcl_GetString(Tcl_GetObjResult(interp)),
                                 CvType<T>::type.GetType());
    }
};


/**
 * Get the wrapper's conversion function from the given type to T.
 * If none exists, an exception is thrown.
 */
template <typename T>
struct ConversionFunctionTo
{
  static ConversionFunction From(const CvQualifiedType& from,
                                 const WrapperBase* wrapper)
    {
    // 8.3.5/3 Top level cv-qualifiers on target type never matter for
    // conversions.  They only affect the parameter inside the function body.
    const Type* to = CvType<T>::type.GetType();
    
    // If the types are the same, use the identity conversion function.
    if(to->Id() == from.GetType()->Id())
      {
      return Converter::ObjectIdentity<T>::GetConversionFunction();
      }
    
    // See if this conversion has been registered.
    ConversionFunction cf = wrapper->GetConversionFunction(from, to);

    // If not, we don't know how to do the conversion.
    if(!cf)
      {
      throw _wrap_UnknownConversionException(from.GetName(), to->Name());
      }
    return cf;
    }
};


/**
 * Determine whether the given Tcl object can be passed as a T parameter.
 */
template <typename T>
struct ObjectCanBe
{
  static bool Test(Tcl_Obj* obj, const WrapperBase* wrapper)
    {
    //    return Conversions::CanConvert(wrapper->GetObjectType(obj),
    //                                   wrapper->GetType(CvType<T>::name));
    return ConversionFunctionTo<T>::From(wrapper->GetObjectType(obj),
                                         wrapper) != NULL;
    }
};


/**
 * Convert the given Tcl object to an object that can be passed as a T
 * parameter.
 */
template <typename T>
struct ObjectAs
{
  static T Get(Tcl_Obj* obj, const WrapperBase* wrapper)
    {
    Tcl_Interp* interp = wrapper->GetInterpreter();
    // First, see if Tcl has given us the type information.
    if(TclObjectTypeIsPointer(obj))
      {
      // The Tcl object stores a Pointer.
      Pointer p;
      Tcl_GetPointerFromObj(interp, obj, &p);
      CvQualifiedType type = p.GetCvQualifiedType();
      ConversionFunction cf = ConversionFunctionTo<T>::From(type, wrapper);
      // When the conversion function dereferences its pointer, it must
      // get a pointer, not the object.
      void* pointer = p.GetObject();
      return ConvertTo<T>::From(&pointer, cf);
      }
    else if(TclObjectTypeIsReference(obj))
      {
      // The Tcl object stores a Reference.
      Reference r;
      Tcl_GetReferenceFromObj(interp, obj, &r);
      CvQualifiedType type = r.GetCvQualifiedType();
      ConversionFunction cf = ConversionFunctionTo<T>::From(type, wrapper);
      // When the conversion function dereferences its pointer, it must
      // get the object referenced.
      return ConvertTo<T>::From(r.GetObject(), cf);
      }
    else if(TclObjectTypeIsBoolean(obj))
      {
      // The Tcl object stores a boolean.
      int i;
      Tcl_GetBooleanFromObj(interp, obj, &i);
      bool b = (i!=0);
      ConversionFunction cf = ConversionFunctionTo<T>::From(CvType<bool>::type, wrapper);
      // When the conversion function dereferences its pointer, it must
      // get the bool object.
      return ConvertTo<T>::From(&b, cf);
      }
    else if(TclObjectTypeIsInt(obj))
      {
      // The Tcl object stores an integer.
      int i;
      Tcl_GetIntFromObj(interp, obj, &i);
      ConversionFunction cf = ConversionFunctionTo<T>::From(CvType<int>::type, wrapper);
      // When the conversion function dereferences its pointer, it must
      // get the int object.
      return ConvertTo<T>::From(&i, cf);
      }
    else if(TclObjectTypeIsDouble(obj))
      {
      // The Tcl object stores a double.
      double d;
      Tcl_GetDoubleFromObj(interp, obj, &d);
      ConversionFunction cf = ConversionFunctionTo<T>::From(CvType<double>::type, wrapper);
      // When the conversion function dereferences its pointer, it must
      // get the double object.
      return ConvertTo<T>::From(&d, cf);
      }
    else
      {
      // Tcl has not given us the type information.  The object cannot be
      // a Pointer or Reference because it would have been converted to one
      // by the ObjectCanBe<T>::From() test from the string representation.
      // It must be either the name of an instance, or just a string.

      // See if it the name of an instance.
      String objectName = Tcl_GetString(obj);
      if(wrapper->InstanceExists(objectName))
        {        
        CvQualifiedType type = wrapper->GetInstanceType(objectName);
        ConversionFunction cf = ConversionFunctionTo<T>::From(type, wrapper);
        // When the conversion function dereferences its pointer, it must
        // get the object.
        return ConvertTo<T>::From(wrapper->GetInstanceObject(objectName), cf);
        }
      else
        {
        // Can't identify the object type.  We will have to assume char*.
        ConversionFunction cf = ConversionFunctionTo<T>::From(CvType<char*>::type, wrapper); 
        // When the conversion function dereferences its pointer, it must
        // get a pointer to char, not a char.
        void* pointer = Tcl_GetString(obj);
        return ConvertTo<T>::From(&pointer, cf);
        }
      }    
    }
};


} // namespace _wrap_

#endif
