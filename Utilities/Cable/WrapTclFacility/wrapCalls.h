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
    String name = wrapper->CreateTemporary(new T(result), CvType<T>::type);

    // The return object to the Tcl interpreter is just a string which
    // refers to an instance.
    Tcl_SetStringObj(Tcl_GetObjResult(wrapper->GetInterpreter()),
                     const_cast<char*>(name.c_str()),
                     name.length());
    
    // Create the command to allow methods to be invoked on the
    // resulting value.
    wrapper->CreateResultCommand(Tcl_GetStringFromObj(Tcl_GetObjResult(interp), NULL),
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
    wrapper->CreateResultCommand(Tcl_GetStringFromObj(Tcl_GetObjResult(interp), NULL),
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
    wrapper->CreateResultCommand(Tcl_GetStringFromObj(Tcl_GetObjResult(interp), NULL),
                                 CvType<T>::type.GetType());
    }
};


/**
 * Convert the given Argument to an object of T.
 * Get the wrapper's conversion function from the Argument's type to T.
 * If none exists, an exception is thrown.
 */
template <typename T>
struct ArgumentAs
{
  static T Get(const WrapperBase::Argument& argument,
               const WrapperBase* wrapper)
    {
    // 8.3.5/3 Top level cv-qualifiers on target type never matter for
    // conversions.  They only affect the parameter inside the function body.
    const Type* to = CvType<T>::type.GetType();
    
    // Get the argument's type from which we must convert.
    CvQualifiedType from = argument.GetType();
    
    // A pointer to the conversion function.
    ConversionFunction cf = NULL;
    
    // If the types are the same, use the identity conversion function.
    if(to->Id() == from.GetType()->Id())
      {
      cf = Converter::ObjectIdentity<T>::GetConversionFunction();
      }
    else
      {
      // We don't have a trivial conversion.  Try to lookup the
      // conversion function.
      cf = wrapper->GetConversionFunction(from, to);
      // If not, we don't know how to do the conversion.
      if(!cf)
        {
        throw _wrap_UnknownConversionException(from.GetName(), to->Name());
        }
      }
    
    // Perform the conversion and return the result.
    return ConvertTo<T>::From(argument.GetValue(), cf);
    }
};


/**
 * Convert the given Argument to a pointer to T.
 */
template <typename T>
struct ArgumentAsPointerTo
{
  static T* Get(const WrapperBase::Argument& argument,
                const WrapperBase* wrapper)
    {
    // 8.3.5/3 Top level cv-qualifiers on target type never matter for
    // conversions.  They only affect the parameter inside the function body.
    const PointerType* to =
      PointerType::SafeDownCast(CvType<T*>::type.GetType());
    
    // Get the argument's type from which we must convert.
    CvQualifiedType from = argument.GetType();
    
    // A pointer to the conversion function.
    ConversionFunction cf = NULL;
    
    // If the "from" type is a pointer and the conversion is a valid
    // cv-qualifier adjustment, use the pointer identity conversion
    // function.
    if(from.GetType()->IsEitherPointerType()
       && Conversions::IsValidQualificationConversion(PointerType::SafeDownCast(from.GetType()),
                                                      PointerType::SafeDownCast(to)))
      {
      cf = Converter::PointerIdentity<T>::GetConversionFunction();
      }
    else
      {
      // We don't have a trivial conversion.  Try to lookup the
      // conversion function.
      cf = wrapper->GetConversionFunction(from, to);
      // If not, we don't know how to do the conversion.
      if(!cf)
        {
        throw _wrap_UnknownConversionException(from.GetName(), to->Name());
        }
      }
    
    // Perform the conversion and return the result.
    return ConvertTo<T*>::From(argument.GetValue(), cf);
    }
};


/**
 * Convert the given Argument to a reference to T.
 */
template <typename T>
class ArgumentAsReferenceTo
{
public:
  static T& Get(const WrapperBase::Argument& argument,
                const WrapperBase* wrapper)
    {
    // 8.3.5/3 Top level cv-qualifiers on target type never matter for
    // conversions.  They only affect the parameter inside the function body.
    const ReferenceType* to =
      ReferenceType::SafeDownCast(CvType<T&>::type.GetType());
    
    // Get the argument's type from which we must convert.
    CvQualifiedType from = argument.GetType();
    
    // If the "from" type is a ReferenceType, dereference it.
    if(from.GetType()->IsReferenceType())
      {
      from = ReferenceType::SafeDownCast(from.GetType())->GetReferencedType();
      }
    
    // A pointer to the conversion function.
    ConversionFunction cf = NULL;

    // If the "to" type is a reference to the "from" type use the
    // reference identity conversion function.
    if(Conversions::ReferenceCanBindAsIdentity(from, to))
      {
      cf = Converter::ReferenceIdentity<T>::GetConversionFunction();
      }
    // See if there is a derived-to-base conversion.
    else if(Conversions::ReferenceCanBindAsDerivedToBase(from, to))
      {
      // TODO: Handle different cv-qualifications.
      cf = wrapper->GetConversionFunction(from, to);
      }
    else
      {
      // We don't have a trivial conversion.  Try to lookup the
      // conversion function.
      cf = wrapper->GetConversionFunction(from, to);
      }
    // Make sure we know how to do the conversion.
    if(!cf)
      {
      throw _wrap_UnknownConversionException(from.GetName(), to->Name());
      }
    
    // Perform the conversion and return the result.
    return ConvertTo<T&>::From(argument.GetValue(), cf);
    } 
};


/**
 * A function object to convert an Argument to a reference to const T.
 * In this case, a temporary may be constructed that must persist throughout
 * the call to a wrapped function.
 */
template <typename T>
class GetArgumentAsReferenceTo_const
{
public:
  GetArgumentAsReferenceTo_const(const WrapperBase* wrapper):
    m_Wrapper(wrapper), m_Temporary(NULL) {}
  ~GetArgumentAsReferenceTo_const()
    { if(m_Temporary) { delete m_Temporary; } }
  
  const T& operator()(const WrapperBase::Argument& argument)
    {
    // 8.3.5/3 Top level cv-qualifiers on target type never matter for
    // conversions.  They only affect the parameter inside the function body.
    const ReferenceType* to =
      ReferenceType::SafeDownCast(CvType<const T&>::type.GetType());
    
    // Get the argument's type from which we must convert.
    CvQualifiedType from = argument.GetType();
    
    // If the "from" type is a ReferenceType, dereference it.
    if(from.GetType()->IsReferenceType())
      {
      from = ReferenceType::SafeDownCast(from.GetType())->GetReferencedType();
      }
    
    // A pointer to the conversion function.
    ConversionFunction cf = NULL;

    // If the "to" type is a reference to the "from" type use the
    // reference identity conversion function.
    if(Conversions::ReferenceCanBindAsIdentity(from, to))
      {
      cf = Converter::ReferenceIdentity<T>::GetConversionFunction();
      }
    // See if there is a derived-to-base conversion.
    else if(Conversions::ReferenceCanBindAsDerivedToBase(from, to))
      {
      // TODO: Handle different cv-qualifications.
      cf = m_Wrapper->GetConversionFunction(from, to);
      }
    else
      {
      // We don't have a trivial conversion.  Try to lookup the
      // conversion function.
      cf = m_Wrapper->GetConversionFunction(from, to);
      if(!cf)
        {
        // There is no direct conversion function.  Try to lookup one
        // that uses a temporary.
        const Type* toType = CvType<T>::type.GetType();
        cf = m_Wrapper->GetConversionFunction(from, toType);
        if(cf)
          {
          m_Temporary = ConvertToTemporaryOf<T>::From(argument.GetValue(), cf);
          return *m_Temporary;
          }
        }
      }
    // Make sure we know how to do the conversion.
    if(!cf)
      {
      throw _wrap_UnknownConversionException(from.GetName(), to->Name());
      }
    
    // Perform the conversion and return the result.
    return ConvertTo<const T&>::From(argument.GetValue(), cf);
    }
  
private:
  /**
   * The Wrapper for which this is handling an argument.
   */
  const WrapperBase* m_Wrapper;
  
  /**
   * If a temporary of type const T is needed, this will point to it.
   */
  const T* m_Temporary;
};


} // namespace _wrap_

#endif
