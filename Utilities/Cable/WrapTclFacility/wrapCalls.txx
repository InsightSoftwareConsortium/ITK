/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapCalls.txx
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
#ifndef _wrapCalls_txx
#define _wrapCalls_txx

namespace _wrap_
{

/**
 * Convert the given Argument to an object of T.
 * Get the wrapper's conversion function from the Argument's type to T.
 * If none exists, an exception is thrown.
 */
template <typename T>
T ArgumentAsInstanceOf<T>::operator()(const Argument& argument)
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
    cf = m_Wrapper->GetConversionFunction(from, to);
    // If not, we don't know how to do the conversion.
    if(!cf)
      {
      throw _wrap_UnknownConversionException(from.GetName(), to->Name());
      }
    }
  
  // Perform the conversion and return the result.
  return ConvertTo<T>::From(argument.GetValue(), cf);
}


/**
 * Convert the given Argument to a pointer to T.
 */
template <typename T>
T* ArgumentAsPointerTo<T>::operator()(const Argument& argument)
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
    cf = m_Wrapper->GetConversionFunction(from, to);
    // If not, we don't know how to do the conversion.
    if(!cf)
      {
      throw _wrap_UnknownConversionException(from.GetName(), to->Name());
      }
    }
    
  // Perform the conversion and return the result.
  return ConvertTo<T*>::From(argument.GetValue(), cf);
}


/**
 * Convert the given Argument to a pointer to T.  This version can optionally
 * point to an array of T.
 */
template <typename T>
T* ArgumentAsPointerTo_array<T>::operator()(const Argument& argument)
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
  else if(from.IsArrayType()
          && (ArrayType::SafeDownCast(from.GetType())->GetElementType() == CvType<T>::type))
    {
    const Argument* elements = reinterpret_cast<const Argument*>(argument.GetValue().object);
    unsigned long length = ArrayType::SafeDownCast(from.GetType())->GetLength();
    m_Array = new NoCvT[length];
    for(unsigned int i = 0; i < length; ++i)
      {
      m_Array[i] = ElementFor(m_Wrapper)(elements[i]);
      }
    return m_Array;
    }
  else
    {
    // We don't have a trivial conversion.  Try to lookup the
    // conversion function.
    cf = m_Wrapper->GetConversionFunction(from, to);
    // If not, we don't know how to do the conversion.
    if(!cf)
      {
      throw _wrap_UnknownConversionException(from.GetName(), to->Name());
      }
    }
    
  // Perform the conversion and return the result.
  return ConvertTo<T*>::From(argument.GetValue(), cf);
}


/**
 * Convert the given Argument to a pointer to function T.
 */
template <typename T>
T* ArgumentAsPointerToFunction<T>::operator()(const Argument& argument)
{
  // 8.3.5/3 Top level cv-qualifiers on target type never matter for
  // conversions.  They only affect the parameter inside the function body.
  const PointerType* to =
    PointerType::SafeDownCast(CvType<T>::type.GetType());
    
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
    cf = Converter::FunctionPointer<T*>::GetConversionFunction();
    }
  else
    {
    // We don't have a trivial conversion.  Try to lookup the
    // conversion function.
    cf = m_Wrapper->GetConversionFunction(from, to);
    // If not, we don't know how to do the conversion.
    if(!cf)
      {
      throw _wrap_UnknownConversionException(from.GetName(), to->Name());
      }
    }
    
  // Perform the conversion and return the result.
  return ConvertTo<T*>::From(argument.GetValue(), cf);
}


/**
 * Convert the given Argument to a reference to T.
 */
template <typename T>
T& ArgumentAsReferenceTo<T>::operator()(const Argument& argument)
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
    cf = m_Wrapper->GetConversionFunction(from, to);
    }
  else
    {
    // We don't have a trivial conversion.  Try to lookup the
    // conversion function.
    cf = m_Wrapper->GetConversionFunction(from, to);
    }
  // Make sure we know how to do the conversion.
  if(!cf)
    {
    throw _wrap_UnknownConversionException(from.GetName(), to->Name());
    }
    
  // Perform the conversion and return the result.
  return ConvertTo<T&>::From(argument.GetValue(), cf);
}


/**
 * A function object to convert an Argument to a reference to const T.
 * In this case, a temporary may be constructed that must persist throughout
 * the call to a wrapped function.
 */
template <typename T>
const T& ArgumentAsReferenceTo_const<T>::operator()(const Argument& argument)
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
  
} // namespace _wrap_

#endif
