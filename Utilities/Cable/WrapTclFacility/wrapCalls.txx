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
 */
template <typename T>
T ArgumentAsInstanceOf<T>::operator()(const Argument& argument)
{
  // Try to find a conversion.
  if(this->FindConversionFunction(argument.GetType()))
    {
    // No conversion is needed.  It is safe do to an identity cast.
    return *reinterpret_cast<T*>(argument.GetValue().object);
    }    
  
  // Perform the conversion and return the result.
  return (reinterpret_cast<T(*)(Anything)>(m_ConversionFunction))(argument.GetValue());
}


/**
 * Convert the given Argument to a pointer to T.
 */
template <typename T>
T* ArgumentAsPointerTo<T>::operator()(const Argument& argument)
{
  // Try to find a conversion.
  if(this->FindConversionFunction(argument.GetType()))
    {
    // No conversion is needed.  It is safe do to an identity cast.
    return reinterpret_cast<T*>(argument.GetValue().object);
    }
    
  // Perform the conversion and return the result.
  return (reinterpret_cast<T*(*)(Anything)>(m_ConversionFunction))(argument.GetValue());
}


/**
 * Convert the given Argument to a pointer to T.  This version can optionally
 * point to an array of T.
 */
template <typename T>
T* ArgumentAsPointerTo_array<T>::operator()(const Argument& argument)
{
  CvQualifiedType from = argument.GetType();
  
  // Try to find a conversion.
  if(this->FindConversionFunction(from))
    {
    // No conversion is needed.  It is safe do to an identity cast.
    return reinterpret_cast<T*>(argument.GetValue().object);
    }
  else if(m_NeedArray)
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
    
  // Perform the conversion and return the result.
  return (reinterpret_cast<T*(*)(Anything)>(m_ConversionFunction))(argument.GetValue());
}


/**
 * Convert the given Argument to a pointer to function T.
 */
template <typename T>
T* ArgumentAsPointerToFunction<T>::operator()(const Argument& argument)
{
  // Try to find a conversion.
  if(this->FindConversionFunction(argument.GetType()))
    {
    // No conversion is needed.  It is safe do to an identity cast.
    return reinterpret_cast<T*>(argument.GetValue().function);
    }
    
  // Perform the conversion and return the result.
  return (reinterpret_cast<T*(*)(Anything)>(m_ConversionFunction))(argument.GetValue());
}


/**
 * Convert the given Argument to a reference to T.
 */
template <typename T>
T& ArgumentAsReferenceTo<T>::operator()(const Argument& argument)
{
  // Try to find a conversion.
  if(this->FindConversionFunction(argument.GetType()))
    {
    // No conversion is needed.  It is safe do to an identity cast.
    return *reinterpret_cast<T*>(argument.GetValue().object);
    }
    
  // Perform the conversion and return the result.  Note that
  // conversion functions that are supposed to return a reference
  // actually return a pointer since the only difference in their
  // implementations would be an extra dereference.  We instead add
  // the dereference here.
  return *((reinterpret_cast<T*(*)(Anything)>(m_ConversionFunction))(argument.GetValue()));
}


/**
 * A function object to convert an Argument to a reference to const T.
 * In this case, a temporary may be constructed that must persist throughout
 * the call to a wrapped function.
 */
template <typename T>
const T& ArgumentAsReferenceTo_const<T>::operator()(const Argument& argument)
{
  // Try to find a conversion.
  if(this->FindConversionFunction(argument.GetType()))
    {
    // No conversion is needed.  It is safe do to an identity cast.
    return *reinterpret_cast<T*>(argument.GetValue().object);
    }
    
  if(m_NeedTemporary)
    {
    // Allocate a temporary instance with a copy-construction of
    // the result.        
    m_Temporary = new T((reinterpret_cast<T(*)(Anything)>(m_ConversionFunction))(argument.GetValue()));
    return *m_Temporary;
    }

  // Perform the conversion and return the result.  Note that
  // conversion functions that are supposed to return a reference
  // actually return a pointer since the only difference in their
  // implementations would be an extra dereference.  We instead add
  // the dereference here.
  return *((reinterpret_cast<const T*(*)(Anything)>(m_ConversionFunction))(argument.GetValue()));
}


} // namespace _wrap_

#endif
