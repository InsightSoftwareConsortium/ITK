/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapConverters.h
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
#ifndef _wrapConverters_h
#define _wrapConverters_h

#include "wrapAnything.h"

namespace _wrap_
{

/**
 * Conversion functions are defined in the Converter namespace.
 */
namespace Converter
{

// Conversion functions returning objects:

/**
 * A conversion function for the identity conversion of an object.
 */
template <typename To>
struct ObjectIdentity
{
  static To Convert(Anything in)
    {
    return *static_cast<To*>(in.object);
    }
  inline static ConversionFunction GetConversionFunction()
    {
    return reinterpret_cast<ConversionFunction>(&ObjectIdentity::Convert);
    }
};


/**
 * A conversion function for a derived-to-base object conversion.
 */
template <typename From, typename To>
struct ObjectDerivedToBase
{
  static To Convert(Anything in)
    {
    return static_cast<To>(*static_cast<From*>(in.object));
    }
  inline static ConversionFunction GetConversionFunction()
    {
    return reinterpret_cast<ConversionFunction>(&ObjectDerivedToBase::Convert);
    }
};


/**
 * A conversion function for calling a type conversion operator.
 */
template <typename From, typename To>
struct ConversionOperator
{
  static To Convert(Anything in)
    {
    return static_cast<From*>(in.object)->operator To();
    }
  inline static ConversionFunction GetConversionFunction()
    {
    return reinterpret_cast<ConversionFunction>(&ConversionOperator::Convert);
    }
};


/**
 * A conversion function for performing conversion by constructor.
 */
template <typename From, typename To>
struct ConversionByConstructor
{
  static To Convert(Anything in)
    {
    return To(*static_cast<From*>(in.object));
    }
  inline static ConversionFunction GetConversionFunction()
    {
    return reinterpret_cast<ConversionFunction>(&ConversionByConstructor::Convert);
    }
};


/**
 * A conversion function for performing a reinterpret_cast on an object.
 */
template <typename From, typename To>
struct ObjectReinterpret
{
  static To Convert(Anything in)
    {
    return reinterpret_cast<To>(*static_cast<From*>(in.object));
    }
  inline static ConversionFunction GetConversionFunction()
    {
    return reinterpret_cast<ConversionFunction>(&ObjectReinterpret::Convert);
    }
};


/**
 * A bug in GCC 2.95.3 prevents static_cast from adding a const like this:
 * void* v; const int* i = static_cast<const int*>(v);
 * However, we can't use a const_cast to add it because it is not allowed
 * when we aren't really adding a const.  This class creates an inline
 * function to add a const_cast if T is a const type.
 */
namespace CastHack
{
  template <typename> struct Caster { inline static void* Cast(void* v) { return v; } };
#ifdef _wrap_CONST_CAST_HACK
  template <typename T> struct Caster<const T> { inline static const void* Cast(void* v) { return const_cast<const void*>(v); } };
#endif
} // namespace CastHack


// Conversion functions returning pointers:

/**
 * A conversion function for pointer identity.
 */
template <typename To>
struct PointerIdentity
{
  static To* Convert(Anything in)
    {
    // This should be
    // return static_cast<To*>(in.object);
    // but GCC doesn't like static_cast to add a const qualifier.
    return static_cast<To*>(CastHack::Caster<To>::Cast(in.object));
    }
  inline static ConversionFunction GetConversionFunction()
    {
    return reinterpret_cast<ConversionFunction>(&PointerIdentity::Convert);
    }
};


/**
 * A conversion function for a derived-to-base pointer conversion.
 */
template <typename From, typename To>
struct PointerDerivedToBase
{
  static To* Convert(Anything in)
    {
    return static_cast<To*>(static_cast<From*>(in.object));
    }
  inline static ConversionFunction GetConversionFunction()
    {
    return reinterpret_cast<ConversionFunction>(&PointerDerivedToBase::Convert);
    }
};


/**
 * A conversion function for pointers to functions.
 */
template <typename To>
struct FunctionPointer
{
  static To Convert(Anything in)
    {
    return reinterpret_cast<To>(in.function);
    }
  inline static ConversionFunction GetConversionFunction()
    {
    return reinterpret_cast<ConversionFunction>(&FunctionPointer::Convert);
    }
};


// Conversion functions returning references:

/**
 * A conversion function for reference identity.
 */
template <typename To>
struct ReferenceIdentity
{
  static To& Convert(Anything in)
    {
    // This should be
    // return *static_cast<To*>(in.object);
    // but GCC doesn't like static_cast to add a const qualifier.
    return *static_cast<To*>(CastHack::Caster<To>::Cast(in.object));
    }
  inline static ConversionFunction GetConversionFunction()
    {
    return reinterpret_cast<ConversionFunction>(&ReferenceIdentity::Convert);
    }
};


/**
 * A conversion function for a derived-to-base reference conversion.
 */
template <typename From, typename To>
struct ReferenceDerivedToBase
{
  static To& Convert(Anything in)
    {
    return static_cast<To&>(*static_cast<From*>(in.object));
    }
  inline static ConversionFunction GetConversionFunction()
    {
    return reinterpret_cast<ConversionFunction>(&ReferenceDerivedToBase::Convert);
    }
};


} // namespace Converter


/**
 * A function to actually call the given ConversionFunction on the given
 * object to produce the given output type.
 */
template <class T>
struct ConvertTo
{
  inline static T From(Anything anything, ConversionFunction cf)
    {
    return (reinterpret_cast<T(*)(Anything)>(cf))(anything);
    }
};


/**
 * A function to actually call the given ConversionFunction on the given
 * object to produce the given output type.  This version handles conversion
 * functions that produce a temporary.
 * This returns a pointer to the temporary.
 */
template <class T>
struct ConvertToTemporaryOf
{
  inline static const T* From(Anything anything, ConversionFunction cf)
    {
    return new T(ConvertTo<T>::From(anything, cf));
    }
};

} // namespace _wrap_

#endif
