/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapConversionTable.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _wrapConversionTable_h
#define _wrapConversionTable_h

#include "wrapUtils.h"
#include "wrapTypeInfo.h"
#include "wrapException.h"
#include <map>

namespace _wrap_
{


/**
 * The general argument to a conversion function.
 * Casts between data and function pointers are not allowed.  Therfore,
 * we need to represent a general object as a class type.  Since only one
 * of the pointers is needed at a time, it can be a union.
 */
union Anything
{
  typedef void* ObjectType;
  typedef void (*FunctionType)();
  ObjectType   object;
  FunctionType function;
};


/**
 * The general type of a conversion function.  A real conversion function
 * will return something, but they are all cast to this for storage
 * in the table.
 */
typedef void (*ConversionFunction)(Anything);


/**
 * A class to maintain a table of conversion functions for a Tcl interpreter.
 *
 * The table is keyed on the "from" and "to" types.  The top level
 * cv-qualifiers on the "from" type can affect the conversion, but
 * those on the "to" type cannot.
 *
 * 8.3.5/3 Top level cv-qualifiers on target type never matter for
 * conversions.  They only affect the parameter inside the function body.
 */
class _wrap_EXPORT ConversionTable
{
public:  
  ConversionTable();
  bool Exists(const CvQualifiedType& from, const Type* to) const;
  void SetConversion(const CvQualifiedType& from,
                     const Type* to, ConversionFunction);
  ConversionFunction GetConversion(const CvQualifiedType& from,
                                   const Type* to) const;
  
private:
  void InitializePredefinedConversions();
  
  typedef std::pair<CvQualifiedType, const Type*> ConversionKey;
  typedef std::map<ConversionKey, ConversionFunction> ConversionMap;

  /**
   * Map from type in/out pair to conversion function.
   */
  ConversionMap m_ConversionMap;
public:
  static ConversionTable* GetForInterpreter(Tcl_Interp*);
  
private:
  typedef std::map<const Tcl_Interp*, ConversionTable*>  InterpreterConversionTableMap;
  static InterpreterConversionTableMap interpreterConversionTableMap;  
};


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
