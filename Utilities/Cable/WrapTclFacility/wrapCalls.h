/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapCalls.h
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
#ifndef _wrapCalls_h
#define _wrapCalls_h

#include "wrapWrapperBase.h"
#include "wrapConstructor.h"
#include "wrapMethod.h"
#include "wrapStaticMethod.h"
#include "wrapTclCxxObject.h"
#include "wrapConversionTable.h"
#include "wrapWrapperFacility.h"
#include "wrapConverters.h"
#include "wrapException.h"
#include "wrapTypeInfo.h"

namespace _wrap_
{

namespace
{
/**
 * Every type invovled in a wrapper should have a specialization of this
 * class with the following members:
 *   static CvQualifiedType type;
 *   typedef T NoCv;  // Type T without any top-level cv-qualifiers.
 * And if the type can be passed as an argument to a function:
 *   typedef ArgumentAs...<T> ArgumentFor;
 */
template <class T>
struct CvType;

/**
 * Every enumeration type that is returned from a method should have a
 * specialization of this class the following member:
 *   static void From(const T& result, const WrapperBase* wrapper);
 */
template <typename T>
struct ReturnEnum;
} // anonymous namespace

/**
 * When a method returns an object, the wrapper function calls this to
 * prepare the return.  This creates the instance, sets the Tcl object
 * result, and creates the Tcl command in case of nested command
 * calls.
 */
template <typename T>
struct Return
{
  typedef typename CvType<T>::NoCv NoCvT;
  static void From(const T& result, const WrapperBase* wrapper)
    {
    Tcl_Interp* interp = wrapper->GetInterpreter();
    CxxObject* cxxObject =
      CxxObject::GetObjectFor(Anything(new NoCvT(result)),
                              CvType<T>::type.GetType(),
                              wrapper->GetWrapperFacility());
    Tcl_SetObjResult(interp, Tcl_NewCxxObjectObj(cxxObject));
    }
  static void FromConstructor(T* result, const WrapperBase* wrapper)
    {
    Tcl_Interp* interp = wrapper->GetInterpreter();
    CxxObject* cxxObject =
      CxxObject::GetObjectFor(Anything(result),
                              CvType<T>::type.GetType(),
                              wrapper->GetWrapperFacility());
    Tcl_SetObjResult(interp, Tcl_NewCxxObjectObj(cxxObject));
    }
};


/**
 * A specialization for a method with no return type.
 */
template <>
struct Return<void>
{ static _wrap_EXPORT void From(const WrapperBase* wrapper); };


/*@{
 * A specialization for returning an object that can be treated as a
 * predefined Tcl object type.
 */
template <>
struct Return<bool>
{ static _wrap_EXPORT void From(bool result, const WrapperBase* wrapper); };

template <>
struct  Return<short>
{ static _wrap_EXPORT void From(short result, const WrapperBase* wrapper); };

template <>
struct  Return<unsigned short>
{ static _wrap_EXPORT void From(unsigned short result, const WrapperBase* wrapper); };

template <>
struct  Return<int>
{ static _wrap_EXPORT void From(int result, const WrapperBase* wrapper); };

template <>
struct  Return<unsigned int>
{ static _wrap_EXPORT void From(unsigned int result, const WrapperBase* wrapper); };

template <>
struct  Return<long>
{ static _wrap_EXPORT void From(long result, const WrapperBase* wrapper); };

template <>
struct Return<unsigned long>
{ static _wrap_EXPORT void From(unsigned long result, const WrapperBase* wrapper); };

template <>
struct  Return<float>
{ static _wrap_EXPORT void From(float result, const WrapperBase* wrapper); };

template <>
struct  Return<double>
{ static _wrap_EXPORT void From(double result, const WrapperBase* wrapper); };
//@}
  
  
/**
 * When a method returns a pointer type, the wrapper function calls this to
 * prepare the return.  This creates the Pointer Tcl object for the
 * Tcl object result.
 */
template <typename T>
struct ReturnPointerTo
{
  static void From(T* result, const WrapperBase* wrapper)
    {
    Tcl_Interp* interp = wrapper->GetInterpreter();
    CxxObject* cxxObject = 
      CxxObject::GetObjectFor(Anything(result),
                              CvType<T*>::type.GetType(),
                              wrapper->GetWrapperFacility());
    Tcl_SetObjResult(interp, Tcl_NewCxxObjectObj(cxxObject));
    }
};
 

/**
 * A specialization of ReturnPointerTo for char* to convert to a Tcl String
 * object.
 */
template <>
struct ReturnPointerTo<char>
{ static _wrap_EXPORT void From(char* result, const WrapperBase* wrapper); };


/**
 * A specialization of ReturnPointerTo for const char* to convert to a
 * Tcl String object.
 */
template <>
struct ReturnPointerTo<const char>
{ static _wrap_EXPORT void From(const char* result, const WrapperBase* wrapper); };


/**
 * When a method returns a reference type, the wrapper function calls this to
 * prepare the return.  This creates the Reference Tcl object for the
 * Tcl object result.
 */
template <typename T>
struct ReturnReferenceTo
{
  static void From(T& result, const WrapperBase* wrapper)
    {
    Tcl_Interp* interp = wrapper->GetInterpreter();
    CxxObject* cxxObject =
      CxxObject::GetObjectFor(Anything(&result),
                              CvType<T&>::type.GetType(),
                              wrapper->GetWrapperFacility());
    Tcl_SetObjResult(interp, Tcl_NewCxxObjectObj(cxxObject));
    }
};


/*@{
 * A specialization for returning a reference to a const object that
 * can be treated as a predefined Tcl object type.
 */
template <>
struct ReturnReferenceTo<const bool>
{ static _wrap_EXPORT void From(const bool& result, const WrapperBase* wrapper); };

template <>
struct  ReturnReferenceTo<const short>
{ static _wrap_EXPORT void From(const short& result, const WrapperBase* wrapper); };

template <>
struct  ReturnReferenceTo<const unsigned short>
{ static _wrap_EXPORT void From(const unsigned short& result, const WrapperBase* wrapper); };

template <>
struct  ReturnReferenceTo<const int>
{ static _wrap_EXPORT void From(const int& result, const WrapperBase* wrapper); };

template <>
struct  ReturnReferenceTo<const unsigned int>
{ static _wrap_EXPORT void From(const unsigned int& result, const WrapperBase* wrapper); };

template <>
struct  ReturnReferenceTo<const long>
{ static _wrap_EXPORT void From(const long& result, const WrapperBase* wrapper); };

template <>
struct ReturnReferenceTo<const unsigned long>
{ static _wrap_EXPORT void From(const unsigned long& result, const WrapperBase* wrapper); };

template <>
struct  ReturnReferenceTo<const float>
{ static _wrap_EXPORT void From(const float& result, const WrapperBase* wrapper); };

template <>
struct  ReturnReferenceTo<const double>
{ static _wrap_EXPORT void From(const double& result, const WrapperBase* wrapper); };
//@}


/**
 * Base class for ArgumentAsInstanceOf to implement non-templated
 * portion of the functor.
 */
class _wrap_EXPORT ArgumentAsInstanceBase
{
protected:
  ArgumentAsInstanceBase(const WrapperBase* wrapper, const Type* type):
    m_Wrapper(wrapper),
    m_ConversionFunction(0),
    m_To(type) {}

  bool FindConversionFunction(const CvQualifiedType& from);
  
  ///! The Wrapper for which this is handling an argument.
  const WrapperBase* m_Wrapper;
  
  ///! The target type to which a conversion is being performed.
  const Type* m_To;
  
  ///! A pointer to the conversion function if it was found.
  ConversionFunction m_ConversionFunction;
};


/**
 * Base class for ArgumentAsPointerTo and ArgumentAsPointerToFunction
 * to implement non-templated portion of the functors.
 */
class _wrap_EXPORT ArgumentAsPointerBase
{
protected:
  ArgumentAsPointerBase(const WrapperBase* wrapper, const Type* type):
    m_Wrapper(wrapper),
    m_ConversionFunction(0),
    m_To(PointerType::SafeDownCast(type)) {}

  bool FindConversionFunction(const CvQualifiedType& from);
  
  ///! The Wrapper for which this is handling an argument.
  const WrapperBase* m_Wrapper;
  
  ///! The target type to which a conversion is being performed.
  const PointerType* m_To;
  
  ///! A pointer to the conversion function if it was found.
  ConversionFunction m_ConversionFunction;
};


/**
 * Base class for ArgumentAsPointerTo_array to implement non-templated
 * portion of the functor.
 */
class _wrap_EXPORT ArgumentAsPointerBase_array
{
protected:
  ArgumentAsPointerBase_array(const WrapperBase* wrapper, const Type* type,
                              const CvQualifiedType& elementType):
    m_Wrapper(wrapper),
    m_ConversionFunction(0),
    m_To(PointerType::SafeDownCast(type)),
    m_ElementType(elementType),
    m_NeedArray(false) {}

  bool FindConversionFunction(const CvQualifiedType& from);
  
  ///! The Wrapper for which this is handling an argument.
  const WrapperBase* m_Wrapper;
  
  ///! The target type to which a conversion is being performed.
  const PointerType* m_To;
  
  ///! A pointer to the conversion function if it was found.
  ConversionFunction m_ConversionFunction;
  
  ///! If a temporary array is needed, this is its element type.
  CvQualifiedType m_ElementType;
  
  ///! Flag for whether a temporary array must be allocated.
  bool m_NeedArray;
};


/**
 * Base class for ArgumentAsReferenceTo to implement non-templated
 * portion of the functor.
 */
class _wrap_EXPORT ArgumentAsReferenceBase
{
protected:
  ArgumentAsReferenceBase(const WrapperBase* wrapper, const Type* type):
    m_Wrapper(wrapper),
    m_ConversionFunction(0),
    m_To(ReferenceType::SafeDownCast(type)) {}

  bool FindConversionFunction(const CvQualifiedType& from);
  bool FindDirectConversionFunction(const CvQualifiedType& from);
  
  ///! The Wrapper for which this is handling an argument.
  const WrapperBase* m_Wrapper;
  
  ///! The target type to which a conversion is being performed.
  const ReferenceType* m_To;
  
  ///! A pointer to the conversion function if it was found.
  ConversionFunction m_ConversionFunction;
};


/**
 * Base class for ArgumentAsReferenceTo_const to implement
 * non-templated portion of the functor.
 */
class _wrap_EXPORT ArgumentAsReferenceBase_const:
  public ArgumentAsReferenceBase
{
protected:
  ArgumentAsReferenceBase_const(const WrapperBase* wrapper, const Type* type,
                                const Type* tempType):
    ArgumentAsReferenceBase(wrapper, type),
    m_TempType(tempType),
    m_NeedTemporary(false) {}

  bool FindConversionFunction(const CvQualifiedType& from);

  ///! The type of a temporary if it is allocated.
  const Type* m_TempType;
  
  ///! Flag for whether a temporary is needed to do the conversion.
  bool m_NeedTemporary;
};


/**
 * Functor to convert an Argument to an object of T.
 */
template <typename T>
class ArgumentAsInstanceOf: public ArgumentAsInstanceBase
{
public:
  ArgumentAsInstanceOf(const WrapperBase* wrapper):
    ArgumentAsInstanceBase(wrapper, CvType<T>::type.GetType()) {}
  T operator()(const Argument&);
};


/**
 * Functor to convert an Argument to a pointer to T.
 */
template <typename T>
class ArgumentAsPointerTo: public ArgumentAsPointerBase
{
public:
  ArgumentAsPointerTo(const WrapperBase* wrapper):
    ArgumentAsPointerBase(wrapper, CvType<T*>::type.GetType()) {}
  T* operator()(const Argument&);
};


/**
 * Functor to convert an Argument to a pointer to T.  This version can
 * optionally point to an array of T.
 */
template <typename T>
class ArgumentAsPointerTo_array: public ArgumentAsPointerBase_array
{
public:
  ArgumentAsPointerTo_array(const WrapperBase* wrapper):
    ArgumentAsPointerBase_array(wrapper,
                                CvType<T*>::type.GetType(),
                                CvType<T>::type),
    m_Array(0) {}  
  ~ArgumentAsPointerTo_array() { if(m_Array) { delete [] m_Array; } }
  T* operator()(const Argument&);
private:  
  typedef typename CvType<T>::NoCv NoCvT;
  typedef typename CvType<T>::ArgumentFor ElementFor;
  
  ///! If the pointer is to an array, this holds it.
  NoCvT* m_Array;
};



/**
 * Functor to convert an Argument to a pointer to function of type T.
 */
template <typename T>
class ArgumentAsPointerToFunction: public ArgumentAsPointerBase
{
public:
  ArgumentAsPointerToFunction(const WrapperBase* wrapper):
    ArgumentAsPointerBase(wrapper, CvType<T*>::type.GetType()) {}
  T* operator()(const Argument&);
};


/**
 * Functor to convert an Argument to a reference to T.
 */
template <typename T>
class ArgumentAsReferenceTo: public ArgumentAsReferenceBase
{
public:
  ArgumentAsReferenceTo(const WrapperBase* wrapper):
    ArgumentAsReferenceBase(wrapper, CvType<T&>::type.GetType()) {}
  T& operator()(const Argument&);
};


/**
 * Functor to convert an Argument to a reference to const T.
 * This version is separate from ArgumentAsReferenceTo since a temporary
 * may be constructed.
 */
template <typename T>
class ArgumentAsReferenceTo_const: public ArgumentAsReferenceBase_const
{
public:
  ArgumentAsReferenceTo_const(const WrapperBase* wrapper):
    ArgumentAsReferenceBase_const(wrapper,
                                  CvType<const T&>::type.GetType(),
                                  CvType<T>::type.GetType()),
    m_Temporary(0) {}
  ~ArgumentAsReferenceTo_const()
    { if(m_Temporary) { delete const_cast<T*>(m_Temporary); } }  
  const T& operator()(const Argument&);
private:
  ///! If a temporary of type const T is needed, this will point to it.
  const T* m_Temporary;
};
 
} // namespace _wrap_

#ifndef _wrap_MANUAL_INSTANTIATION
#include "wrapCalls.txx"
#endif
 
#endif
