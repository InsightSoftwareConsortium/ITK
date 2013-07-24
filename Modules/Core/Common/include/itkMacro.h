/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
/**
 * itkMacro.h defines standard system-wide macros, constants, and other
 * parameters. One of its most important functions is to define macros used
 * to interface to instance variables in a standard fashion. For example,
 * these macros manage modified time, debugging information, and provide a
 * standard interface to set and get instance variables.  Macros are
 * available for built-in types; for string classe; vector arrays;
 * object pointers; and debug, warning, and error printout information.
 */

#ifndef __itkMacro_h
#define __itkMacro_h

#include "itkWin32Header.h"
#include "itkConfigure.h"
#include "ITKCommonExport.h"

#include <typeinfo>

#include <string>
#include <cstdlib>
#ifndef NDEBUG
#include <cassert>
#endif

#include <sstream>

/** \namespace itk
 * \brief The "itk" namespace contains all Insight Segmentation and
 * Registration Toolkit (ITK) classes. There are several nested namespaces
 * within the itk:: namespace. */
namespace itk
{
// end namespace itk - this is here for documentation purposes
}

/** A convenience macro marks variables as not being used by a method,
 * avoiding compile-time warnings. */
#define itkNotUsed(x)

/*
 * ITK only supports MSVC++ 7.1 and greater
 * MSVC++ 11.0 _MSC_VER = 1700
 * MSVC++ 10.0 _MSC_VER = 1600
 * MSVC++ 9.0 _MSC_VER = 1500
 * MSVC++ 8.0 _MSC_VER = 1400
 * MSVC++ 7.1 _MSC_VER = 1310
 * MSVC++ 7.0 _MSC_VER = 1300
 * MSVC++ 6.0 _MSC_VER = 1200
 * MSVC++ 5.0 _MSC_VER = 1100
*/
#if defined( _MSC_VER ) && ( _MSC_VER < 1310 )
//#error "_MSC_VER < 1310 (MSVC++ 7.1) not supported under ITKv4"
#endif
#if defined( __SUNPRO_CC ) && ( __SUNPRO_CC < 0x590 )
#error "__SUNPRO_CC < 0x590 not supported under ITKv4"
#endif
#if defined( __CYGWIN__ )
#error "The Cygwin compiler is not supported in ITKv4 and above"
#endif
#if defined( __BORLANDC__ )
#error "The Borland C compiler is not supported in ITKv4 and above"
#endif
#if defined( __MWERKS__ )
#error "The MetroWerks compiler is not supported in ITKv4 and above"
#endif
#if defined( __GNUC__ ) && ( __GNUC__ < 3 )
#error "The __GNUC__ version 2.95 compiler is not supprted under ITKv4 and above"
#if defined( __sgi )
//This is true for IRIX 6.5.18m with MIPSPro 7.3.1.3m.
//TODO: At some future point, it may be necessary to
//define a minimum __sgi version that will work.
#error "The __sgi compiler is not supprted under ITKv4 and above"
#endif
#endif

// Setup symbol exports
// begin legacy
#if defined( _WIN32 ) || defined ( WIN32 )
  #define ITK_ABI_IMPORT __declspec(dllimport)
  #define ITK_ABI_EXPORT __declspec(dllexport)
  #define ITK_ABI_HIDDEN
#else
  #if __GNUC__ >= 4
    #define ITK_ABI_IMPORT __attribute__ ((visibility ("default")))
    #define ITK_ABI_EXPORT __attribute__ ((visibility ("default")))
    #define ITK_ABI_HIDDEN __attribute__ ((visibility ("hidden")))
  #else
    #define ITK_ABI_IMPORT
    #define ITK_ABI_EXPORT
    #define ITK_ABI_HIDDEN
  #endif
#endif
// end legacy

/** Define two object creation methods.  The first method, New(),
 * creates an object from a class, potentially deferring to a factory.
 * The second method, CreateAnother(), creates an object from an
 * instance, potentially deferring to a factory.  This second method
 * allows you to create an instance of an object that is exactly the
 * same type as the referring object.  This is useful in cases where
 * an object has been cast back to a base class.
 *
 * These creation methods first try asking the object factory to create
 * an instance, and then default to the standard "new" operator if the
 * factory fails.
 *
 * These routines assigns the raw pointer to a smart pointer and then call
 * UnRegister() on the rawPtr to compensate for LightObject's constructor
 * initializing an object's reference count to 1 (needed for proper
 * initialization of process objects and data objects cycles).
 *
 * Break the methods into itkSimpleNewMacro and itkCreateAnotherMacro
 * so we can selectively overload CreateAnother() without having to
 * provide a definition for New(). */
#define itkNewMacro(x)                                         \
  itkSimpleNewMacro(x)                                         \
  itkCreateAnotherMacro(x)                                     \
  itkCloneMacro(x)

#define itkSimpleNewMacro(x)                                   \
  static Pointer New(void)                                     \
    {                                                          \
    Pointer smartPtr = ::itk::ObjectFactory< x >::Create();    \
    if ( smartPtr.GetPointer() == NULL )                       \
      {                                                        \
      smartPtr = new x;                                        \
      }                                                        \
    smartPtr->UnRegister();                                    \
    return smartPtr;                                           \
    }

#define itkCreateAnotherMacro(x)                               \
  virtual::itk::LightObject::Pointer CreateAnother(void) const \
    {                                                          \
    ::itk::LightObject::Pointer smartPtr;                      \
    smartPtr = x::New().GetPointer();                          \
    return smartPtr;                                           \
    }

#define itkCloneMacro(x)                                        \
  Pointer Clone() const                             \
  {                                                             \
    Pointer rval =                                  \
      dynamic_cast<x *>(this->InternalClone().GetPointer());    \
    return rval;                                                \
  }

/** Define two object creation methods.  The first method, New(),
 * creates an object from a class but does not defer to a factory.
 * The second method, CreateAnother(), creates an object from an
 * instance, again without deferring to a factory.  This second method
 * allows you to create an instance of an object that is exactly the
 * same type as the referring object.  This is useful in cases where
 * an object has been cast back to a base class.
 *
 * These creation methods first try asking the object factory to create
 * an instance, and then default to the standard "new" operator if the
 * factory fails.
 *
 * These routines assigns the raw pointer to a smart pointer and then call
 * UnRegister() on the rawPtr to compensate for LightObject's constructor
 * initializing an object's reference count to 1 (needed for proper
 * initialization of process objects and data objects cycles). */
#define itkFactorylessNewMacro(x)                              \
  static Pointer New(void)                                     \
    {                                                          \
    Pointer smartPtr;                                          \
    x *     rawPtr = new x;                                    \
    smartPtr = rawPtr;                                         \
    rawPtr->UnRegister();                                      \
    return smartPtr;                                           \
    }                                                          \
  virtual::itk::LightObject::Pointer CreateAnother(void) const \
    {                                                          \
    ::itk::LightObject::Pointer smartPtr;                      \
    smartPtr = x::New().GetPointer();                          \
    return smartPtr;                                           \
    }

/** Macro used to add standard methods to all classes, mainly type
 * information. */
#define itkTypeMacro(thisClass, superclass)  \
  virtual const char *GetNameOfClass() const \
    {                                        \
    return #thisClass;                      \
    }

namespace itk
{
/**
 * The following is used to output debug, warning, and error messages.
 * Use a global function which actually calls:
 * OutputWindow::GetInstance()->DisplayText();
 * This is to avoid Object \#include of OutputWindow
 * while OutputWindow \#includes Object. */
extern ITKCommon_EXPORT void OutputWindowDisplayText(const char *);

extern ITKCommon_EXPORT void OutputWindowDisplayErrorText(const char *);

extern ITKCommon_EXPORT void OutputWindowDisplayWarningText(const char *);

extern ITKCommon_EXPORT void OutputWindowDisplayGenericOutputText(const char *);

extern ITKCommon_EXPORT void OutputWindowDisplayDebugText(const char *);
} // end namespace itk

/** This macro is used to print debug (or other information). They are
 * also used to catch errors, etc. Example usage looks like:
 * itkDebugMacro(<< "this is debug info" << this->SomeVariable); */
#if defined( NDEBUG )
#define itkDebugMacro(x)
#define itkDebugStatement(x)
#else
#define itkDebugMacro(x)                                                \
    {                                                                   \
    if ( this->GetDebug() && ::itk::Object::GetGlobalWarningDisplay() ) \
      {                                                                 \
      std::ostringstream itkmsg;                                        \
      itkmsg << "Debug: In " __FILE__ ", line " << __LINE__ << "\n"     \
             << this->GetNameOfClass() << " (" << this << "): " x       \
             << "\n\n";                                                 \
      ::itk::OutputWindowDisplayDebugText( itkmsg.str().c_str() );      \
      }                                                                 \
    }

//The itkDebugStatement is to be used to protect code that is only
//used in the itkDebugMacro
#define itkDebugStatement(x) x
#endif

/** This macro is used to print warning information (i.e., unusual circumstance
 * but not necessarily fatal.) Example usage looks like:
 * itkWarningMacro(<< "this is warning info" << this->SomeVariable); */
#define itkWarningMacro(x)                                            \
    {                                                                 \
    if ( ::itk::Object::GetGlobalWarningDisplay() )                   \
      {                                                               \
      std::ostringstream itkmsg;                                      \
      itkmsg << "WARNING: In " __FILE__ ", line " << __LINE__ << "\n" \
             << this->GetNameOfClass() << " (" << this << "): " x     \
             << "\n\n";                                               \
      ::itk::OutputWindowDisplayWarningText( itkmsg.str().c_str() );  \
      }                                                               \
    }

//The itkDebugStatement is to be used ot protect code that is only
//used in the itkDebugMacro
#define itkWarningStatement(x) x

#if defined( ITK_CPP_FUNCTION )
  #if defined( _WIN32 ) && !defined( __MINGW32__ ) && !defined( CABLE_CONFIGURATION ) \
  && !defined( CSWIG )
    #define ITK_LOCATION __FUNCSIG__
  #elif defined( __GNUC__ )
    #define ITK_LOCATION __PRETTY_FUNCTION__
  #else
    #define ITK_LOCATION __FUNCTION__
  #endif
#else
  #define ITK_LOCATION "unknown"
#endif

#include "itkExceptionObject.h"

/** The exception macro is used to print error information (i.e., usually
 * a condition that results in program failure). Example usage looks like:
 * itkExceptionMacro(<< "this is error info" << this->SomeVariable); */
#define itkExceptionMacro(x)                                                            \
    {                                                                                   \
    std::ostringstream message;                                                         \
    message << "itk::ERROR: " << this->GetNameOfClass()                                 \
            << "(" << this << "): " x;                                                  \
    ::itk::ExceptionObject e_(__FILE__, __LINE__, message.str().c_str(), ITK_LOCATION); \
    throw e_; /* Explicit naming to work around Intel compiler bug.  */                 \
    }

#define itkGenericExceptionMacro(x)                                                     \
    {                                                                                   \
    std::ostringstream message;                                                         \
    message << "itk::ERROR: " x;                                                        \
    ::itk::ExceptionObject e_(__FILE__, __LINE__, message.str().c_str(), ITK_LOCATION); \
    throw e_; /* Explicit naming to work around Intel compiler bug.  */                 \
    }

#define itkDeclareExceptionMacro(newexcp,parentexcp,whatmessage)                        \
namespace itk {                                                                         \
class newexcp : public parentexcp                                            \
{                                                                                       \
public:                                                                                 \
newexcp( const char *file, unsigned int lineNumber ) :                                  \
parentexcp( file, lineNumber )                                                          \
{                                                                                       \
  this->SetDescription( whatmessage );                                                  \
}                                                                                       \
newexcp( const std::string & file, unsigned int lineNumber ) :                          \
parentexcp( file, lineNumber )                                                          \
{                                                                                       \
  this->SetDescription( whatmessage );                                                  \
}                                                                                       \
itkTypeMacro(newexcp, parentexcp);                                                      \
};                                                                                      \
}

#define itkSpecializedExceptionMacro(exceptiontype)                                     \
    {                                                                                   \
    ::itk::exceptiontype e_(__FILE__, __LINE__);                                        \
    e_.SetLocation(ITK_LOCATION);                                                       \
    throw e_; /* Explicit naming to work around Intel compiler bug.  */                 \
    }

#define itkSpecializedMessageExceptionMacro(exceptiontype,x)                            \
    {                                                                                   \
    ::itk::exceptiontype e_(__FILE__, __LINE__);                                        \
    std::ostringstream message;                                                         \
    message << "itk::ERROR: " x;                                                        \
    e_.SetDescription(message.str().c_str());                                           \
    e_.SetLocation(ITK_LOCATION);                                                       \
    throw e_; /* Explicit naming to work around Intel compiler bug.  */                 \
    }


#define itkGenericOutputMacro(x)                                           \
    {                                                                      \
    if ( ::itk::Object::GetGlobalWarningDisplay() )                        \
      {                                                                    \
      std::ostringstream itkmsg;                                           \
      itkmsg << "WARNING: In " __FILE__ ", line " << __LINE__ << "\n"      \
      x << "\n\n";                                                         \
      ::itk::OutputWindowDisplayGenericOutputText( itkmsg.str().c_str() ); \
      }                                                                    \
    }

//----------------------------------------------------------------------------
// Macros for simplifying the use of logging
//
#define itkLogMacro(x, y)                                \
    {                                                    \
    if ( this->GetLogger() )                             \
      {                                                  \
      this->GetLogger()->Write(::itk::LoggerBase::x, y); \
      }                                                  \
    }

#define itkLogMacroStatic(obj, x, y)                    \
    {                                                   \
    if ( obj->GetLogger() )                             \
      {                                                 \
      obj->GetLogger()->Write(::itk::LoggerBase::x, y); \
      }                                                 \
    }

//----------------------------------------------------------------------------
// Setup legacy code policy.
//
// CMake options ITK_LEGACY_REMOVE and ITK_LEGACY_SILENT are converted
// They may be used to completely remove legacy code or silence the
// warnings.  The default is to warn about their use.
//
// Source files that test the legacy code may define ITK_LEGACY_TEST
// like this:
//
//  #define ITK_LEGACY_TEST
//  #include "itkClassWithDeprecatedMethod.h"
//
// in order to silence the warnings for calling deprecated methods.
// No other source files in ITK should call the methods since they are
// provided only for compatibility with older user code.

// Define itkLegacyMacro to mark legacy methods where they are
// declared in their class.  Example usage:
//
//   // @deprecated Replaced by MyOtherMethod() as of ITK 2.0.
//   itkLegacyMacro(void MyMethod());
#if defined( ITK_LEGACY_REMOVE )
#define itkLegacyMacro(method) /* no ';' */
#elif defined( ITK_LEGACY_SILENT ) || defined( ITK_LEGACY_TEST ) || defined( CSWIG )
// Provide legacy methods with no warnings.
#define itkLegacyMacro(method) method
#else
// Setup compile-time warnings for uses of deprecated methods if
// possible on this compiler.
#if defined( __GNUC__ ) && !defined( __INTEL_COMPILER ) && ( __GNUC__ > 3 || ( __GNUC__ == 3 && __GNUC_MINOR__ >= 1 ) )
#define itkLegacyMacro(method) method __attribute__( ( deprecated ) )
#elif defined( _MSC_VER )
#define itkLegacyMacro(method) __declspec(deprecated) method
#else
#define itkLegacyMacro(method) method
#endif
#endif

// Macros to create runtime deprecation warning messages in function
// bodies.  Example usage:
//
//   void itkMyClass::MyOldMethod()
//     {
//     itkLegacyBodyMacro(itkMyClass::MyOldMethod, 2.0);
//     }
//
//   void itkMyClass::MyMethod()
//     {
//     itkLegacyReplaceBodyMacro(itkMyClass::MyMethod, 2.0,
//                               itkMyClass::MyOtherMethod);
//     }
#if defined( ITK_LEGACY_REMOVE ) || defined( ITK_LEGACY_SILENT )
#define itkLegacyBodyMacro(method, version)
#define itkLegacyReplaceBodyMacro(method, version, replace)
#define itkGenericLegacyBodyMacro(method, version)
#define itkGenericLegacyReplaceBodyMacro(method, version, replace)
#else
#define itkLegacyBodyMacro(method, version) \
  itkWarningMacro(#method " was deprecated for ITK " #version " and will be removed in a future version.")
#define itkLegacyReplaceBodyMacro(method, version, replace)                                                   \
  itkWarningMacro(                                                                                            \
    #method " was deprecated for ITK " #version " and will be removed in a future version.  Use " #replace \
    " instead.")
#define itkGenericLegacyBodyMacro(method, version) \
  itkGenericOutputMacro(#method " was deprecated for ITK " #version " and will be removed in a future version.")
#define itkGenericLegacyReplaceBodyMacro(method, version, replace)                                            \
  itkGenericOutputMacro(                                                                                      \
    #method " was deprecated for ITK " #version " and will be removed in a future version.  Use " #replace \
    " instead.")
#endif

#if defined ( ITK_LEGACY_REMOVE )
#define ITK_TEMPLATE_TXX  "error ITK_TEMPLATE_TXX is no longer a supported identifier, you should replace with ITK_MANUAL_INSTANTIATION as a replacement"
#else
#define ITK_TEMPLATE_TXX 1
#endif


// Most modern x86 CPUs have 64 byte aligned blocks which are used for
// the cache lines. By aligning multi-threaded structures with the
// cache lines, false shared can be reduced, and performance
// increased.
#define ITK_CACHE_LINE_ALIGNMENT 64

//
// itkAlignedTypedef is a macro which creates a new typedef to make a
// data structure aligned.
//
#if defined ( ITK_HAS_CPP11_ALIGNAS )
# define itkAlignedTypedef( alignment, oldtype, newtype )   \
  typedef oldtype newtype alignas(alignment)
#elif defined( ITK_HAS_GNU_ATTRIBUTE_ALIGNED )
# define itkAlignedTypedef( alignment, oldtype, newtype )   \
  typedef oldtype newtype __attribute__((aligned(alignment)))
#elif defined ( _MSC_VER )
# define itkAlignedTypedef( alignment, oldtype, newtype )   \
  typedef __declspec(align( alignment )) struct oldtype newtype
#else
# define itkAlignedTypedef( alignment, oldtype, newtype )        \
  typedef oldtype newtype
#endif

//=============================================================================
/* Define a common way of declaring a templated function as a friend inside a class.
  - ITK_FRIEND_TEMPLATE_FUNCTION_ARGUMENTS(T)

  The following templated function

            template <T>
            T add(const T & a, const T & b);

  is declared as friend in some compilers as:

            class A
              {
              public:
                friend Self add<Self>( const Self & a, const Self & b );
              }

   while other compilers will do

            class A
              {
              public:
                friend Self add<>( const Self & a, const Self & b );
              }

   This characteristic of the compiler is checked by a TRY_COMPILE
   command defined in Insight/CMake/itkTestFriendTemplatedFunction.cxx

*/
#if defined( ITK_SUPPORTS_TEMPLATED_FRIEND_FUNCTION_WITH_NULL_STRING )
#define ITK_FRIEND_TEMPLATE_FUNCTION_ARGUMENT(T)
#else
#if defined( ITK_SUPPORTS_TEMPLATED_FRIEND_FUNCTION_WITH_EMPTY_BRACKETS )
#define ITK_FRIEND_TEMPLATE_FUNCTION_ARGUMENT(T)  < >
#else
#if defined( ITK_SUPPORTS_TEMPLATED_FRIEND_FUNCTION_WITH_TEMPLATE_ARGUMENTS )
#define ITK_FRIEND_TEMPLATE_FUNCTION_ARGUMENT(T)  < T >
#endif
#endif
#endif
// THIS IS A TEMPORARY PATCH FOR Visual Studio 10. The correct solution must
// be implemented in Insight/CMake/itkTestFriendTemplatedFunction.cxx
#if ( defined ( _MSC_VER ) && ( _MSC_VER >= 1600 ) )
#ifdef  ITK_FRIEND_TEMPLATE_FUNCTION_ARGUMENT
#undef  ITK_FRIEND_TEMPLATE_FUNCTION_ARGUMENT
#endif
#define ITK_FRIEND_TEMPLATE_FUNCTION_ARGUMENT(T)
#endif

//=============================================================================
/* Choose a way to prevent template instantiation on this platform.
  - ITK_TEMPLATE_DO_NOT_INSTANTIATE = use #pragma do_not_instantiate to
                                      prevent instantiation
  - ITK_TEMPLATE_EXTERN = use extern template to prevent instantiation
*/
#if defined( __INTEL_COMPILER ) && __INTEL_COMPILER >= 700
#define ITK_TEMPLATE_EXTERN 1
#elif defined( __GNUC__ ) && __GNUC__ >= 3
#define ITK_TEMPLATE_EXTERN 1
#elif defined( _MSC_VER )
#define ITK_TEMPLATE_EXTERN 1
#endif
#if !defined( ITK_TEMPLATE_DO_NOT_INSTANTIATE )
#define ITK_TEMPLATE_DO_NOT_INSTANTIATE 0
#endif
#if !defined( ITK_TEMPLATE_EXTERN )
#define ITK_TEMPLATE_EXTERN 0
#endif

/* Define a macro to explicitly instantiate a template.
  - ITK_TEMPLATE_EXPORT(X) =
      Explicitly instantiate X, where X is of the form N(a1[,a2...,aN]).
      examples: ITK_TEMPLATE_EXPORT(1(class Foo<int>))
                ITK_TEMPLATE_EXPORT(2(class Bar<int, char>))
      Use one level of expansion delay to allow user code to have
      a macro determining the number of arguments. */
#define ITK_TEMPLATE_EXPORT(x) ITK_TEMPLATE_EXPORT_DELAY(x)
#define ITK_TEMPLATE_EXPORT_DELAY(x) template ITK_TEMPLATE_##x;

/* Define a macro to prevent template instantiations.
  - ITK_TEMPLATE_IMPORT(X) =
      Prevent instantiation of X, where X is of the form N(a1[,a2...,aN]).
      examples: ITK_TEMPLATE_IMPORT(1(class Foo<int>))
                ITK_TEMPLATE_IMPORT(2(class Bar<int, char>))
      Use one level of expansion delay to allow user code to have
      a macro determining the number of arguments.
*/
#if ITK_TEMPLATE_EXTERN
#define ITK_TEMPLATE_IMPORT_DELAY(x) extern template ITK_TEMPLATE_##x;
#elif ITK_TEMPLATE_DO_NOT_INSTANTIATE
#define ITK_TEMPLATE_IMPORT_DELAY(x) \
  ITK_TEMPLATE_IMPORT_IMPL(do_not_instantiate ITK_TEMPLATE_##x)
#define ITK_TEMPLATE_IMPORT_IMPL(x) _Pragma(#x)
#endif
#if defined( ITK_TEMPLATE_IMPORT_DELAY )
#define ITK_TEMPLATE_IMPORT(x) ITK_TEMPLATE_IMPORT_DELAY(x)
#define ITK_TEMPLATE_IMPORT_WORKS 1
#else
#define ITK_TEMPLATE_IMPORT(x)
#define ITK_TEMPLATE_IMPORT_WORKS 0
#endif

//=============================================================================

/* Define macros to export and import template instantiations for each
   library in ITK.  */
#define ITK_EXPORT_ITKCommon(c, x, n) \
  ITK_EXPORT_TEMPLATE(ITKCommon_EXPORT, c, x, n)
#define ITK_IMPORT_ITKCommon(c, x, n) \
  ITK_IMPORT_TEMPLATE(ITKCommon_EXPORT, c, x, n)

//--------------------------------------------------------------------------------
//  Helper macros for Template Meta-Programming techniques of for-loops
// unrolling
//--------------------------------------------------------------------------------

//--------------------------------------------------------------------------------
// Macro that generates an unrolled for loop for assigning elements of one array
// to elements of another array The array are assumed to be of same length
// (dimension), and this is also assumed to be the value of NumberOfIterations.
// No verification of size is performed. Casting is perfomed as part of the
// assignment, by using the DestinationElementType as the casting type.
// Source and destination array types must have defined opearator[] in their
// API.
#define itkForLoopAssignmentMacro(DestinationType,                                 \
                                  SourceType,                                      \
                                  DestinationElementType,                          \
                                  DestinationArray,                                \
                                  SourceArray,                                     \
                                  NumberOfIterations)                              \
  for ( unsigned int i = 0; i < NumberOfIterations; ++i )                          \
    {                                                                              \
    DestinationArray[i] = static_cast< DestinationElementType >( SourceArray[i] ); \
    }

//--------------------------------------------------------------------------------
// Macro that generates an unrolled for loop for rounding and assigning
// elements of one array to elements of another array The array are assumed to
// be of same length (dimension), and this is also assumed to be the value of
// NumberOfIterations.  No verification of size is performed. Casting is
// perfomed as part of the assignment, by using the DestinationElementType as
// the casting type.
// Source and destination array types must have defined opearator[] in their
// API.
#define itkForLoopRoundingAndAssignmentMacro(DestinationType,                         \
                                             Sourcrnd_halfintup,                      \
                                             DestinationElementType,                  \
                                             DestinationArray,                        \
                                             SourceArray,                             \
                                             NumberOfIterations)                      \
  for ( unsigned int i = 0; i < NumberOfIterations; ++i )                             \
    {                                                                                 \
    DestinationArray[i] = itk::Math::Round< DestinationElementType >(SourceArray[i]); \
    }

// end of Template Meta Programming helper macros

#ifndef NDEBUG

#ifdef _POSIX_SOURCE
#define itkAssertInDebugOrThrowInReleaseMacro(msg) __assert_fail (msg, __FILE__, __LINE__, __ASSERT_FUNCTION);
#else
#define itkAssertInDebugOrThrowInReleaseMacro(msg) itkGenericExceptionMacro(<< msg);
#endif

#else
#define itkAssertInDebugOrThrowInReleaseMacro(msg) itkGenericExceptionMacro(<< msg);
#endif

#define itkAssertOrThrowMacro(test, message)                       \
  if ( !( test ) )                                                 \
    {                                                              \
    std::ostringstream msgstr;                                     \
    msgstr << message;                                             \
    itkAssertInDebugOrThrowInReleaseMacro( msgstr.str().c_str() ); \
    }

#ifndef NDEBUG
#define itkAssertInDebugAndIgnoreInReleaseMacro(X) assert(X)
#else
#define itkAssertInDebugAndIgnoreInReleaseMacro(X)
#endif

#ifdef ITKV3_COMPATIBILITY
// As of MSVS++ 7.1 and greater, typename is supported in templates
// All ITKv4 compilers support the typename keyword, but this is
// needed to ease transition from ITKv3.
#define ITK_TYPENAME typename
#endif

/** itkDynamicCastInDebugMode
  * Use static_cast in Release builds, and dynamic_cast in Debug
  */
template <typename TTarget, typename TSource>
TTarget itkDynamicCastInDebugMode(TSource x)
{
#ifndef NDEBUG
  if(x == 0)
    {
    return 0;
    }
  TTarget rval = dynamic_cast<TTarget>(x);
  if(rval == 0)
    {
    itkGenericExceptionMacro(<< "Failed dynamic cast to "
                             << typeid(TTarget).name()
                             << " object type = "
                             << x->GetNameOfClass());
    }
  return rval;
#else
  return static_cast<TTarget>(x);
#endif

//  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//  !!  The ITK Get/Set Macros for various types !!
//  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//This is probably better, but requires a lot of extra work
//for gettting ExplicitInstantiation to work properly. \#define
// itkStaticConstMacro(name, type, value) static const type name = value
#define itkStaticConstMacro(name, type, value) enum { name = value }

#define itkGetStaticConstMacro(name) (Self::name)

/** Set an input. This defines the Set"name"() method */
#define itkSetInputMacro(name, type)                                              \
  virtual void Set##name(const type *_arg)                                        \
    {                                                                             \
    itkDebugMacro("setting input " #name " to " << _arg);                         \
    if ( _arg != static_cast< type * >( this->ProcessObject::GetInput(#name) ) )  \
      {                                                                           \
      this->ProcessObject::SetInput( #name, const_cast< type * >( _arg ) );       \
      this->Modified();                                                           \
      }                                                                           \
    }

/** Get an input. This defines the Get"name"() method */
#define itkGetInputMacro(name, type)                                                                            \
  virtual const type * Get##name() const                                                                        \
    {                                                                                                           \
    itkDebugMacro( "returning input " << #name " of "                                                           \
                                      << static_cast< const type * >( this->ProcessObject::GetInput(#name) ) ); \
    return static_cast< const type * >( this->ProcessObject::GetInput(#name) );                                 \
    }

/** Set a decorated input. This defines the Set"name"() and a Set"name"Input() method */
#define itkSetDecoratedInputMacro(name, type)                                                                 \
  virtual void Set##name##Input(const SimpleDataObjectDecorator< type > *_arg)                                \
    {                                                                                                         \
    itkDebugMacro("setting input " #name " to " << _arg);                                                     \
    if ( _arg != static_cast< SimpleDataObjectDecorator< type > * >( this->ProcessObject::GetInput(#name) ) ) \
      {                                                                                                       \
      this->ProcessObject::SetInput( #name, const_cast< SimpleDataObjectDecorator< type > * >( _arg ) );      \
      this->Modified();                                                                                       \
      }                                                                                                       \
    }                                                                                                         \
  virtual void Set##name(const type &_arg)                           \
    {                                                                \
    typedef SimpleDataObjectDecorator< type > DecoratorType;         \
    itkDebugMacro("setting input " #name " to " << _arg);            \
    const DecoratorType *oldInput =                                  \
      static_cast< const DecoratorType * >(                          \
        this->ProcessObject::GetInput(#name) );                      \
    if ( oldInput && oldInput->Get() == _arg )                       \
      {                                                              \
      return;                                                        \
      }                                                              \
    typename DecoratorType::Pointer newInput = DecoratorType::New(); \
    newInput->Set(_arg);                                             \
    this->Set##name##Input(newInput);                                \
    }

/** Set a decorated input. This defines the Set"name"() and Set"name"Input() method */
#define itkGetDecoratedInputMacro(name, type)                                                                 \
  virtual const SimpleDataObjectDecorator< type > * Get##name##Input() const                                                                 \
    {                                                                                                                                        \
    itkDebugMacro( "returning input " << #name " of "                                                                                        \
                                      << static_cast< const SimpleDataObjectDecorator< type > * >( this->ProcessObject::GetInput(#name) ) ); \
    return static_cast< const SimpleDataObjectDecorator< type > * >( this->ProcessObject::GetInput(#name) );                                 \
    }                                                                \
  virtual const type & Get##name() const                             \
    {                                                                \
    itkDebugMacro("Getting input " #name);                           \
    typedef SimpleDataObjectDecorator< type > DecoratorType;         \
    const DecoratorType *input =                                     \
      static_cast< const DecoratorType * >(                          \
        this->ProcessObject::GetInput(#name) );                      \
    if( input == NULL )                                              \
      {                                                              \
      itkExceptionMacro(<<"input" #name " is not set");              \
      }                                                              \
    return input->Get();                                             \
    }

/** Set a decorated input. This defines the Set"name"() and Set"name"Input() method
 * and Get"name" and Get"name"Input methods */
#define itkSetGetDecoratedInputMacro(name, type)  \
  itkSetDecoratedInputMacro(name, type)           \
  itkGetDecoratedInputMacro(name, type)

/** Set a decorated input that derives from itk::Object, but not from
 * itk::DataObject. This defines the Set"name"() and Set"name"Input
 * methods.
 */
#define itkSetDecoratedObjectInputMacro(name, type)          \
  virtual void Set##name##Input(const DataObjectDecorator< type > *_arg)                                \
    {                                                                                                   \
    itkDebugMacro("setting input " #name " to " << _arg);                                               \
    if ( _arg != static_cast< DataObjectDecorator< type > * >( this->ProcessObject::GetInput(#name) ) ) \
      {                                                                                                 \
      this->ProcessObject::SetInput( #name, const_cast< DataObjectDecorator< type > * >( _arg ) );      \
      this->Modified();                                                                                 \
      }                                                                                                 \
    }                                                                                                   \
  virtual void Set##name(const type * _arg)                          \
    {                                                                \
    typedef DataObjectDecorator< type > DecoratorType;               \
    itkDebugMacro("setting input " #name " to " << _arg);            \
    const DecoratorType *oldInput =                                  \
      static_cast< const DecoratorType * >(                          \
        this->ProcessObject::GetInput(#name) );                      \
    if ( oldInput && oldInput->Get() == _arg )                       \
      {                                                              \
      return;                                                        \
      }                                                              \
    typename DecoratorType::Pointer newInput = DecoratorType::New(); \
    newInput->Set(_arg);                                             \
    this->Set##name##Input(newInput);                                \
    }

/** Get a decorated input that derives from itk::Object, but not from
 * itk::DataObject. This defines the Get"name"() and Get"name"Input
 * methods.
 */
#define itkGetDecoratedObjectInputMacro(name, type)          \
  virtual const DataObjectDecorator< type > * Get##name##Input() const                                                                 \
    {                                                                                                                                  \
    itkDebugMacro( "returning input " << #name " of "                                                                                  \
                                      << static_cast< const DataObjectDecorator< type > * >( this->ProcessObject::GetInput(#name) ) ); \
    return static_cast< const DataObjectDecorator< type > * >( this->ProcessObject::GetInput(#name) );                                 \
    }                                                                \
  virtual const type * Get##name() const                             \
    {                                                                \
    itkDebugMacro("Getting input " #name);                           \
    typedef DataObjectDecorator< type > DecoratorType;               \
    const DecoratorType *input =                                     \
      static_cast< const DecoratorType * >(                          \
        this->ProcessObject::GetInput(#name) );                      \
    if( input == NULL )                                              \
      {                                                              \
      itkExceptionMacro(<<"input" #name " is not set");              \
      }                                                              \
    return input->Get();                                             \
    }

/** Set a decorated input. This defines the Set"name"() and Set"name"Input() method
 * and Get"name" and Get"name"Input methods */
#define itkSetGetDecoratedObjectInputMacro(name, type)  \
  itkSetDecoratedObjectInputMacro(name, type)           \
  itkGetDecoratedObjectInputMacro(name, type)

/** Set built-in type.  Creates member Set"name"() (e.g., SetVisibility()); */
#define itkSetMacro(name, type)                      \
  virtual void Set##name (const type _arg)         \
    {                                                \
    itkDebugMacro("setting " #name " to " << _arg); \
    if ( this->m_##name != _arg )                  \
      {                                              \
      this->m_##name = _arg;                       \
      this->Modified();                              \
      }                                              \
    }

/** Get built-in type.  Creates member Get"name"() (e.g., GetVisibility()); */
#define itkGetMacro(name, type)                                       \
  virtual type Get##name ()                                         \
    {                                                                 \
    return this->m_##name;                                          \
    }

/** Get built-in type.  Creates member Get"name"() (e.g., GetVisibility());
 * This is the "const" form of the itkGetMacro.  It should be used unless
 * the member can be changed through the "Get" access routine. */
#define itkGetConstMacro(name, type)                                  \
  virtual type Get##name () const                                   \
    {                                                                 \
    return this->m_##name;                                          \
    }

/** Get built-in type.  Creates member Get"name"() (e.g., GetVisibility());
 * This is the "const" form of the itkGetMacro.  It should be used unless
 * the member can be changed through the "Get" access routine.
 * This versions returns a const reference to the variable. */
#define itkGetConstReferenceMacro(name, type)                         \
  virtual const type &Get##name () const                            \
    {                                                                 \
    return this->m_##name;                                          \
    }

/** Set built-in type.  Creates member Set"name"() (e.g., SetVisibility());
 * This should be use when the type is an enum. It is use to avoid warnings on
 * some compilers with non specified enum types passed to
 * itkDebugMacro. */
#define itkSetEnumMacro(name, type)                                           \
  virtual void Set##name (const type _arg)                                  \
    {                                                                         \
    itkDebugMacro( "setting " #name " to " << static_cast< long >( _arg ) ); \
    if ( this->m_##name != _arg )                                           \
      {                                                                       \
      this->m_##name = _arg;                                                \
      this->Modified();                                                       \
      }                                                                       \
    }

/** Get built-in type.  Creates member Get"name"() (e.g., GetVisibility());
  * This should be use when the type is an enum. It is use to avoid warnings on
  * some compilers with non specified enum types passed to
  * itkDebugMacro. */
#define itkGetEnumMacro(name, type)                                           \
  virtual type Get##name () const                                             \
    {                                                                         \
    return this->m_##name;                                                    \
    }

/** Set character string.  Creates member Set"name"()
 * (e.g., SetFilename(char *)). The macro assumes that
 * the class member (name) is declared a type std::string. */
#define itkSetStringMacro(name)                             \
  virtual void Set##name (const char *_arg)               \
    {                                                       \
    if ( _arg && ( _arg == this->m_##name ) ) { return; } \
    if ( _arg )                                             \
      {                                                     \
      this->m_##name = _arg;                              \
      }                                                     \
    else                                                    \
      {                                                     \
      this->m_##name = "";                                \
      }                                                     \
    this->Modified();                                       \
    }                                                       \
  virtual void Set##name (const std::string & _arg)       \
    {                                                       \
    this->Set##name( _arg.c_str() );                      \
    }                                                       \


/** Get character string.  Creates member Get"name"()
 * (e.g., SetFilename(char *)). The macro assumes that
 * the class member (name) is declared as a type std::string. */
#define itkGetStringMacro(name)            \
  virtual const char *Get##name () const \
    {                                      \
    return this->m_##name.c_str();       \
    }

/** Set built-in type where value is constrained between min/max limits.
 * Create member Set"name"() (e.q., SetRadius()). \#defines are
 * convienience for clamping open-ended values. */
#define itkSetClampMacro(name, type, min, max)                                    \
  virtual void Set##name (type _arg)                                            \
    {                                                                             \
    itkDebugMacro("setting " << #name " to " << _arg);                           \
    if ( this->m_##name != ( _arg < min ? min : ( _arg > max ? max : _arg ) ) ) \
      {                                                                           \
      this->m_##name = ( _arg < min ? min : ( _arg > max ? max : _arg ) );      \
      this->Modified();                                                           \
      }                                                                           \
    }

/** Set pointer to object; uses Object reference counting methodology.
 * Creates method Set"name"() (e.g., SetPoints()). Note that using
 * smart pointers requires using real pointers when setting input,
 * but returning smart pointers on output. */
#define itkSetObjectMacro(name, type)                   \
  virtual void Set##name (type * _arg)                \
    {                                                   \
    itkDebugMacro("setting " << #name " to " << _arg); \
    if ( this->m_##name != _arg )                     \
      {                                                 \
      this->m_##name = _arg;                          \
      this->Modified();                                 \
      }                                                 \
    }

/** Get a smart pointer to an object.  Creates the member
 * Get"name"() (e.g., GetPoints()).
 * NOTE:  This function returns a non-const
 * version of the internal member variable
 * and could easily be used to modify the
 * behavior of the class without
 * properly resetting the pipeline
 * symantics */
// NOTE: A class can use either itkGetModifiableObjectMacro
//       or itkGetObjectMacro, but not both.
//       A class can use either itkGetModifiableObjectMacro
//       or itkGetConstObjectMacro, but not both.
//       If the desired behavior is to only provide const
//       access to the itkObject ivar, then use itkGetConstObjectMacro,
//       else use itkGetModifiableObjectMacro for read/write access to
//       the ivar.
//       It is permissable to use both itkGetObjectMacro and itkGetConstObjectMacro
//       for backwards compatibility.
//       If the ITK_LEGACY_REMOVE=FALSE, then it is
//       permissable to use itkGetObjectMacro which
//       defines both signatures itk::GetXXX() and
//       itk::GetModifiableXXX()

/** Get a smart const pointer to an object.  Creates the member
 * Get"name"() (e.g., GetPoints()). */
#define itkGetConstObjectMacro(name, type)            \
  virtual const type * Get##name () const             \
    {                                                 \
    return this->m_##name.GetPointer();               \
    }


#if defined ( ITK_FUTURE_LEGACY_REMOVE )
// In the future, the itkGetObjectMacro will be deprecated with the ITK_LEGACY_REMOVE
// flag.  For now, this very advanced feature is only available
// through manual setting of a compiler define -DITK_FUTURE_LEGACY_REMOVE
// ("/DITK_FUTURE_LEGACY_REMOVE /EHsc" with Visual Studio)
// to ease the transition from the historical GetObjectMacro to the GetModifiableObjectMacro
#  define itkGetObjectMacro(name, type)                                                         \
  virtual type * Get##name ()                                                                   \
    {                                                                                           \
    purposeful_error("itkGetObjectMacro should be replaced with itkGetModifiableObjectMacro."); \
    }

#  define itkGetModifiableObjectMacro(name, type)     \
  virtual type * GetModifiable##name ()               \
    {                                                 \
    return this->m_##name.GetPointer();               \
    }                                                 \
  itkGetConstObjectMacro(name, type)

#else  // defined ( ITK_FUTURE_LEGACY_REMOVE )
/** Get a smart pointer to an object.  Creates the member
 * Get"name"() (e.g., GetPoints()). */
# define itkGetObjectMacro(name, type)                \
  virtual type * Get##name ()                         \
    {                                                 \
    return this->m_##name.GetPointer();               \
    }
#  define itkGetModifiableObjectMacro(name, type)     \
  virtual type * GetModifiable##name ()               \
    {                                                 \
    return this->m_##name.GetPointer();               \
    }                                                 \
  itkGetConstObjectMacro(name, type)                  \
  itkGetObjectMacro(name, type)
#endif // defined ( ITK_FUTURE_LEGACY_REMOVE )

// For backwards compatibility define ITK_EXPORT to nothing
#define ITK_EXPORT


/** Get a const reference to a smart pointer to an object.
 * Creates the member Get"name"() (e.g., GetPoints()). */
#define itkGetConstReferenceObjectMacro(name, type)                     \
  virtual const typename type::Pointer & Get##name () const             \
    {                                                                   \
    return this->m_##name;                                              \
    }

/** Set const pointer to object; uses Object reference counting methodology.
 * Creates method Set"name"() (e.g., SetPoints()). Note that using
 * smart pointers requires using real pointers when setting input,
 * but returning smart pointers on output. */
#define itkSetConstObjectMacro(name, type)              \
  virtual void Set##name (const type * _arg)          \
    {                                                   \
    itkDebugMacro("setting " << #name " to " << _arg); \
    if ( this->m_##name != _arg )                     \
      {                                                 \
      this->m_##name = _arg;                          \
      this->Modified();                                 \
      }                                                 \
    }

/** Create members "name"On() and "name"Off() (e.g., DebugOn() DebugOff()).
 * Set method must be defined to use this macro. */
#define itkBooleanMacro(name) \
  virtual void name##On ()  \
    {                         \
    this->Set##name(true);  \
    }                         \
  virtual void name##Off () \
    {                         \
    this->Set##name(false); \
    }

/** General set vector macro creates a single method that copies specified
 * number of values into object.
 * Examples: void SetColor(c,3) */
#define itkSetVectorMacro(name, type, count) \
  virtual void Set##name(type data[])      \
    {                                        \
    unsigned int i;                          \
    for ( i = 0; i < count; i++ )            \
      {                                      \
      if ( data[i] != this->m_##name[i] )  \
        {                                    \
        break;                               \
        }                                    \
      }                                      \
    if ( i < count )                         \
      {                                      \
      this->Modified();                      \
      for ( i = 0; i < count; i++ )          \
        {                                    \
        this->m_##name[i] = data[i];       \
        }                                    \
      }                                      \
    }

/** Get vector macro. Returns pointer to type (i.e., array of type).
 * This is for efficiency. */
#define itkGetVectorMacro(name, type, count) \
  virtual type * Get##name () const        \
    {                                        \
    return this->m_##name;                 \
    }

/** Construct a non-templatized helper class that
 * provides the GPU kernel source code as a const char*
 */
#define itkGPUKernelClassMacro(kernel)   \
class kernel                  \
  {                                      \
    public:                              \
      static const char* GetOpenCLSource(); \
    private:                             \
      kernel();                          \
      virtual ~kernel();                 \
      kernel(const kernel &);            \
      void operator=(const kernel &);    \
  };

#define itkGetOpenCLSourceFromKernelMacro(kernel) \
  static const char* GetOpenCLSource() \
  {                                 \
    return kernel::GetOpenCLSource();  \
  }

}

#endif //end of itkMacro.h
