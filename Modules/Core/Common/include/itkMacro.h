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

#ifndef itkMacro_h
#define itkMacro_h

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

// Define ITK_PRAGMA macro.
//
// It sets "#pragma" preprocessor directives without expecting the arguments
// to be quoted.
#define ITK_PRAGMA(x) _Pragma (#x)

// The clang compiler has many useful non-default compiler warnings
// that tend to have a high false positive rate.
// The following set of defines allows us to suppress false positives
// and still track down suspicious code
#if defined(__clang__) && defined(__has_warning)
#define CLANG_PRAGMA_PUSH ITK_PRAGMA(clang diagnostic push)
#define CLANG_PRAGMA_POP  ITK_PRAGMA(clang diagnostic pop)
# if __has_warning("-Wfloat-equal")
#define CLANG_SUPPRESS_Wfloat_equal ITK_PRAGMA( clang diagnostic ignored "-Wfloat-equal" )
# endif
#else
#define CLANG_PRAGMA_PUSH
#define CLANG_PRAGMA_POP
#define CLANG_SUPPRESS_Wfloat_equal
#endif

// Define ITK_GCC_PRAGMA_DIAG(param1 [param2 [...]]) macro.
//
// This macros sets a pragma diagnostic if it supported by the version
// of GCC being used otherwise it is a no-op.
//
// GCC diagnostics pragma supported only with GCC >= 4.2
#if defined( __GNUC__ ) && !defined( __INTEL_COMPILER )
#  if ( __GNUC__ > 4 ) || (( __GNUC__ >= 4 ) && ( __GNUC_MINOR__ >= 2 ))
#    define ITK_GCC_PRAGMA_DIAG(x) ITK_PRAGMA(GCC diagnostic x)
#  else
#    define ITK_GCC_PRAGMA_DIAG(x)
#  endif
#else
#  define ITK_GCC_PRAGMA_DIAG(x)
#endif

// Define ITK_GCC_PRAGMA_DIAG_(PUSH|POP) macros.
//
// These macros respectively push and pop the diagnostic context
// if it is supported by the version of GCC being used
// otherwise it is a no-op.
//
// GCC push/pop diagnostics pragma are supported only with GCC >= 4.6
//
// Define macro ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP if it is supported.
#if defined( __GNUC__ ) && !defined( __INTEL_COMPILER )
#  if ( __GNUC__ > 4 ) || (( __GNUC__ >= 4 ) && ( __GNUC_MINOR__ >= 6 ))
#    define ITK_GCC_PRAGMA_DIAG_PUSH() ITK_GCC_PRAGMA_DIAG(push)
#    define ITK_GCC_PRAGMA_DIAG_POP() ITK_GCC_PRAGMA_DIAG(pop)
#    define ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP
#  else
#    define ITK_GCC_PRAGMA_DIAG_PUSH()
#    define ITK_GCC_PRAGMA_DIAG_POP()
#  endif
#else
#  define ITK_GCC_PRAGMA_DIAG_PUSH()
#  define ITK_GCC_PRAGMA_DIAG_POP()
#endif

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

// Setup symbol exports
#ifndef ITK_TEMPLATE_EXPORT
  #ifdef ITK_TEMPLATE_VISIBILITY_DEFAULT
    #define ITK_TEMPLATE_EXPORT __attribute__ ((visibility ("default")))
  #else
    #define ITK_TEMPLATE_EXPORT
  #endif
#endif

// Setup symbol exports
#ifdef ITK_TEMPLATE_VISIBILITY_DEFAULT
  #define ITK_FORCE_EXPORT_MACRO(moduleName) __attribute__ ((visibility ("default")))
#else
  #define ITK_FORCE_EXPORT_MACRO(moduleName) moduleName ## _EXPORT
#endif

#ifndef ITK_FORWARD_EXPORT
  // If build with shared libraries, on MacOS, if USE_COMPILER_HIDDEN_VISIBILITY is ON
  #if defined(__APPLE__)\
   && defined(ITK_TEMPLATE_VISIBILITY_DEFAULT)\
   && defined(ITK_BUILD_SHARED_LIBS)\
   && defined(USE_COMPILER_HIDDEN_VISIBILITY)
    #define ITK_FORWARD_EXPORT __attribute__ ((visibility ("default")))
  #else
    #define ITK_FORWARD_EXPORT
  #endif
#endif

#if ITK_COMPILED_CXX_STANDARD_VERSION >= 201103L
  #define ITK_HAS_CXX11_RVREF
#endif

#if ! defined( ITK_FUTURE_LEGACY_REMOVE )
  #if ITK_COMPILER_CXX_STATIC_ASSERT
    #define ITK_HAS_CXX11_STATIC_ASSERT //NOTE DEPRECATED!  should be ITK_COMPILER_CXX_STATIC_ASSERT
  #endif

  #if ITK_COMPILER_CXX_DELETED_FUNCTIONS
    #define ITK_DELETE_FUNCTION =delete //NOTE DEPRECATED!  should be ITK_DELETED_FUNCTION
  #else
    #define ITK_DELETE_FUNCTION
  #endif
  //NOTE DEPRECATED! should use ITK_COMPILER_CXX_ALIGNAS
  #if ITK_COMPILER_CXX_ALIGNAS
     // defined if the compiler supports C++11 alignas type specifier
     #define ITK_HAS_CPP11_ALIGNAS
  #else
     #ifdef ITK_HAS_CPP11_ALIGNAS
       #undef ITK_HAS_CPP11_ALIGNAS
     #endif
  #endif
  #if ITK_COMPILER_CXX_NOEXCEPT // NOTE DEPRECATED!  Kept for backwards compatibility.
    // Should use ITK_NOEXCEPT defined in: itk_compiler_detection.h.
    // Following comments apply to ITK_NOEXCEPT as well.
    // Use only for low-level functions. Swap, UnRegister, etc, involved in move semantics.
    // Avoid using it elsewhere.
    // Unnecesary in destructors when c++11 becomes a requirement.
    //   Used right now to avoid c++03 errors in destructors of child classes of std::exception.
    #define ITK_NOEXCEPT_OR_THROW() noexcept
  #else
    #define ITK_NOEXCEPT_OR_THROW() throw()
  #endif
#else
  //NOTE DEPRECATED should be ITK_NOEXCEPT
  #define ITK_NOEXCEPT_OR_THROW error "Replace ITK_NOEXCEPT_OR_THROW with ITK_NOEXCEPT"
  //NOTE DEPRECATED!  should be ITK_COMPILER_CXX_STATIC_ASSERT
  #define ITK_HAS_CXX11_STATIC_ASSERT error "Replace ITK_HAS_CXX11_STATIC_ASSERT with ITK_COMPILER_CXX_STATIC_ASSERT"
  //NOTE DEPRECATED!  should be ITK_DELETED_FUNCTION
  #define ITK_DELETE_FUNCTION error "Replace ITK_DELETE_FUNCTION with ITK_DELETED_FUNCTION"
  //NOTE DEPRECATED! should use ITK_COMPILER_CXX_ALIGNAS
  #define ITK_HAS_CPP11_ALIGNAS error "Replace ITK_HAS_CPP11_ALIGNAS with ITK_COMPILER_CXX_ALIGNAS"
#endif


#if ITK_COMPILER_CXX_CONSTEXPR
  #define ITK_CONSTEXPR_FUNC constexpr
  #define ITK_CONSTEXPR_VAR constexpr
#else
  #define ITK_CONSTEXPR_FUNC inline
  #define ITK_CONSTEXPR_VAR const
#endif

// Use "ITK_FALLTHROUGH;" to annotate deliberate fall-through in switches,
// use it analogously to "break;".  The trailing semi-colon is required.
#if defined( __GNUC__ ) && !defined( __INTEL_COMPILER )
# if ( __GNUC__ >= 7 )
#  define ITK_FALLTHROUGH __attribute__((fallthrough))
# endif
#elif ITK_COMPILED_CXX_STANDARD_VERSION >= 201103L && defined(__has_warning)
# if __has_feature(cxx_attributes) && __has_warning("-Wimplicit-fallthrough")
#  define ITK_FALLTHROUGH [[clang::fallthrough]]
# endif
#endif

#ifndef ITK_FALLTHROUGH
# define ITK_FALLTHROUGH ((void)0)
#endif

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
    if ( smartPtr.GetPointer() == ITK_NULLPTR )                \
      {                                                        \
      smartPtr = new x;                                        \
      }                                                        \
    smartPtr->UnRegister();                                    \
    return smartPtr;                                           \
    }

#define itkCreateAnotherMacro(x)                               \
  virtual ::itk::LightObject::Pointer CreateAnother(void) const ITK_OVERRIDE \
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
  virtual ::itk::LightObject::Pointer CreateAnother(void) const ITK_OVERRIDE \
    {                                                          \
    ::itk::LightObject::Pointer smartPtr;                      \
    smartPtr = x::New().GetPointer();                          \
    return smartPtr;                                           \
    }

//
// A macro to disallow the copy constructor and operator= functions
// This should be used in the private: declarations for a class
//
// ITK's paradigm for smart pointer and pipeline consistency
// prohibits the use of copy construction and operator= functions.
//
#define ITK_DISALLOW_COPY_AND_ASSIGN(TypeName)         \
  TypeName(const TypeName&) ITK_DELETED_FUNCTION;      \
  void operator=(const TypeName&) ITK_DELETED_FUNCTION

/** Macro used to add standard methods to all classes, mainly type
 * information. */
#define itkTypeMacro(thisClass, superclass)  \
  virtual const char *GetNameOfClass() const ITK_OVERRIDE \
    {                                        \
    return #thisClass;                      \
    }

#define itkTypeMacroNoParent(thisClass)             \
  virtual const char *GetNameOfClass() const \
  {                                          \
    return #thisClass;                       \
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
  #if defined( _WIN32 ) && !defined( __MINGW32__ ) && !defined( ITK_WRAPPING_PARSER )
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
//   // \deprecated Replaced by MyOtherMethod() as of ITK 2.0.
//   itkLegacyMacro(void MyMethod());
//
// See below for what to do for the method definition.
#if defined( ITK_LEGACY_REMOVE )
#define itkLegacyMacro(method) /* no ';' */
#elif defined( ITK_LEGACY_SILENT ) || defined( ITK_LEGACY_TEST ) || defined( ITK_WRAPPING_PARSER )
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
//   #if !defined( ITK_LEGACY_REMOVE )
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
//   #endif
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
// itkPadStruct will add padding to a structure to ensure a minimum size
// for ensuring that adjacent structures do not share CACHE lines.
// Each struct will take up some multiple of cacheline sizes.
// This is particularly useful for arrays of thread private variables.
//
#define itkPadStruct( mincachesize, oldtype, newtype )                        \
    struct newtype: public oldtype                                            \
      {                                                                       \
         char _StructPadding[mincachesize - (sizeof(oldtype)%mincachesize) ]; \
      };

//
// itkAlignedTypedef is a macro which creates a new typedef to make a
// data structure aligned.
//
#if defined( ITK_HAS_GNU_ATTRIBUTE_ALIGNED )
# define itkAlignedTypedef( alignment, oldtype, newtype )   \
  typedef oldtype newtype __attribute__((aligned(alignment)))
#elif defined ( _MSC_VER )
# define itkAlignedTypedef( alignment, oldtype, newtype )   \
  typedef __declspec(align( alignment )) oldtype newtype
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
}

//  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//  !!  The ITK Get/Set Macros for various types !!
//  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
/** Portable definition of static constants.
 *
 * \pre \c type shall be an integral type (\c bool and enums are accepted as
 * well). If using C++, float may be valid (see below).
 *
 * \warning If the compiler does not support in-class member initialization,
 * the constants will be signed integers. You may observe warnings due to signed /
 * unsigned comparisons.
 *
 * When using C++11 or greater, constexpr
 * may be necessary for static const float initialization
 * and is beneficial in other cases where a value can be constant.
 *
 * \ingroup ITKCommon */
#if defined(__GNUC__) && ((__GNUC__ * 100) + __GNUC_MINOR__ ) < 405 && !defined( __clang__ ) && !defined( __INTEL_COMPILER )
#  define itkStaticConstMacro(name,type,value) enum { name = value }
#else
#  define itkStaticConstMacro(name,type,value) static ITK_CONSTEXPR_VAR type name = value
#endif

#define itkGetStaticConstMacro(name) (Self::name)

/** Set an input. This defines the Set"name"() method */
#define itkSetInputMacro(name, type)                                              \
  virtual void Set##name(const type *_arg)                                        \
    {                                                                             \
    itkDebugMacro("setting input " #name " to " << _arg);                         \
    if ( _arg != itkDynamicCastInDebugMode< type * >( this->ProcessObject::GetInput(#name) ) )  \
      {                                                                           \
      this->ProcessObject::SetInput( #name, const_cast< type * >( _arg ) );       \
      this->Modified();                                                           \
      }                                                                           \
    }

/** Get an input. This defines the Get"name"() method */
#define itkGetInputMacro(name, type)                                                                            \
  virtual const type * Get##name() const                                                                        \
    {                                                                                                           \
    itkDebugMacro( "returning input " << #name " of " <<  this->ProcessObject::GetInput(#name) );               \
    return itkDynamicCastInDebugMode< const type * >( this->ProcessObject::GetInput(#name) );                   \
    }

/** Set a decorated input. This defines the Set"name"() and a Set"name"Input() method */
#define itkSetDecoratedInputMacro(name, type)                                                                 \
  virtual void Set##name##Input(const SimpleDataObjectDecorator< type > *_arg)                                \
    {                                                                                                         \
    itkDebugMacro("setting input " #name " to " << _arg);                                                     \
    if ( _arg != itkDynamicCastInDebugMode< SimpleDataObjectDecorator< type > * >( this->ProcessObject::GetInput(#name) ) ) \
      {                                                                                                       \
      this->ProcessObject::SetInput( #name, const_cast< SimpleDataObjectDecorator< type > * >( _arg ) );      \
      this->Modified();                                                                                       \
      }                                                                                                       \
    }                                                                                                         \
  virtual void Set##name(const SimpleDataObjectDecorator< type > *_arg)                                \
    {                                                                                                         \
    this->Set##name##Input(_arg);                                                                                                  \
    }                                                                                                         \
  virtual void Set##name(const type &_arg)                           \
    {                                                                \
    typedef SimpleDataObjectDecorator< type > DecoratorType;         \
    itkDebugMacro("setting input " #name " to " << _arg);            \
    const DecoratorType *oldInput =                                  \
      itkDynamicCastInDebugMode< const DecoratorType * >(            \
        this->ProcessObject::GetInput(#name) );                      \
CLANG_PRAGMA_PUSH                                                    \
CLANG_SUPPRESS_Wfloat_equal                                          \
    if ( oldInput && oldInput->Get() == _arg )                       \
CLANG_PRAGMA_POP                                                     \
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
    itkDebugMacro( "returning input " << #name " of " << this->ProcessObject::GetInput(#name) );                                             \
    return itkDynamicCastInDebugMode< const SimpleDataObjectDecorator< type > * >( this->ProcessObject::GetInput(#name) );                   \
    }                                                                \
  virtual const type & Get##name() const                             \
    {                                                                \
    itkDebugMacro("Getting input " #name);                           \
    typedef SimpleDataObjectDecorator< type > DecoratorType;         \
    const DecoratorType *input =                                     \
      itkDynamicCastInDebugMode< const DecoratorType * >(            \
        this->ProcessObject::GetInput(#name) );                      \
    if( input == ITK_NULLPTR )                                       \
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
    if ( _arg != itkDynamicCastInDebugMode< DataObjectDecorator< type > * >( this->ProcessObject::GetInput(#name) ) ) \
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
      itkDynamicCastInDebugMode< const DecoratorType * >(            \
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
      itkDebugMacro( "returning input " << #name " of "<< this->ProcessObject::GetInput(#name) );                                      \
    return itkDynamicCastInDebugMode< const DataObjectDecorator< type > * >( this->ProcessObject::GetInput(#name) );                   \
    }                                                                \
  virtual const type * Get##name() const                             \
    {                                                                \
    itkDebugMacro("Getting input " #name);                           \
    typedef DataObjectDecorator< type > DecoratorType;               \
    const DecoratorType *input =                                     \
      itkDynamicCastInDebugMode< const DecoratorType * >(            \
        this->ProcessObject::GetInput(#name) );                      \
    if( input == ITK_NULLPTR )                                       \
      {                                                              \
      return ITK_NULLPTR;                                            \
      }                                                              \
    return input->Get();                                             \
    }

/** Set a decorated input. This defines the Set"name"() and Set"name"Input() method
 * and Get"name" and Get"name"Input methods */
#define itkSetGetDecoratedObjectInputMacro(name, type)  \
  itkSetDecoratedObjectInputMacro(name, type)           \
  itkGetDecoratedObjectInputMacro(name, type)

/** Set built-in type.  Creates member Set"name"() (e.g., SetVisibility()); */
#define itkSetMacro(name, type)                     \
  virtual void Set##name (const type _arg)          \
    {                                               \
    itkDebugMacro("setting " #name " to " << _arg); \
CLANG_PRAGMA_PUSH                                   \
CLANG_SUPPRESS_Wfloat_equal                         \
    if ( this->m_##name != _arg )                   \
      {                                             \
      this->m_##name = _arg;                        \
      this->Modified();                             \
      }                                             \
CLANG_PRAGMA_POP                                    \
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
 * This should be used when the type is an enum. It is used to avoid warnings on
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
#define itkSetClampMacro(name, type, min, max)                                 \
  virtual void Set##name (type _arg)                                     \
    {                                                                          \
    const type temp_extrema=( _arg < min ? min : ( _arg > max ? max : _arg ) );\
    itkDebugMacro("setting " << #name " to " << _arg);                         \
CLANG_PRAGMA_PUSH                                                              \
CLANG_SUPPRESS_Wfloat_equal                                                    \
    if ( this->m_##name != temp_extrema )                                      \
      {                                                                        \
      this->m_##name = temp_extrema;                                           \
      this->Modified();                                                        \
      }                                                                        \
CLANG_PRAGMA_POP                                                               \
    }

//NOTE: warning: comparing floating point with == or != is unsafe [-Wfloat-equal]
/** Set pointer to object; uses Object reference counting methodology.
 * Creates method Set"name"() (e.g., SetPoints()). Note that using
 * smart pointers requires using real pointers when setting input,
 * but returning smart pointers on output. */
#define itkSetObjectMacro(name, type)                  \
  virtual void Set##name (type * _arg)                 \
    {                                                  \
    itkDebugMacro("setting " << #name " to " << _arg); \
CLANG_PRAGMA_PUSH                                      \
CLANG_SUPPRESS_Wfloat_equal                            \
    if ( this->m_##name != _arg )                      \
      {                                                \
      this->m_##name = _arg;                           \
      this->Modified();                                \
      }                                                \
CLANG_PRAGMA_POP                                       \
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
  virtual void Set##name(type data[])        \
    {                                        \
    unsigned int i;                          \
    for ( i = 0; i < count; i++ )            \
      {                                      \
CLANG_PRAGMA_PUSH                            \
CLANG_SUPPRESS_Wfloat_equal                  \
      if ( data[i] != this->m_##name[i] )    \
CLANG_PRAGMA_POP                             \
        {                                    \
        break;                               \
        }                                    \
      }                                      \
    if ( i < count )                         \
      {                                      \
      this->Modified();                      \
      for ( i = 0; i < count; i++ )          \
        {                                    \
        this->m_##name[i] = data[i];         \
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

/**\def itkGPUKernelClassMacro
 * Construct a non-templatized helper class that
 * provides the GPU kernel source code as a const char*
 */
#define itkGPUKernelClassMacro(kernel)      \
/**\class kernel                            \
 * Workaround KWstyle bug                   \
 * \ingroup ITKCommon                       \
 */                                         \
class kernel                                \
  {                                         \
    public:                                 \
      static const char* GetOpenCLSource(); \
    private:                                \
      kernel();                             \
      virtual ~kernel();                    \
      kernel(const kernel &);               \
      void operator=(const kernel &);       \
  };

#define itkGetOpenCLSourceFromKernelMacro(kernel) \
  static const char* GetOpenCLSource() \
  {                                 \
    return kernel::GetOpenCLSource();  \
  }

// A useful macro in the PrintSelf method for printing member variables
// which are pointers to object based on the LightObject class.
#define itkPrintSelfObjectMacro(name)                                 \
  if (static_cast<const LightObject*>(this->m_##name) == ITK_NULLPTR) \
    {                                                                 \
    os << indent << #name << ": (null)" << std::endl;                 \
    }                                                                 \
  else                                                                \
    {                                                                 \
    os << indent << #name << ": " << std::endl;                       \
    this->m_##name->Print(os,indent.GetNextIndent());                 \
    }


/** Set a decorated output. This defines the Set"name"() and a Set"name"Output() method */
#define itkSetDecoratedOutputMacro(name, type)                                                                 \
  virtual void Set##name##Output(const SimpleDataObjectDecorator< type > *_arg)                                \
    {                                                                                                         \
    itkDebugMacro("setting output " #name " to " << _arg);                                                     \
    if ( _arg != itkDynamicCastInDebugMode< SimpleDataObjectDecorator< type > * >( this->ProcessObject::GetOutput(#name) ) ) \
      {                                                                                                       \
      this->ProcessObject::SetOutput( #name, const_cast< SimpleDataObjectDecorator< type > * >( _arg ) );      \
      this->Modified();                                                                                       \
      }                                                                                                       \
    }                                                                                                         \
  virtual void Set##name(const type &_arg)                                \
    {                                                                     \
    typedef SimpleDataObjectDecorator< type > DecoratorType;              \
    itkDebugMacro("setting output " #name " to " << _arg);                \
    DecoratorType *output = itkDynamicCastInDebugMode< DecoratorType * >( \
        this->ProcessObject::GetOutput(#name) );                          \
    if ( output )                                                         \
      {                                                                   \
      if ( output->Get() == _arg )                                        \
        {                                                                 \
        return;                                                           \
        }                                                                 \
      else                                                                \
        {                                                                 \
        output->Set(_arg);                                                \
        }                                                                 \
      }                                                                   \
    else                                                                  \
      {                                                                   \
      typename DecoratorType::Pointer newOutput = DecoratorType::New();   \
      newOutput->Set(_arg);                                               \
      this->Set##name##Output(newOutput);                                 \
      }                                                                   \
    }

/** Set a decorated output. This defines the Get"name"() and Get"name"Output() method */
#define itkGetDecoratedOutputMacro(name, type)                                                                                \
  virtual const SimpleDataObjectDecorator< type > * Get##name##Output() const                                                 \
    {                                                                                                                         \
    itkDebugMacro( "returning output " << #name " of " << this->ProcessObject::GetOutput(#name) );                            \
    return itkDynamicCastInDebugMode< const SimpleDataObjectDecorator< type > * >( this->ProcessObject::GetOutput(#name) );   \
    }                                                                \
  virtual const type & Get##name() const                             \
    {                                                                \
    itkDebugMacro("Getting output " #name);                          \
    typedef SimpleDataObjectDecorator< type > DecoratorType;         \
    const DecoratorType *output =                                    \
      itkDynamicCastInDebugMode< const DecoratorType * >(            \
        this->ProcessObject::GetOutput(#name) );                     \
    if( output == ITK_NULLPTR )                                      \
      {                                                              \
      itkExceptionMacro(<<"output" #name " is not set");             \
      }                                                              \
    return output->Get();                                            \
    }


#endif //end of itkMacro.h
