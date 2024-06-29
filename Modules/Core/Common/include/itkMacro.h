/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
 * available for built-in types; for string classes; vector arrays;
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
#  include <cassert>
#  include "itkPrintHelper.h" // for ostream operator<<std::vector<T>
#endif

#include <sstream>
#include <type_traits> // For is_same, remove_const, and remove_reference.

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

// clang-format off
/** The `static_assert(true, "")`  idiom is commonly employed for
 *  C++11 or greater to ensure that it is compile-time only
 *  check that can not be part of the binary file.
 *
 *  The ITK_NOOP_STATEMENT idiom allows a macro to be used anywhere
 *  that a statement is expected, and to enforce consistent use of
 *  ';' after a macro. The static_assert is a constexpr that can be
 *  used in places where raw statements (i.e. 'do{} while(0)') are
 *  not allowed (i.e. after class member function definitions).
 *  */
#define ITK_NOOP_STATEMENT static_assert(true, "")

/* NOTE:  The ITK_MACROEND_NOOP_STATEMENT is used at the end
 * of ITK supported macros to ensure that when the macro
 * is used in the code base that the line must have ';' after
 * the macro is used. This makes formatting visually similar
 * to functions, and greatly improves the clang-format
 * behaviors for indentation.  This also assists
 * modern IDE's and removes 1000's of warnings about
 * unused statements or unnecessary ';' when macros are
 * used. */
#define ITK_MACROEND_NOOP_STATEMENT ITK_NOOP_STATEMENT
// clang-format on

// Define ITK_PRAGMA macro.
//
// It sets "#pragma" preprocessor directives without expecting the arguments
// to be quoted.
#define ITK_PRAGMA(x) _Pragma(#x)

// The GCC/Clang compilers have many useful non-default compiler warnings
// that tend to have a high false positive rate or are otherwise not always appropriate.
// The following set of defines allows us to suppress instances of said warnings.

// For GCC and Clang (Clang also identifies itself as GCC, and supports these pragmas):
#if defined(__GNUC__)
#  define ITK_GCC_PRAGMA_PUSH ITK_PRAGMA(GCC diagnostic push)
#  define ITK_GCC_PRAGMA_POP ITK_PRAGMA(GCC diagnostic pop)
#  define ITK_GCC_SUPPRESS_Wfloat_equal ITK_PRAGMA(GCC diagnostic ignored "-Wfloat-equal")
#  define ITK_GCC_SUPPRESS_Wformat_nonliteral ITK_PRAGMA(GCC diagnostic ignored "-Wformat-nonliteral")
#  define ITK_GCC_SUPPRESS_Warray_bounds ITK_PRAGMA(GCC diagnostic ignored "-Warray-bounds")
#else
#  define ITK_GCC_PRAGMA_PUSH
#  define ITK_GCC_PRAGMA_POP
#  define ITK_GCC_SUPPRESS_Wfloat_equal
#  define ITK_GCC_SUPPRESS_Wformat_nonliteral
#  define ITK_GCC_SUPPRESS_Warray_bounds
#endif

// For Clang only (and not GCC):
#if defined(__clang__) && defined(__has_warning)
#  define ITK_CLANG_PRAGMA_PUSH ITK_PRAGMA(clang diagnostic push)
#  define ITK_CLANG_PRAGMA_POP ITK_PRAGMA(clang diagnostic pop)
#  define ITK_CLANG_SUPPRESS_Wzero_as_null_pointer_constant ITK_PRAGMA(clang diagnostic ignored "-Wzero-as-null-pointer-constant")
#else
#  define ITK_CLANG_PRAGMA_PUSH
#  define ITK_CLANG_PRAGMA_POP
#  define ITK_CLANG_SUPPRESS_Wzero_as_null_pointer_constant
#endif

// These were not intended as public API, but some code was nevertheless using them.
// Support the pre ITK 5.4 spelling for compatibility.
#define CLANG_PRAGMA_PUSH ITK_CLANG_PRAGMA_PUSH
#define CLANG_PRAGMA_POP ITK_CLANG_PRAGMA_POP
#define CLANG_SUPPRESS_Wfloat_equal ITK_GCC_SUPPRESS_Wfloat_equal

#if !defined(ITK_LEGACY_REMOVE)
// Issue warning if deprecated preprocessor flag is used.
#  define CLANG_SUPPRESS_Wcpp14_extensions                                                                \
    [[deprecated("Remove deprecated CLANG_SUPPRESS_Wcpp14_extensions c++14 warning suppression")]] void * \
      CLANG_SUPPRESS_Wcpp14_extensions = nullptr;
#endif

// Intel compiler convenience macros
#if defined(__INTEL_COMPILER)
#  define INTEL_PRAGMA_WARN_PUSH ITK_PRAGMA(warning push)
#  define INTEL_PRAGMA_WARN_POP ITK_PRAGMA(warning pop)
#  define INTEL_SUPPRESS_warning_1292 ITK_PRAGMA(warning disable 1292)
#else
#  define INTEL_PRAGMA_WARN_PUSH
#  define INTEL_PRAGMA_WARN_POP
#  define INTEL_SUPPRESS_warning_1292
#endif

// Define ITK_GCC_PRAGMA_DIAG(param1 [param2 [...]]) macro.
//
// This macro sets a pragma diagnostic
//
// Define ITK_GCC_PRAGMA_DIAG_(PUSH|POP) macros.
//
// These macros respectively push and pop the diagnostic context
//
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)
#  define ITK_GCC_PRAGMA_DIAG(x) ITK_PRAGMA(GCC diagnostic x)
#  define ITK_GCC_PRAGMA_DIAG_PUSH() ITK_GCC_PRAGMA_DIAG(push)
#  define ITK_GCC_PRAGMA_DIAG_POP() ITK_GCC_PRAGMA_DIAG(pop)
#else
#  define ITK_GCC_PRAGMA_DIAG(x)
#  define ITK_GCC_PRAGMA_DIAG_PUSH()
#  define ITK_GCC_PRAGMA_DIAG_POP()
#endif

/*
 * ITK only supports MSVC++ 14.2 and greater
 * MSVC++ 14.2 _MSC_VER == 1920 (Visual Studio 2019 Version 16.0)
 */
#if defined(_MSC_VER) && (_MSC_VER < 1920)
#  error "MSVC versions before Visual Studio 2019 are not supported"
#endif
#if defined(__SUNPRO_CC) && (__SUNPRO_CC < 0x5140)
#  error "SUNPro C++ < 5.14.0 is not supported"
#endif
#if defined(__CYGWIN__)
#  error "The Cygwin compiler is not supported"
#endif
#if defined(__BORLANDC__)
#  error "The Borland C compiler is not supported"
#endif
#if defined(__MWERKS__)
#  error "The MetroWerks compiler is not supported"
#endif
#if defined(__GNUC__) && !defined(__clang__) && !defined(__INTEL_COMPILER) && (__GNUC__ < 7)
#  error "GCC < 7 is not supported"
#endif
#if defined(__sgi)
// This is true for IRIX 6.5.18m with MIPSPro 7.3.1.3m.
// TODO: At some future point, it may be necessary to
// define a minimum __sgi version that will work.
#  error "The SGI compiler is not supported"
#endif
#if defined(__APPLE__)
#  if defined(__clang__) && (__cplusplus < 201703L)
#    error "Apple LLVM compiling with a standard less than C++17 is not supported"
#  endif
#elif defined(__clang__) && (__clang_major__ < 5)
#  error "Clang < 5 is not supported"
#endif
#if defined(__INTEL_COMPILER) && (__INTEL_COMPILER < 1910)
#  error "Intel C++ < 19.1 is not supported4"
#endif

// Setup symbol exports
#if defined(_WIN32) || defined(WIN32)
#  define ITK_ABI_IMPORT __declspec(dllimport)
#  define ITK_ABI_EXPORT __declspec(dllexport)
#  define ITK_ABI_HIDDEN
#else
#  ifdef __GNUC__
#    define ITK_ABI_IMPORT __attribute__((visibility("default")))
#    define ITK_ABI_EXPORT __attribute__((visibility("default")))
#    define ITK_ABI_HIDDEN __attribute__((visibility("hidden")))
#  else
#    define ITK_ABI_IMPORT
#    define ITK_ABI_EXPORT
#    define ITK_ABI_HIDDEN
#  endif
#endif

// Setup symbol exports
#ifndef ITK_TEMPLATE_EXPORT
#  ifdef ITK_TEMPLATE_VISIBILITY_DEFAULT
#    define ITK_TEMPLATE_EXPORT __attribute__((visibility("default")))
#  else
#    define ITK_TEMPLATE_EXPORT
#  endif
#endif

// Setup symbol exports
#ifdef ITK_TEMPLATE_VISIBILITY_DEFAULT
#  define ITK_FORCE_EXPORT_MACRO(moduleName) __attribute__((visibility("default")))
#else
#  define ITK_FORCE_EXPORT_MACRO(moduleName) moduleName##_EXPORT
#endif

#ifndef ITK_FORWARD_EXPORT
// If build with shared libraries, on MacOS, if USE_COMPILER_HIDDEN_VISIBILITY is ON
#  if defined(__APPLE__) && defined(ITK_TEMPLATE_VISIBILITY_DEFAULT) && defined(ITK_BUILD_SHARED_LIBS) && \
    defined(USE_COMPILER_HIDDEN_VISIBILITY)
#    define ITK_FORWARD_EXPORT __attribute__((visibility("default")))
#  else
#    define ITK_FORWARD_EXPORT
#  endif
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
#define itkNewMacro(x)      \
  itkSimpleNewMacro(x);     \
  itkCreateAnotherMacro(x); \
  itkCloneMacro(x);         \
  ITK_MACROEND_NOOP_STATEMENT

#define itkSimpleNewMacro(x)                              \
  static Pointer New()                                    \
  {                                                       \
    Pointer smartPtr = ::itk::ObjectFactory<x>::Create(); \
    if (smartPtr == nullptr)                              \
    {                                                     \
      smartPtr = new x();                                 \
    }                                                     \
    smartPtr->UnRegister();                               \
    return smartPtr;                                      \
  }                                                       \
  ITK_MACROEND_NOOP_STATEMENT

#define itkCreateAnotherMacro(x)                                                               \
  ::itk::LightObject::Pointer CreateAnother() const override { return x::New().GetPointer(); } \
  ITK_MACROEND_NOOP_STATEMENT

#define itkCloneMacro(x)                                                  \
  Pointer Clone() const                                                   \
  {                                                                       \
    Pointer rval = dynamic_cast<x *>(this->InternalClone().GetPointer()); \
    return rval;                                                          \
  }                                                                       \
  ITK_MACROEND_NOOP_STATEMENT

/** Define an object creation method throwing an exception if the object
 * is not created through the object factory, for use in base classes that
 * do not fully implement a backend. */
#define itkFactoryOnlyNewMacro(x)  \
  itkSimpleFactoryOnlyNewMacro(x); \
  itkCreateAnotherMacro(x);        \
  itkCloneMacro(x);                \
  ITK_MACROEND_NOOP_STATEMENT

#define itkSimpleFactoryOnlyNewMacro(x)                                                                 \
  static auto New()->Pointer                                                                            \
  {                                                                                                     \
    Pointer smartPtr = ::itk::ObjectFactory<x>::Create();                                               \
    if (smartPtr == nullptr)                                                                            \
    {                                                                                                   \
      itkSpecializedMessageExceptionMacro(ExceptionObject,                                              \
                                          "Object factory failed to instantiate " << typeid(x).name()); \
    }                                                                                                   \
    smartPtr->UnRegister();                                                                             \
    return smartPtr;                                                                                    \
  }                                                                                                     \
  ITK_MACROEND_NOOP_STATEMENT

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
#define itkFactorylessNewMacro(x) \
  static Pointer New()            \
  {                               \
    x *     rawPtr = new x();     \
    Pointer smartPtr = rawPtr;    \
    rawPtr->UnRegister();         \
    return smartPtr;              \
  }                               \
  itkCreateAnotherMacro(x);       \
  ITK_MACROEND_NOOP_STATEMENT

//
// A macro to disallow the copy constructor, copy assignment,
// move constructor, and move assignment functions.
// This should be used in the public: declarations for a class
//
// ITK's paradigm for smart pointer and pipeline consistency
// prohibits the use of copy/move construction and copy/move assignment
// functions.
//
#define ITK_DISALLOW_COPY_AND_MOVE(TypeName)       \
  TypeName(const TypeName &) = delete;             \
  TypeName & operator=(const TypeName &) = delete; \
  TypeName(TypeName &&) = delete;                  \
  TypeName & operator=(TypeName &&) = delete

#if !defined(ITK_LEGACY_REMOVE)
#  define ITK_DISALLOW_COPY_AND_ASSIGN(TypeName) ITK_DISALLOW_COPY_AND_MOVE(TypeName)
#else
#  define ITK_DISALLOW_COPY_AND_ASSIGN(TypeName) \
    static_assert(false, "Replace deprecated ITK_DISALLOW_COPY_AND_ASSIGN with modern ITK_DISALLOW_COPY_AND_MOVE")
#endif


/** Explicitly "defaults" the copy constructor, copy assignment operator, move constructor, and move assignment operator
of the specified class. Especially meant to address compiler warnings like:
 - "warning: definition of implicit copy assignment operator for '<TypeName>' is deprecated because it has a
user-declared destructor [-Wdeprecated]" (Mac10.13-AppleClang)
 - "warning C5267: definition of implicit copy constructor for '<TypeName>' is deprecated because it has a user-provided
destructor." (Visual Studio 2022/MSVC)
  Intended to be used in the public section of a class. */
#define ITK_DEFAULT_COPY_AND_MOVE(TypeName)         \
  TypeName(const TypeName &) = default;             \
  TypeName & operator=(const TypeName &) = default; \
  TypeName(TypeName &&) = default;                  \
  TypeName & operator=(TypeName &&) = default


// When ITK_EXPERIMENTAL_CXX20_REWRITTEN_UNEQUAL_OPERATOR is defined, ITK uses
// the ability for operator!= to be rewritten automatically in terms of
// operator==, as introduced with C++20. This macro is experimental. It may be
// modified, renamed, or removed without backward compatibility support.
#if __cplusplus >= 202002L
#  define ITK_EXPERIMENTAL_CXX20_REWRITTEN_UNEQUAL_OPERATOR
#endif

// Note: The following macro, ITK_UNEQUAL_OPERATOR_MEMBER_FUNCTION(TypeName),
// is only for internal use within the implementation of ITK. It may be
// modified, renamed, or removed without backward compatibility support.
#ifdef ITK_EXPERIMENTAL_CXX20_REWRITTEN_UNEQUAL_OPERATOR
// With C++20, operator!= is automatically rewritten in terms of the
// corresponding operator==.
#  define ITK_UNEQUAL_OPERATOR_MEMBER_FUNCTION(TypeName) ITK_MACROEND_NOOP_STATEMENT
#else
// For C++14 and C++17, this macro defines an operator!= member function that
// just calls the corresponding operator== member function.
#  define ITK_UNEQUAL_OPERATOR_MEMBER_FUNCTION(TypeName)                                 \
    bool operator!=(const TypeName & other) const { return !(this->operator==(other)); } \
    ITK_MACROEND_NOOP_STATEMENT
#endif


// Internal macro (not part of the public ITK API), used to implement `GetNameOfClass()` member functions.
#define itkInternalGetNameOfClassImplementationMacro(thisClass)                                             \
  {                                                                                                         \
    static_assert(std::is_same_v<thisClass, std::remove_const_t<std::remove_reference_t<decltype(*this)>>>, \
                  "The macro argument `" #thisClass                                                         \
                  "` appears incorrect! It should correspond with the name of this class!");                \
    return #thisClass;                                                                                      \
  }                                                                                                         \
  ITK_MACROEND_NOOP_STATEMENT


/** Macro's used to add `GetNameOfClass()` member functions to polymorphic ITK classes: `itkVirtualGetNameOfClassMacro`
 * adds a virtual `GetNameOfClass()` member function to the class definition, and `itkOverrideGetNameOfClassMacro` adds
 * a `GetNameOfClass()` override. */
#define itkVirtualGetNameOfClassMacro(thisClass) \
  virtual const char * GetNameOfClass() const itkInternalGetNameOfClassImplementationMacro(thisClass)

#define itkOverrideGetNameOfClassMacro(thisClass) \
  const char * GetNameOfClass() const override itkInternalGetNameOfClassImplementationMacro(thisClass)

#ifdef ITK_FUTURE_LEGACY_REMOVE
#  define itkTypeMacro(thisClass, superclass)                                                                      \
    static_assert(false,                                                                                           \
                  "In a future revision of ITK, the macro `itkTypeMacro(thisClass, superclass)` will be removed. " \
                  "Please call `itkOverrideGetNameOfClassMacro(thisClass)` instead!")
#  define itkTypeMacroNoParent(thisClass)                                                                      \
    static_assert(false,                                                                                       \
                  "In a future revision of ITK, the macro `itkTypeMacroNoParent(thisClass)` will be removed. " \
                  "Please call `itkVirtualGetNameOfClassMacro(thisClass)` instead!")
#else
/** Legacy macro's to add or override a `GetNameOfClass()` member function.
 * \deprecated Instead of `itkTypeMacro`, it is preferred to call `itkOverrideGetNameOfClassMacro` (without `superclass`
 * argument). Instead of `itkTypeMacroNoParent`, it is preferred to call `itkVirtualGetNameOfClassMacro`.
 */
#  define itkTypeMacro(thisClass, superclass) itkOverrideGetNameOfClassMacro(thisClass)
#  define itkTypeMacroNoParent(thisClass) itkVirtualGetNameOfClassMacro(thisClass)
#endif


namespace itk
{
/**
 * The following is used to output debug, warning, and error messages.
 * Use a global function which actually calls:
 * OutputWindow::GetInstance()->DisplayText();
 * This is to avoid Object \#include of OutputWindow
 * while OutputWindow \#includes Object. */
extern ITKCommon_EXPORT void
OutputWindowDisplayText(const char *);

extern ITKCommon_EXPORT void
OutputWindowDisplayErrorText(const char *);

extern ITKCommon_EXPORT void
OutputWindowDisplayWarningText(const char *);

extern ITKCommon_EXPORT void
OutputWindowDisplayGenericOutputText(const char *);

extern ITKCommon_EXPORT void
OutputWindowDisplayDebugText(const char *);

} // end namespace itk

// The itkDebugStatement is to be used to protect code that is only used in the itkDebugMacro
/** This macro is used to print debug (or other information). They are
 * also used to catch errors, etc. Requires that the caller implements
 * the GetDebug() method (see itk::Object). Example usage looks like:
 * itkDebugMacro("this is debug info" << this->SomeVariable); */
#if defined(NDEBUG)
#  define itkDebugMacro(x) ITK_NOOP_STATEMENT
#  define itkDebugStatement(x) ITK_NOOP_STATEMENT
#else
#  define itkDebugMacro(x)                                                     \
    {                                                                          \
      using namespace ::itk::print_helper; /* for ostream << std::vector<T> */ \
      if (this->GetDebug() && ::itk::Object::GetGlobalWarningDisplay())        \
      {                                                                        \
        std::ostringstream itkmsg;                                             \
        itkmsg << "Debug: In " __FILE__ ", line " << __LINE__ << '\n'          \
               << this->GetNameOfClass() << " (" << this << "): " x << "\n\n"; \
        ::itk::OutputWindowDisplayDebugText(itkmsg.str().c_str());             \
      }                                                                        \
    }                                                                          \
    ITK_MACROEND_NOOP_STATEMENT
// The itkDebugStatement is to be used to protect code that is only
// used in the itkDebugMacro
#  define itkDebugStatement(x) x
#endif

/** This macro is used to print warning information (i.e., unusual circumstance
 * but not necessarily fatal.) Example usage looks like:
 * itkWarningMacro("this is warning info" << this->SomeVariable); */
#define itkWarningMacro(x)                                                   \
  {                                                                          \
    if (::itk::Object::GetGlobalWarningDisplay())                            \
    {                                                                        \
      std::ostringstream itkmsg;                                             \
      itkmsg << "WARNING: In " __FILE__ ", line " << __LINE__ << '\n'        \
             << this->GetNameOfClass() << " (" << this << "): " x << "\n\n"; \
      ::itk::OutputWindowDisplayWarningText(itkmsg.str().c_str());           \
    }                                                                        \
  }                                                                          \
  ITK_MACROEND_NOOP_STATEMENT


#define itkWarningStatement(x) x

#if defined(ITK_CPP_FUNCTION)
#  if defined(_WIN32) && !defined(__MINGW32__) && !defined(ITK_WRAPPING_PARSER)
#    define ITK_LOCATION __FUNCSIG__
#  elif defined(__GNUC__)
#    define ITK_LOCATION __PRETTY_FUNCTION__
#  else
#    define ITK_LOCATION __FUNCTION__
#  endif
#else
#  define ITK_LOCATION "unknown"
#endif

#define itkDeclareExceptionMacro(newexcp, parentexcp, whatmessage)                   \
  namespace itk                                                                      \
  {                                                                                  \
  class newexcp : public parentexcp                                                  \
  {                                                                                  \
  public:                                                                            \
    /* default message provides backward compatibility for a given exception type */ \
    static constexpr const char * const default_exception_message = whatmessage;     \
    /* Inherit the constructors from its base class. */                              \
    using parentexcp::parentexcp;                                                    \
    itkOverrideGetNameOfClassMacro(newexcp);                                         \
  };                                                                                 \
  }                                                                                  \
  ITK_MACROEND_NOOP_STATEMENT


#define itkSpecializedMessageExceptionMacro(ExceptionType, x)                                                        \
  {                                                                                                                  \
    std::ostringstream exceptionDescriptionOutputStringStream;                                                       \
    exceptionDescriptionOutputStringStream << "ITK ERROR: " x;                                                       \
    throw ::itk::ExceptionType(                                                                                      \
      std::string{ __FILE__ }, __LINE__, exceptionDescriptionOutputStringStream.str(), std::string{ ITK_LOCATION }); \
  }                                                                                                                  \
  ITK_MACROEND_NOOP_STATEMENT

#define itkSpecializedExceptionMacro(ExceptionType) \
  itkSpecializedMessageExceptionMacro(ExceptionType, << ::itk::ExceptionType::default_exception_message)

/** The itkExceptionMacro macro is used to print error information (i.e., usually
 * a condition that results in program failure). Example usage looks like:
 * itkExceptionMacro("this is error info" << this->SomeVariable); */
#define itkExceptionMacro(x) \
  itkSpecializedMessageExceptionMacro(ExceptionObject, << this->GetNameOfClass() << '(' << this << "): " x)

#define itkGenericExceptionMacro(x) itkSpecializedMessageExceptionMacro(ExceptionObject, x)

#define itkGenericOutputMacro(x)                                                   \
  {                                                                                \
    if (::itk::Object::GetGlobalWarningDisplay())                                  \
    {                                                                              \
      std::ostringstream itkmsg;                                                   \
      itkmsg << "WARNING: In " __FILE__ ", line " << __LINE__ << "\n" x << "\n\n"; \
      ::itk::OutputWindowDisplayGenericOutputText(itkmsg.str().c_str());           \
    }                                                                              \
  }                                                                                \
  ITK_MACROEND_NOOP_STATEMENT

//----------------------------------------------------------------------------
// Macros for simplifying the use of logging
//
#define itkLogMacro(x, y)                                \
  {                                                      \
    if (this->GetLogger())                               \
    {                                                    \
      this->GetLogger()->Write(::itk::LoggerBase::x, y); \
    }                                                    \
  }                                                      \
  ITK_MACROEND_NOOP_STATEMENT

#define itkLogMacroStatic(obj, x, y)                    \
  {                                                     \
    if (obj->GetLogger())                               \
    {                                                   \
      obj->GetLogger()->Write(::itk::LoggerBase::x, y); \
    }                                                   \
  }                                                     \
  ITK_MACROEND_NOOP_STATEMENT

//----------------------------------------------------------------------------
// Setup legacy code policy.
//
// CMake options:
//  - When ITK_LEGACY_REMOVE:BOOL=ON, legacy code is hidden, thus causing compiler errors for code that depends on it
//  - When ITK_LEGACY_REMOVE:BOOL=OFF, and ITK_LEGACY_SILENT:BOOL=ON, use
//    of legacy code will not produce compiler warnings.
//  - When ITK_LEGACY_REMOVE:BOOL=OFF, and ITK_LEGACY_SILENT:BOOL=OFF, use
//    of legacy code will produce compiler warnings
//
// ITK_LEGACY_SILENT silently use legacy code. The default is to warn about legacy code use.
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
#if defined(ITK_LEGACY_REMOVE)
#  define itkLegacyMacro(method) /* no ';' */
#else
#  if defined(ITK_LEGACY_SILENT) || defined(ITK_LEGACY_TEST)
//   Provide legacy methods with no warnings.
#    define itkLegacyMacro(method) method
#  else
//   Request compile-time warnings for uses of deprecated methods.
#    define itkLegacyMacro(method) [[deprecated]] method
#  endif
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
//
//   NOTE: These 4 macros itkLegacyBodyMacro, itkLegacyReplaceBodyMacro,
//         itkGenericLegacyBodyMacro, and itkGenericLegacyReplaceBodyMacro
//         are purposefully not defined when ITK_LEGACY_REMOVE is on,
//         because these macros are only relevant inside code segments
//         that are conditionally compiled only when ITK_LEGACY_REMOVE
//         is off.
#if defined(ITK_LEGACY_SILENT)
#  define itkLegacyBodyMacro(method, version) ITK_NOOP_STATEMENT
#  define itkLegacyReplaceBodyMacro(method, version, replace) ITK_NOOP_STATEMENT
#  define itkGenericLegacyBodyMacro(method, version) ITK_NOOP_STATEMENT
#  define itkGenericLegacyReplaceBodyMacro(method, version, replace) ITK_NOOP_STATEMENT
#else
#  define itkLegacyBodyMacro(method, version) \
    itkWarningMacro(#method " was deprecated for ITK " #version " and will be removed in a future version.")
#  define itkLegacyReplaceBodyMacro(method, version, replace)   \
    itkWarningMacro(#method " was deprecated for ITK " #version \
                            " and will be removed in a future version.  Use " #replace " instead.")
#  define itkGenericLegacyBodyMacro(method, version) \
    itkGenericOutputMacro(#method " was deprecated for ITK " #version " and will be removed in a future version.")
#  define itkGenericLegacyReplaceBodyMacro(method, version, replace)  \
    itkGenericOutputMacro(#method " was deprecated for ITK " #version \
                                  " and will be removed in a future version.  Use " #replace " instead.")
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
#define itkPadStruct(mincachesize, oldtype, newtype)                      \
  struct newtype : public oldtype                                         \
  {                                                                       \
    char _StructPadding[mincachesize - (sizeof(oldtype) % mincachesize)]; \
  }

//
// itkAlignedTypedef is a macro which creates a new type to make a
// data structure aligned.
//
#if defined(ITK_HAS_GNU_ATTRIBUTE_ALIGNED)
#  define itkAlignedTypedef(alignment, oldtype, newtype) using newtype = oldtype __attribute__((aligned(alignment)))
#elif defined(_MSC_VER)
#  define itkAlignedTypedef(alignment, oldtype, newtype) using newtype = __declspec(align(alignment)) oldtype
#else
#  define itkAlignedTypedef(alignment, oldtype, newtype) using newtype = oldtype
#endif

#if defined(ITK_FUTURE_LEGACY_REMOVE)
//=============================================================================
/*
NOTE: DEPRECATED - This macro is not longer needed to support modern
compilers.

 Define a common way of declaring a templated function as a friend inside a class.
  - ITK_FRIEND_TEMPLATE_FUNCTION_ARGUMENTS(T)

  The following templated function

            template <T>
            T add(const T & a, const T & b);

   is declared as friend with

            class A
              {
              public:
                friend Self add<>( const Self & a, const Self & b );
              }

*/
#  define ITK_FRIEND_TEMPLATE_FUNCTION_ARGUMENT(T) < >
#else // LEGACY_REMOVE
#  define ITK_FRIEND_TEMPLATE_FUNCTION_ARGUMENT(T) "Macro remove use C++11 compliant declaration of "
#endif

//--------------------------------------------------------------------------------
//  Helper macros for Template Meta-Programming techniques of for-loops
// unrolling
//--------------------------------------------------------------------------------

//--------------------------------------------------------------------------------
// Macro that generates an unrolled for loop for assigning elements of one array
// to elements of another array The array are assumed to be of same length
// (dimension), and this is also assumed to be the value of NumberOfIterations.
// No verification of size is performed. Casting is performed as part of the
// assignment, by using the DestinationElementType as the casting type.
// Source and destination array types must have defined operator[] in their
// API.
#define itkForLoopAssignmentMacro(                                                                        \
  DestinationType, SourceType, DestinationElementType, DestinationArray, SourceArray, NumberOfIterations) \
  for (unsigned int i = 0; i < NumberOfIterations; ++i)                                                   \
  {                                                                                                       \
    DestinationArray[i] = static_cast<DestinationElementType>(SourceArray[i]);                            \
  }                                                                                                       \
  ITK_MACROEND_NOOP_STATEMENT

//--------------------------------------------------------------------------------
// Macro that generates an unrolled for loop for rounding and assigning
// elements of one array to elements of another array The array are assumed to
// be of same length (dimension), and this is also assumed to be the value of
// NumberOfIterations.  No verification of size is performed. Casting is
// performed as part of the assignment, by using the DestinationElementType as
// the casting type.
// Source and destination array types must have defined operator[] in their
// API.
#define itkForLoopRoundingAndAssignmentMacro(                                                                     \
  DestinationType, Sourcrnd_halfintup, DestinationElementType, DestinationArray, SourceArray, NumberOfIterations) \
  for (unsigned int i = 0; i < NumberOfIterations; ++i)                                                           \
  {                                                                                                               \
    DestinationArray[i] = ::itk::Math::Round<DestinationElementType>(SourceArray[i]);                             \
  }                                                                                                               \
  ITK_MACROEND_NOOP_STATEMENT

// end of Template Meta Programming helper macros

#if !defined(NDEBUG) && !defined(ITK_WRAPPING)

#  ifdef __GLIBC__
#    define itkAssertInDebugOrThrowInReleaseMacro(msg) __assert_fail(msg, __FILE__, __LINE__, __ASSERT_FUNCTION);
#  else
#    define itkAssertInDebugOrThrowInReleaseMacro(msg) itkGenericExceptionMacro(<< msg);
#  endif

#else
#  define itkAssertInDebugOrThrowInReleaseMacro(msg) itkGenericExceptionMacro(<< msg);
#endif

#define itkAssertOrThrowMacro(test, message)                     \
  if (!(test))                                                   \
  {                                                              \
    std::ostringstream msgstr;                                   \
    msgstr << message;                                           \
    itkAssertInDebugOrThrowInReleaseMacro(msgstr.str().c_str()); \
  }                                                              \
  ITK_MACROEND_NOOP_STATEMENT

#if !defined(NDEBUG) && !defined(ITK_WRAPPING)
#  define itkAssertInDebugAndIgnoreInReleaseMacro(X) assert(X)
#else
#  define itkAssertInDebugAndIgnoreInReleaseMacro(X) ITK_NOOP_STATEMENT
#endif


//  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//  !!  The ITK Get/Set Macros for various types !!
//  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifdef ITK_FUTURE_LEGACY_REMOVE
#  define itkStaticConstMacro(name, type, value) \
    "Replace itkStaticConstMacro(name, type, value) with `static constexpr type name = value`"
#  define itkGetStaticConstMacro(name) "Replace itkGetStaticConstMacro(name) with `Self::name`"
#else
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
#  define itkStaticConstMacro(name, type, value) static constexpr type name = value

#  define itkGetStaticConstMacro(name) (Self::name)
#endif

/** Set an input. This defines the Set"name"() method */
#define itkSetInputMacro(name, type)                                                     \
  virtual void Set##name(const type * _arg)                                              \
  {                                                                                      \
    itkDebugMacro("setting input " #name " to " << _arg);                                \
    if (_arg != itkDynamicCastInDebugMode<type *>(this->ProcessObject::GetInput(#name))) \
    {                                                                                    \
      this->ProcessObject::SetInput(#name, const_cast<type *>(_arg));                    \
      this->Modified();                                                                  \
    }                                                                                    \
  }                                                                                      \
  ITK_MACROEND_NOOP_STATEMENT

/** Get an input. This defines the Get"name"() method */
#define itkGetInputMacro(name, type)                                                           \
  virtual const type * Get##name() const                                                       \
  {                                                                                            \
    itkDebugMacro("returning input " << #name " of " << this->ProcessObject::GetInput(#name)); \
    return itkDynamicCastInDebugMode<const type *>(this->ProcessObject::GetInput(#name));      \
  }                                                                                            \
  ITK_MACROEND_NOOP_STATEMENT

// clang-format off
/** Set a decorated input. This defines the Set"name"() and a Set"name"Input() method */
#define itkSetDecoratedInputMacro(name, type)                                                                       \
  virtual void Set##name##Input(const SimpleDataObjectDecorator<type> * _arg)                                       \
  {                                                                                                                 \
    itkDebugMacro("setting input " #name " to " << _arg);                                                           \
    if (_arg != itkDynamicCastInDebugMode<SimpleDataObjectDecorator<type> *>(this->ProcessObject::GetInput(#name))) \
    {                                                                                                               \
      this->ProcessObject::SetInput(#name, const_cast<SimpleDataObjectDecorator<type> *>(_arg));                    \
      this->Modified();                                                                                             \
    }                                                                                                               \
  }                                                                                                                 \
  virtual void Set##name(const SimpleDataObjectDecorator<type> * _arg) { this->Set##name##Input(_arg); }            \
  virtual void Set##name(const type & _arg)                                                                         \
  {                                                                                                                 \
    using DecoratorType = SimpleDataObjectDecorator<type>;                                                          \
    itkDebugMacro("setting input " #name " to " << _arg);                                                           \
    const DecoratorType * oldInput =                                                                                \
      itkDynamicCastInDebugMode<const DecoratorType *>(this->ProcessObject::GetInput(#name));                       \
    ITK_GCC_PRAGMA_PUSH                                                                                             \
    ITK_GCC_SUPPRESS_Wfloat_equal                                                                                   \
    if (oldInput && oldInput->Get() == _arg)                                                                        \
    {                                                                                                               \
      return;                                                                                                       \
    }                                                                                                               \
    ITK_GCC_PRAGMA_POP                                                                                              \
    auto newInput = DecoratorType::New();                                                                           \
    newInput->Set(_arg);                                                                                            \
    this->Set##name##Input(newInput);                                                                               \
  }                                                                                                                 \
  ITK_MACROEND_NOOP_STATEMENT
// clang-format on

/** Set a decorated input. This defines the Set"name"() and Set"name"Input() method */
#define itkGetDecoratedInputMacro(name, type)                                                                        \
  virtual const SimpleDataObjectDecorator<type> * Get##name##Input() const                                           \
  {                                                                                                                  \
    itkDebugMacro("returning input " << #name " of " << this->ProcessObject::GetInput(#name));                       \
    return itkDynamicCastInDebugMode<const SimpleDataObjectDecorator<type> *>(this->ProcessObject::GetInput(#name)); \
  }                                                                                                                  \
  virtual const type & Get##name() const                                                                             \
  {                                                                                                                  \
    itkDebugMacro("Getting input " #name);                                                                           \
    using DecoratorType = SimpleDataObjectDecorator<type>;                                                           \
    const DecoratorType * input =                                                                                    \
      itkDynamicCastInDebugMode<const DecoratorType *>(this->ProcessObject::GetInput(#name));                        \
    if (input == nullptr)                                                                                            \
    {                                                                                                                \
      itkExceptionMacro("input" #name " is not set");                                                                \
    }                                                                                                                \
    return input->Get();                                                                                             \
  }                                                                                                                  \
  ITK_MACROEND_NOOP_STATEMENT

/** Set a decorated input. This defines the Set"name"() and Set"name"Input() method
 * and Get"name" and Get"name"Input methods */
#define itkSetGetDecoratedInputMacro(name, type) \
  itkSetDecoratedInputMacro(name, type);         \
  itkGetDecoratedInputMacro(name, type)

/** Set a decorated input that derives from itk::Object, but not from
 * itk::DataObject. This defines the Set"name"() and Set"name"Input
 * methods.
 */
#define itkSetDecoratedObjectInputMacro(name, type)                                                           \
  virtual void Set##name##Input(const DataObjectDecorator<type> * _arg)                                       \
  {                                                                                                           \
    itkDebugMacro("setting input " #name " to " << _arg);                                                     \
    if (_arg != itkDynamicCastInDebugMode<DataObjectDecorator<type> *>(this->ProcessObject::GetInput(#name))) \
    {                                                                                                         \
      this->ProcessObject::SetInput(#name, const_cast<DataObjectDecorator<type> *>(_arg));                    \
      this->Modified();                                                                                       \
    }                                                                                                         \
  }                                                                                                           \
  virtual void Set##name(const type * _arg)                                                                   \
  {                                                                                                           \
    using DecoratorType = DataObjectDecorator<type>;                                                          \
    itkDebugMacro("setting input " #name " to " << _arg);                                                     \
    const DecoratorType * oldInput =                                                                          \
      itkDynamicCastInDebugMode<const DecoratorType *>(this->ProcessObject::GetInput(#name));                 \
    if (oldInput && oldInput->Get() == _arg)                                                                  \
    {                                                                                                         \
      return;                                                                                                 \
    }                                                                                                         \
    auto newInput = DecoratorType::New();                                                                     \
    newInput->Set(_arg);                                                                                      \
    this->Set##name##Input(newInput);                                                                         \
  }                                                                                                           \
  ITK_MACROEND_NOOP_STATEMENT

/** Get a decorated input that derives from itk::Object, but not from
 * itk::DataObject. This defines the Get"name"() and Get"name"Input
 * methods.
 */
#define itkGetDecoratedObjectInputMacro(name, type)                                                            \
  virtual const DataObjectDecorator<type> * Get##name##Input() const                                           \
  {                                                                                                            \
    itkDebugMacro("returning input " << #name " of " << this->ProcessObject::GetInput(#name));                 \
    return itkDynamicCastInDebugMode<const DataObjectDecorator<type> *>(this->ProcessObject::GetInput(#name)); \
  }                                                                                                            \
  virtual const type * Get##name() const                                                                       \
  {                                                                                                            \
    itkDebugMacro("Getting input " #name);                                                                     \
    using DecoratorType = DataObjectDecorator<type>;                                                           \
    const DecoratorType * input =                                                                              \
      itkDynamicCastInDebugMode<const DecoratorType *>(this->ProcessObject::GetInput(#name));                  \
    if (input == nullptr)                                                                                      \
    {                                                                                                          \
      return nullptr;                                                                                          \
    }                                                                                                          \
    return input->Get();                                                                                       \
  }                                                                                                            \
  ITK_MACROEND_NOOP_STATEMENT

/** Set a decorated input. This defines the Set"name"() and Set"name"Input() method
 * and Get"name" and Get"name"Input methods */
#define itkSetGetDecoratedObjectInputMacro(name, type) \
  itkSetDecoratedObjectInputMacro(name, type);         \
  itkGetDecoratedObjectInputMacro(name, type)

/** Set built-in type or regular C++ type.  Creates member Set"name"() (e.g., SetVisibility()); */
// clang-format off
#define itkSetMacro(name, type)                     \
  virtual void Set##name(type _arg)                 \
  {                                                 \
    itkDebugMacro("setting " #name " to " << _arg); \
    ITK_GCC_PRAGMA_PUSH                             \
    ITK_GCC_SUPPRESS_Wfloat_equal                   \
    if (this->m_##name != _arg)                     \
    {                                               \
      this->m_##name = std::move(_arg);             \
      this->Modified();                             \
    }                                               \
    ITK_GCC_PRAGMA_POP                              \
  }                                                 \
  ITK_MACROEND_NOOP_STATEMENT
// clang-format on
/** Get built-in type.  Creates member Get"name"() (e.g., GetVisibility()); */
#define itkGetMacro(name, type)                       \
  virtual type Get##name() { return this->m_##name; } \
  ITK_MACROEND_NOOP_STATEMENT

/** Get built-in type.  Creates member Get"name"() (e.g., GetVisibility());
 * This is the "const" form of the itkGetMacro.  It should be used unless
 * the member can be changed through the "Get" access routine. */
#define itkGetConstMacro(name, type)                        \
  virtual type Get##name() const { return this->m_##name; } \
  ITK_MACROEND_NOOP_STATEMENT

/** Get built-in type.  Creates member Get"name"() (e.g., GetVisibility());
 * This is the "const" form of the itkGetMacro.  It should be used unless
 * the member can be changed through the "Get" access routine.
 * This versions returns a const reference to the variable. */
#define itkGetConstReferenceMacro(name, type)                       \
  virtual const type & Get##name() const { return this->m_##name; } \
  ITK_MACROEND_NOOP_STATEMENT

/** Set built-in type.  Creates member Set"name"() (e.g., SetVisibility());
 * This should be used when the type is an enum. It is used to avoid warnings on
 * some compilers with non specified enum types passed to
 * itkDebugMacro. */
#define itkSetEnumMacro(name, type)                                    \
  virtual void Set##name(const type _arg)                              \
  {                                                                    \
    itkDebugMacro("setting " #name " to " << static_cast<long>(_arg)); \
    if (this->m_##name != _arg)                                        \
    {                                                                  \
      this->m_##name = _arg;                                           \
      this->Modified();                                                \
    }                                                                  \
  }                                                                    \
  ITK_MACROEND_NOOP_STATEMENT

/** Get built-in type.  Creates member Get"name"() (e.g., GetVisibility());
 * This should be use when the type is an enum. It is use to avoid warnings on
 * some compilers with non specified enum types passed to
 * itkDebugMacro. */
#define itkGetEnumMacro(name, type)                         \
  virtual type Get##name() const { return this->m_##name; } \
  ITK_MACROEND_NOOP_STATEMENT

/** Set character string.  Creates member Set"name"()
 * (e.g., SetFilename(char *)). The macro assumes that
 * the class member (name) is declared a type std::string. */
#define itkSetStringMacro(name)                                                       \
  virtual void Set##name(const char * _arg)                                           \
  {                                                                                   \
    if (_arg && (_arg == this->m_##name))                                             \
    {                                                                                 \
      return;                                                                         \
    }                                                                                 \
    if (_arg)                                                                         \
    {                                                                                 \
      this->m_##name = _arg;                                                          \
    }                                                                                 \
    else                                                                              \
    {                                                                                 \
      this->m_##name = "";                                                            \
    }                                                                                 \
    this->Modified();                                                                 \
  }                                                                                   \
  virtual void Set##name(const std::string & _arg) { this->Set##name(_arg.c_str()); } \
  ITK_MACROEND_NOOP_STATEMENT


/** Get character string.  Creates member Get"name"()
 * (e.g., SetFilename(char *)). The macro assumes that
 * the class member (name) is declared as a type std::string. */
#define itkGetStringMacro(name)                                             \
  virtual const char * Get##name() const { return this->m_##name.c_str(); } \
  ITK_MACROEND_NOOP_STATEMENT

// clang-format off
/** Set built-in type where value is constrained between min/max limits.
 * Create member Set"name"() (e.q., SetRadius()). \#defines are
 * convenience for clamping open-ended values. */
#define itkSetClampMacro(name, type, min, max)                                  \
  virtual void Set##name(type _arg)                                             \
  {                                                                             \
    const type temp_extrema = (_arg <= min ? min : (_arg >= max ? max : _arg)); \
    itkDebugMacro("setting " << #name " to " << _arg);                          \
    ITK_GCC_PRAGMA_PUSH                                                         \
    ITK_GCC_SUPPRESS_Wfloat_equal                                               \
    if (this->m_##name != temp_extrema)                                         \
    {                                                                           \
      this->m_##name = temp_extrema;                                            \
      this->Modified();                                                         \
    }                                                                           \
    ITK_GCC_PRAGMA_POP                                                          \
  }                                                                             \
  ITK_MACROEND_NOOP_STATEMENT
// clang-format on

// clang-format off
/** Set pointer to object; uses Object reference counting methodology.
 * Creates method Set"name"() (e.g., SetPoints()). Note that using
 * smart pointers requires using raw pointers when setting input. */
#define itkSetObjectMacro(name, type)                  \
  virtual void Set##name(type * _arg)                  \
  {                                                    \
    itkDebugMacro("setting " << #name " to " << _arg); \
    if (this->m_##name != _arg)                        \
    {                                                  \
      this->m_##name = _arg;                           \
      this->Modified();                                \
    }                                                  \
  }                                                    \
  ITK_MACROEND_NOOP_STATEMENT
// clang-format on

/** Get a raw pointer to an object.  Creates the member
 * Get"name"() (e.g., GetPoints()).
 * NOTE:  This function returns a non-const
 * version of the internal member variable
 * and could easily be used to modify the
 * behavior of the class without
 * properly resetting the pipeline
 * semantics */
// NOTE: A class can use either itkGetModifiableObjectMacro
//       or itkGetObjectMacro, but not both.
//       A class can use either itkGetModifiableObjectMacro
//       or itkGetConstObjectMacro, but not both.
//       If the desired behavior is to only provide const
//       access to the itkObject ivar, then use itkGetConstObjectMacro,
//       else use itkGetModifiableObjectMacro for read/write access to
//       the ivar.
//       It is permissible to use both itkGetObjectMacro and itkGetConstObjectMacro
//       for backwards compatibility.
//       If the ITK_LEGACY_REMOVE=FALSE, then it is
//       permissible to use itkGetObjectMacro which
//       defines both signatures itk::GetXXX() and
//       itk::GetModifiableXXX()

/** Get a raw const pointer to an object.  Creates the member
 * Get"name"() (e.g., GetPoints()). */
#define itkGetConstObjectMacro(name, type)                                       \
  virtual const type * Get##name() const { return this->m_##name.GetPointer(); } \
  ITK_MACROEND_NOOP_STATEMENT


#if defined(ITK_FUTURE_LEGACY_REMOVE)
// In the future, the itkGetObjectMacro will be deprecated with the ITK_LEGACY_REMOVE
// flag.  For now, this very advanced feature is only available
// through manual setting of a compiler define -DITK_FUTURE_LEGACY_REMOVE
// ("/DITK_FUTURE_LEGACY_REMOVE /EHsc" with Visual Studio)
// to ease the transition from the historical GetObjectMacro to the GetModifiableObjectMacro
#  define itkGetObjectMacro(name, type)                                                           \
    virtual type * Get##name()                                                                    \
    {                                                                                             \
      purposeful_error("itkGetObjectMacro should be replaced with itkGetModifiableObjectMacro."); \
    }                                                                                             \
    ITK_MACROEND_NOOP_STATEMENT

#  define itkGetModifiableObjectMacro(name, type)                                \
    virtual type * GetModifiable##name() { return this->m_##name.GetPointer(); } \
    itkGetConstObjectMacro(name, type)

#else // defined ( ITK_FUTURE_LEGACY_REMOVE )
/** Get a raw pointer to an object.  Creates the member
 * Get"name"() (e.g., GetPoints()). */
#  define itkGetObjectMacro(name, type)                                \
    virtual type * Get##name() { return this->m_##name.GetPointer(); } \
    ITK_MACROEND_NOOP_STATEMENT
#  define itkGetModifiableObjectMacro(name, type)                                \
    virtual type * GetModifiable##name() { return this->m_##name.GetPointer(); } \
    itkGetConstObjectMacro(name, type);                                          \
    itkGetObjectMacro(name, type)
#endif // defined ( ITK_FUTURE_LEGACY_REMOVE )

// For backwards compatibility define ITK_EXPORT to nothing
#define ITK_EXPORT


/** Get a const reference to a smart pointer to an object.
 * Creates the member Get"name"() (e.g., GetPoints()). */
#define itkGetConstReferenceObjectMacro(name, type)                                   \
  virtual const typename type::Pointer & Get##name() const { return this->m_##name; } \
  ITK_MACROEND_NOOP_STATEMENT

/** Set const pointer to object; uses Object reference counting methodology.
 * Creates method Set"name"() (e.g., SetPoints()). Note that using
 * smart pointers requires using raw pointers when setting input. */
#define itkSetConstObjectMacro(name, type)             \
  virtual void Set##name(const type * _arg)            \
  {                                                    \
    itkDebugMacro("setting " << #name " to " << _arg); \
    if (this->m_##name != _arg)                        \
    {                                                  \
      this->m_##name = _arg;                           \
      this->Modified();                                \
    }                                                  \
  }                                                    \
  ITK_MACROEND_NOOP_STATEMENT

/** Create members "name"On() and "name"Off() (e.g., DebugOn() DebugOff()).
 * Set method must be defined to use this macro. */
#define itkBooleanMacro(name)                          \
  virtual void name##On() { this->Set##name(true); }   \
  virtual void name##Off() { this->Set##name(false); } \
  ITK_MACROEND_NOOP_STATEMENT

// clang-format off
/** General set vector macro creates a single method that copies specified
 * number of values into object.
 * Examples: void SetColor(c,3) */
#define itkSetVectorMacro(name, type, count) \
  virtual void Set##name(type data[])        \
  {                                          \
    unsigned int i;                          \
    for (i = 0; i < count; ++i)              \
    {                                        \
      ITK_GCC_PRAGMA_PUSH                        \
      ITK_GCC_SUPPRESS_Wfloat_equal              \
      if (data[i] != this->m_##name[i])      \
      {                                      \
        break;                               \
      }                                      \
      ITK_GCC_PRAGMA_POP                     \
    }                                        \
    if (i < count)                           \
    {                                        \
      this->Modified();                      \
      for (i = 0; i < count; ++i)            \
      {                                      \
        this->m_##name[i] = data[i];         \
      }                                      \
    }                                        \
  }                                          \
  ITK_MACROEND_NOOP_STATEMENT
// clang-format on

/** Get vector macro. Returns pointer to type (i.e., array of type).
 * This is for efficiency. */
#define itkGetVectorMacro(name, type, count)                  \
  virtual type * Get##name() const { return this->m_##name; } \
  ITK_MACROEND_NOOP_STATEMENT

/**\def itkGPUKernelClassMacro
 * Construct a non-templatized helper class that
 * provides the GPU kernel source code as a const char*
 */
#define itkGPUKernelClassMacro(kernel) class itkGPUKernelMacro(kernel) ITK_MACROEND_NOOP_STATEMENT

/**\def itkGPUKernelMacro
 * Equivalent to the original `itkGPUKernelClassMacro(kernel)` macro, but
 * then without adding the `class` keyword. Useful when an export specifier
 * needs to be added between the `class` keyword and the class name.
 */
#define itkGPUKernelMacro(kernel)          \
  kernel                                   \
  {                                        \
  public:                                  \
    ITK_DISALLOW_COPY_AND_MOVE(kernel);    \
    kernel() = delete;                     \
    ~kernel() = delete;                    \
    static const char * GetOpenCLSource(); \
  }                                        \
  ITK_MACROEND_NOOP_STATEMENT

#define itkGetOpenCLSourceFromKernelMacro(kernel)                             \
  static const char * GetOpenCLSource() { return kernel::GetOpenCLSource(); } \
  ITK_MACROEND_NOOP_STATEMENT

// A useful macro in the PrintSelf method for printing member variables
// which are pointers to object based on the LightObject class.
#define itkPrintSelfObjectMacro(name)                              \
  if (static_cast<const LightObject *>(this->m_##name) == nullptr) \
  {                                                                \
    os << indent << #name << ": (null)" << std::endl;              \
  }                                                                \
  else                                                             \
  {                                                                \
    os << indent << #name << ": " << std::endl;                    \
    this->m_##name->Print(os, indent.GetNextIndent());             \
  }                                                                \
  ITK_MACROEND_NOOP_STATEMENT


// A useful macro in the PrintSelf method for printing boolean member
// variables.
#define itkPrintSelfBooleanMacro(name)                                           \
  os << indent << #name << ": " << (this->m_##name ? "On" : "Off") << std::endl; \
  ITK_MACROEND_NOOP_STATEMENT


/** Set a decorated output. This defines the Set"name"() and a Set"name"Output() method */
#define itkSetDecoratedOutputMacro(name, type)                                                                       \
  virtual void Set##name##Output(const SimpleDataObjectDecorator<type> * _arg)                                       \
  {                                                                                                                  \
    itkDebugMacro("setting output " #name " to " << _arg);                                                           \
    if (_arg != itkDynamicCastInDebugMode<SimpleDataObjectDecorator<type> *>(this->ProcessObject::GetOutput(#name))) \
    {                                                                                                                \
      this->ProcessObject::SetOutput(#name, const_cast<SimpleDataObjectDecorator<type> *>(_arg));                    \
      this->Modified();                                                                                              \
    }                                                                                                                \
  }                                                                                                                  \
  virtual void Set##name(const type & _arg)                                                                          \
  {                                                                                                                  \
    using DecoratorType = SimpleDataObjectDecorator<type>;                                                           \
    itkDebugMacro("setting output " #name " to " << _arg);                                                           \
    DecoratorType * output = itkDynamicCastInDebugMode<DecoratorType *>(this->ProcessObject::GetOutput(#name));      \
    if (output)                                                                                                      \
    {                                                                                                                \
      if (output->Get() == _arg)                                                                                     \
      {                                                                                                              \
        return;                                                                                                      \
      }                                                                                                              \
      else                                                                                                           \
      {                                                                                                              \
        output->Set(_arg);                                                                                           \
      }                                                                                                              \
    }                                                                                                                \
    else                                                                                                             \
    {                                                                                                                \
      auto newOutput = DecoratorType::New();                                                                         \
      newOutput->Set(_arg);                                                                                          \
      this->Set##name##Output(newOutput);                                                                            \
    }                                                                                                                \
  }                                                                                                                  \
  ITK_MACROEND_NOOP_STATEMENT

/** Set a decorated output. This defines the Get"name"() and Get"name"Output() method */
#define itkGetDecoratedOutputMacro(name, type)                                                                        \
  virtual const SimpleDataObjectDecorator<type> * Get##name##Output() const                                           \
  {                                                                                                                   \
    itkDebugMacro("returning output " << #name " of " << this->ProcessObject::GetOutput(#name));                      \
    return itkDynamicCastInDebugMode<const SimpleDataObjectDecorator<type> *>(this->ProcessObject::GetOutput(#name)); \
  }                                                                                                                   \
  virtual const type & Get##name() const                                                                              \
  {                                                                                                                   \
    itkDebugMacro("Getting output " #name);                                                                           \
    using DecoratorType = SimpleDataObjectDecorator<type>;                                                            \
    const DecoratorType * output =                                                                                    \
      itkDynamicCastInDebugMode<const DecoratorType *>(this->ProcessObject::GetOutput(#name));                        \
    if (output == nullptr)                                                                                            \
    {                                                                                                                 \
      itkExceptionMacro("output" #name " is not set");                                                                \
    }                                                                                                                 \
    return output->Get();                                                                                             \
  }                                                                                                                   \
  ITK_MACROEND_NOOP_STATEMENT


#define itkExceptionObject_h
#include "itkExceptionObject.h"
#undef itkExceptionObject_h

/** itkDynamicCastInDebugMode
 * Use static_cast in Release builds, and dynamic_cast in Debug
 *
 * Note: this must come after:
 *
 *   #include "itkExceptionObject.h"
 */
template <typename TTarget, typename TSource>
TTarget
itkDynamicCastInDebugMode(TSource x)
{
#ifndef NDEBUG
  if (x == nullptr)
  {
    return nullptr;
  }
  TTarget rval = dynamic_cast<TTarget>(x);
  if (rval == nullptr)
  {
    itkGenericExceptionMacro("Failed dynamic cast to " << typeid(TTarget).name()
                                                       << " object type = " << x->GetNameOfClass());
  }
  return rval;
#else
  return static_cast<TTarget>(x);
#endif
}

#if __cplusplus >= 202002L
#  define ITK_NODISCARD(message) [[nodiscard(message)]]
#else
#  define ITK_NODISCARD(message) [[nodiscard]]
#endif

//-*-*-*

#if defined(ITK_LEGACY_REMOVE)
#  define itkExposeEnumValue(name) \
    static_assert(false, "ERROR: Replace static_cast<int>(name) with with proper enumeration instead of integer")

#  define ITK_NOEXCEPT_OR_THROW static_assert(false, "Replace ITK_NOEXCEPT_OR_THROW with ITK_NOEXCEPT")

#  define ITK_DELETE_FUNCTION static_assert(false, "ERROR: ITK_DELETE_FUNCTION must be replaced with `= delete`"
#  define ITK_CONSTEXPR_FUNC static_assert(false, "ERROR: ITK_CONSTEXPR_FUNC must be replaced with 'constexpr'")
#  define ITK_CONSTEXPR_VAR static_assert(false, "ERROR: ITK_CONSTEXPR_VAR must be replaced with 'constexpr'")

#  define ITK_FALLTHROUGH static_assert(false, "ERROR: ITK_FALLTHROUGH must be replaced with '[[fallthrough]]'")

// Defines which used to be in itk_compiler_detection.h
#  define ITK_ALIGNAS static_assert(false, "ERROR: ITK_ALIGNAS must be replaced with 'alignas'")
#  define ITK_ALIGNOF static_assert(false, "ERROR: ITK_ALIGNOF must be replaced with 'alignof'")
#  define ITK_DEPRECATED static_assert(false, "ERROR: ITK_DEPRECATED must be replaced with '[[deprecated]]'")
#  define ITK_DEPRECATED_MSG \
    static_assert(false, "ERROR: ITK_DEPRECATED_MSG must be replaced with '[[deprecated(MSG)]]'")
#  define ITK_CONSTEXPR static_assert(false, "ERROR: ITK_CONSTEXPR must be replaced with 'constexpr'")
#  define ITK_DELETED_FUNCTION static_assert(false, "ERROR: ITK_DELETED_FUNCTION must be replaced with '= delete'")
#  define ITK_EXTERN_TEMPLATE static_assert(false, "ERROR: ITK_EXTERN_TEMPLATE must be replaced with 'extern'")
#  define ITK_FINAL static_assert(false, "ERROR: ITK_FINAL must be replaced with 'final'")
#  define ITK_NOEXCEPT static_assert(false, "ERROR: ITK_NOEXCEPT must be replaced with 'noexcept'")
#  define ITK_NOEXCEPT_EXPR static_assert(false, "ERROR: ITK_NOEXCEPT_EXPR must be replaced with 'noexcept'")
#  define ITK_NULLPTR static_assert(false, "ERROR: ITK_NULLPTR must be replaced with 'nullptr'")
#  define ITK_OVERRIDE static_assert(false, "ERROR: ITK_OVERRIDE must be replaced with 'override'")
#  define ITK_STATIC_ASSERT static_assert(false, "ERROR: ITK_STATIC_ASSERT must be replaced with 'static_assert'")
#  define ITK_STATIC_ASSERT_MSG \
    static_assert(false, "ERROR: ITK_STATIC_ASSERT_MSG must be replaced with 'static_assert'")
#  define ITK_THREAD_LOCAL static_assert(false, "ERROR: ITK_THREAD_LOCAL must be replaced with 'thread_local'")

// A macro for methods which are const in ITKv5 and ITKv6 require const for functions
#  define ITKv5_CONST static_assert(false, "ERROR: ITKv5_CONST must be replaced with 'const'")

#  define ITK_ITERATOR_VIRTUAL static_assert(false, "ERROR: ITK_ITERATOR_VIRTUAL must be removed'")
#  define ITK_ITERATOR_OVERRIDE static_assert(false, "ERROR: ITK_ITERATOR_OVERRIDE must be removed")
#  define ITK_ITERATOR_FINAL static_assert(false, "ERROR: ITK_ITERATOR_FINAL must be removed")

#else
// DEPRECATED: These macros are left here for compatibility with remote modules.
// Once they have been removed from all known remote modules, this code should
// be removed.

// Future remove `#define itkExposeEnumValue(name)`
// "Replace type of  `name` with proper enumeration instead of integer.
#  define itkExposeEnumValue(name) static_cast<int>(name)


#  define ITK_NOEXCEPT_OR_THROW ITK_NOEXCEPT

#  define ITK_FALLTHROUGH [[fallthrough]]

#  define ITK_DELETE_FUNCTION = delete

#  define ITK_CONSTEXPR_FUNC constexpr
#  define ITK_CONSTEXPR_VAR constexpr

// Defines which used to be in itk_compiler_detection.h
#  define ITK_ALIGNAS(X) alignas(X)
#  define ITK_ALIGNOF(X) alignof(X)
#  define ITK_DEPRECATED [[deprecated]]
#  define ITK_DEPRECATED_MSG(MSG) [[deprecated(MSG)]]
#  define ITK_CONSTEXPR constexpr
#  define ITK_DELETED_FUNCTION = delete
#  define ITK_EXTERN_TEMPLATE extern
#  define ITK_FINAL final
#  define ITK_NOEXCEPT noexcept
#  define ITK_NOEXCEPT_EXPR(X) noexcept(X)
#  define ITK_NULLPTR nullptr
#  define ITK_OVERRIDE override
#  define ITK_STATIC_ASSERT(X) static_assert(X, #  X)
#  define ITK_STATIC_ASSERT_MSG(X, MSG) static_assert(X, MSG)
#  define ITK_THREAD_LOCAL thread_local

// A macro for methods which are const in after ITKv4
#  define ITKv5_CONST const

#  define ITK_ITERATOR_VIRTUAL  /*purposefully empty for ITKv6, iterators are not virtual for performance reasons*/
#  define ITK_ITERATOR_OVERRIDE /*purposefully empty for ITKv6, iterators are not virtual for performance reasons*/
#  define ITK_ITERATOR_FINAL    /*purposefully empty for ITKv6, iterators are not virtual for performance reasons*/
#endif

#endif // itkMacro_h
