/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMacro.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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

#include <string>

// Determine type of string stream to use.
#if !defined(CMAKE_NO_ANSI_STRING_STREAM)
#  include <sstream>
#elif !defined(CMAKE_NO_ANSI_STREAM_HEADERS)
#  include <strstream>
#  define ITK_NO_ANSI_STRING_STREAM
#else
#  include <strstream.h>
#  define ITK_NO_ANSI_STRING_STREAM
#endif

/** \namespace itk
 * \brief The "itk" namespace contains all Insight Segmentation and
 * Registration Toolkit (ITK) classes. There are several nested namespaces
 * within the itk:: namespace. */
namespace itk
{
} // end namespace itk - this is here for documentation purposes

/** A convenience macro marks variables as not being used by a method,
 * avoiding compile-time warnings. */
#define itkNotUsed(x)

/** Macro to initialize static constants.  This is used frequently to replace
 * the use of enum's within a class.  Some compilers do not allow an enum of
 * one class to be passed as template argument to another class. Other
 * uses of this macro as possible.
 *
 * This is based (verbatim) on the BOOST_STATIC_CONSTANT macro. The original
 * Boost documentation is below.
 *
 * BOOST_STATIC_CONSTANT workaround --------------------------------------- //
 * On compilers which don't allow in-class initialization of static integral
 * constant members, we must use enums as a workaround if we want the constants
 * to be available at compile-time. This macro gives us a convenient way to
 * declare such constants.
 */
#if defined(_MSC_VER) && (_MSC_VER <= 1300) 
#   define ITK_NO_INCLASS_MEMBER_INITIALIZATION
#endif
#if defined(__SUNPRO_CC) && (__SUNPRO_CC <= 0x540)
#   define ITK_NO_INCLASS_MEMBER_INITIALIZATION
#endif
#if defined(__SVR4) && !defined(__SUNPRO_CC)
#   define ITK_NO_INCLASS_MEMBER_INITIALIZATION
#endif

// A class template like this will not instantiate on GCC 2.95:
//   template<class T> struct A
//   {
//     static const int N = 1;
//     enum { S = sizeof(A::N) };
//   };
// We need to use enum for static constants instead.
#if defined(__GNUC__) && __GNUC__ < 3
# define ITK_NO_SIZEOF_CONSTANT_LOOKUP
#endif

#if defined(_MSC_VER) && (_MSC_VER <= 1300) 
#define ITK_NO_SELF_AS_TRAIT_IN_TEMPLATE_ARGUMENTS
#endif

#if defined(ITK_NO_INCLASS_MEMBER_INITIALIZATION) || \
    defined(ITK_NO_SIZEOF_CONSTANT_LOOKUP)
#   define itkStaticConstMacro(name,type,value) enum { name = value }
#else
#   define itkStaticConstMacro(name,type,value) static const type name = value
#endif

#ifdef ITK_NO_SELF_AS_TRAIT_IN_TEMPLATE_ARGUMENTS
#   define itkGetStaticConstMacro(name) name
#else
#   define itkGetStaticConstMacro(name) (Self::name)
#endif

/** Set an input. This defines the Set"name"Input() method */
#define itkSetInputMacro(name, type, number) \
  virtual void Set##name##Input(const type *_arg) \
  { \
    itkDebugMacro("setting input " #name " to " << _arg); \
    if (_arg != static_cast<type *>(this->ProcessObject::GetInput( number ))) \
      { \
      this->ProcessObject::SetNthInput( number, const_cast<type *>(_arg) ); \
      this->Modified(); \
      } \
  } \
  virtual void SetInput##number(const type *_arg) \
  { \
    itkDebugMacro("setting input " #number " to " << _arg); \
    if (_arg != static_cast<type *>(this->ProcessObject::GetInput( number ))) \
      { \
      this->ProcessObject::SetNthInput( number, const_cast<type *>(_arg) ); \
      this->Modified(); \
      } \
  } 


/** Get an input. This defines the Get"name"Input() method */
#define itkGetInputMacro(name, type, number) \
  virtual const type * Get##name##Input() const \
  { \
    itkDebugMacro("returning input " << #name " of " << static_cast<const type *>(this->ProcessObject::GetInput( number )) ); \
    return static_cast<const type *>(this->ProcessObject::GetInput( number )); \
  } \
  virtual const type * GetInput##number() const \
  { \
    itkDebugMacro("returning input " << #number " of " << static_cast<const type *>(this->ProcessObject::GetInput( number )) ); \
    return static_cast<const type *>(this->ProcessObject::GetInput( number )); \
  } 


/** Set built-in type.  Creates member Set"name"() (e.g., SetVisibility()); */
#define itkSetMacro(name,type) \
  virtual void Set##name (const type _arg) \
  { \
    itkDebugMacro("setting " #name " to " << _arg); \
    if (this->m_##name != _arg) \
      { \
      this->m_##name = _arg; \
      this->Modified(); \
      } \
  } 

/** Get built-in type.  Creates member Get"name"() (e.g., GetVisibility()); */
#define itkGetMacro(name,type) \
  virtual type Get##name () \
  { \
    itkDebugMacro("returning " << #name " of " << this->m_##name ); \
    return this->m_##name; \
  }

/** Get built-in type.  Creates member Get"name"() (e.g., GetVisibility());
 * This is the "const" form of the itkGetMacro.  It should be used unless
 * the member can be changed through the "Get" access routine. */
#define itkGetConstMacro(name,type) \
  virtual type Get##name () const \
  { \
    itkDebugMacro("returning " << #name " of " << this->m_##name ); \
    return this->m_##name; \
  }

/** Get built-in type.  Creates member Get"name"() (e.g., GetVisibility());
 * This is the "const" form of the itkGetMacro.  It should be used unless
 * the member can be changed through the "Get" access routine.
 * This versions returns a const reference to the variable. */
#define itkGetConstReferenceMacro(name,type) \
  virtual const type & Get##name () const \
  { \
    itkDebugMacro("returning " << #name " of " << this->m_##name ); \
    return this->m_##name; \
  }

/** Set character string.  Creates member Set"name"() 
 * (e.g., SetFilename(char *)). The macro assumes that
 * the class member (name) is declared a type std::string. */
#define itkSetStringMacro(name) \
  virtual void Set##name (const char* _arg) \
  { \
    if ( _arg && (_arg == this->m_##name) ) { return;} \
    if (_arg) \
      { \
      this->m_##name = _arg;\
      } \
     else \
      { \
      this->m_##name = ""; \
      } \
    this->Modified(); \
  } 


/** Get character string.  Creates member Get"name"() 
 * (e.g., SetFilename(char *)). The macro assumes that
 * the class member (name) is declared a type std::string. */
#define itkGetStringMacro(name) \
  virtual const char* Get##name () const \
  { \
    return this->m_##name.c_str(); \
  } 

/** Set built-in type where value is constrained between min/max limits.
 * Create member Set"name"() (e.q., SetRadius()). #defines are 
 * convienience for clamping open-ended values. */
#define itkSetClampMacro(name,type,min,max) \
  virtual void Set##name (type _arg) \
  { \
    itkDebugMacro("setting " << #name " to " << _arg ); \
    if (this->m_##name != (_arg<min?min:(_arg>max?max:_arg))) \
      { \
      this->m_##name = (_arg<min?min:(_arg>max?max:_arg)); \
      this->Modified(); \
      } \
  } 

/** Set pointer to object; uses Object reference counting methodology.
 * Creates method Set"name"() (e.g., SetPoints()). Note that using
 * smart pointers requires using real pointers when setting input,
 * but returning smart pointers on output. */
#define itkSetObjectMacro(name,type) \
  virtual void Set##name (type* _arg) \
  { \
    itkDebugMacro("setting " << #name " to " << _arg ); \
    if (this->m_##name != _arg) \
      { \
      this->m_##name = _arg; \
      this->Modified(); \
      } \
  } 

/** Get a smart pointer to an object.  Creates the member 
 * Get"name"() (e.g., GetPoints()). */
#define itkGetObjectMacro(name,type) \
  virtual type * Get##name () \
  { \
    itkDebugMacro("returning " #name " address " << this->m_##name ); \
    return this->m_##name.GetPointer(); \
  } 

/** Set const pointer to object; uses Object reference counting methodology.
 * Creates method Set"name"() (e.g., SetPoints()). Note that using
 * smart pointers requires using real pointers when setting input,
 * but returning smart pointers on output. */
#define itkSetConstObjectMacro(name,type) \
  virtual void Set##name (const type* _arg) \
  { \
    itkDebugMacro("setting " << #name " to " << _arg ); \
    if (this->m_##name != _arg) \
      { \
      this->m_##name = _arg; \
      this->Modified(); \
      } \
  } 


/** Get a smart const pointer to an object.  Creates the member 
 * Get"name"() (e.g., GetPoints()). */
#define itkGetConstObjectMacro(name,type) \
  virtual const type * Get##name () const \
  { \
    itkDebugMacro("returning " #name " address " << this->m_##name ); \
    return this->m_##name.GetPointer(); \
  } 

/** Get a const reference to a smart pointer to an object.  
 * Creates the member Get"name"() (e.g., GetPoints()). */
#define itkGetConstReferenceObjectMacro(name,type) \
  virtual const typename type::Pointer & Get##name () const \
  { \
    itkDebugMacro("returning " #name " address " << this->m_##name ); \
    return this->m_##name; \
  } 

/** Create members "name"On() and "name"Off() (e.g., DebugOn() DebugOff()).
 * Set method must be defined to use this macro. */
#define itkBooleanMacro(name) \
  virtual void name##On () { this->Set##name(true);} \
  virtual void name##Off () { this->Set##name(false);}

/** General set vector macro creates a single method that copies specified
 * number of values into object.
 * Examples: void SetColor(c,3) */
#define itkSetVectorMacro(name,type,count) \
  virtual void Set##name(type data[]) \
  { \
    unsigned int i; \
    for (i=0; i<count; i++) { if ( data[i] != this->m_##name[i] ) { break; }} \
    if ( i < count ) \
      { \
      this->Modified(); \
      for (i=0; i<count; i++) { this->m_##name[i] = data[i]; }\
      } \
  }

/** Get vector macro. Returns pointer to type (i.e., array of type).
 * This is for efficiency. */
#define itkGetVectorMacro(name,type,count) \
  virtual type *Get##name () const \
  { \
    return this->m_##name; \
  } 

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
 * initialization of process objects and data objects cycles). */
#define itkNewMacro(x) \
static Pointer New(void) \
{ \
  Pointer smartPtr; \
  x *rawPtr = ::itk::ObjectFactory<x>::Create(); \
  if(rawPtr == NULL) \
    { \
    rawPtr = new x; \
    } \
  smartPtr = rawPtr; \
  rawPtr->UnRegister(); \
  return smartPtr; \
} \
virtual ::itk::LightObject::Pointer CreateAnother(void) const \
{ \
  ::itk::LightObject::Pointer smartPtr; \
  smartPtr = x::New().GetPointer(); \
  return smartPtr; \
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
#define itkFactorylessNewMacro(x) \
static Pointer New(void) \
{ \
  Pointer smartPtr; \
  x *rawPtr = new x; \
  smartPtr = rawPtr; \
  rawPtr->UnRegister(); \
  return smartPtr; \
} \
virtual LightObject::Pointer CreateAnother(void) const \
{ \
  LightObject::Pointer smartPtr; \
  smartPtr = x::New().GetPointer(); \
  return smartPtr; \
}

/** Macro used to add standard methods to all classes, mainly type
 * information. */
#define itkTypeMacro(thisClass,superclass) \
    virtual const char *GetNameOfClass() const \
        {return #thisClass;} 


namespace itk
{
/** 
 * The following is used to output debug, warning, and error messages. 
 * Use a global function which actually calls:
 * OutputWindow::GetInstance()->DisplayText();
 * This is to avoid Object #include of OutputWindow
 * while OutputWindow #includes Object. */
extern ITKCommon_EXPORT void OutputWindowDisplayText(const char*);
extern ITKCommon_EXPORT void OutputWindowDisplayErrorText(const char*);
extern ITKCommon_EXPORT void OutputWindowDisplayWarningText(const char*);
extern ITKCommon_EXPORT void OutputWindowDisplayGenericOutputText(const char*);
extern ITKCommon_EXPORT void OutputWindowDisplayDebugText(const char*);
} // end namespace itk

/** This macro is used to print debug (or other information). They are
 * also used to catch errors, etc. Example usage looks like:
 * itkDebugMacro(<< "this is debug info" << this->SomeVariable); */
#if defined(ITK_LEAN_AND_MEAN) || defined(__BORLANDC__)
#define itkDebugMacro(x)
#else
#define itkDebugMacro(x) \
  { if (this->GetDebug() && ::itk::Object::GetGlobalWarningDisplay())   \
    { ::itk::OStringStream itkmsg; \
      itkmsg << "Debug: In " __FILE__ ", line " << __LINE__ << "\n" \
             << this->GetNameOfClass() << " (" << this << "): " x  \
             << "\n\n"; \
      ::itk::OutputWindowDisplayDebugText(itkmsg.str().c_str());} \
}
#endif


/** This macro is used to print warning information (i.e., unusual circumstance
 * but not necessarily fatal.) Example usage looks like:
 * itkWarningMacro(<< "this is warning info" << this->SomeVariable); */
#ifdef ITK_LEAN_AND_MEAN
#define itkWarningMacro(x)
#else
#define itkWarningMacro(x) \
{ if (itk::Object::GetGlobalWarningDisplay()) \
    { ::itk::OStringStream itkmsg; \
      itkmsg << "WARNING: In " __FILE__ ", line " << __LINE__ << "\n" \
             << this->GetNameOfClass() << " (" << this << "): " x  \
             << "\n\n"; \
      itk::OutputWindowDisplayWarningText(itkmsg.str().c_str());} \
}
#endif

namespace itk
{

/**
 * itk::OStringStream wrapper to hide differences between
 * std::ostringstream and the old ostrstream.  Necessary for
 * portability.
 */
#if !defined(ITK_NO_ANSI_STRING_STREAM)
class OStringStream: public std::ostringstream
{
public:
  OStringStream() {}
private:
  OStringStream(const OStringStream&);
  void operator=(const OStringStream&);
};
#else
namespace OStringStreamDetail
{
  class Cleanup
  {
  public:
    Cleanup(std::ostrstream& ostr): m_OStrStream(ostr) {}
    ~Cleanup() { m_OStrStream.rdbuf()->freeze(0); }
    static void IgnoreUnusedVariable(const Cleanup&) {}
  protected:
    std::ostrstream& m_OStrStream;
  };
}//namespace OStringStreamDetail

class OStringStream: public std::ostrstream
{
public:
  typedef std::ostrstream Superclass;
  OStringStream() {}
  std::string str()
    {
      OStringStreamDetail::Cleanup cleanup(*this);
      OStringStreamDetail::Cleanup::IgnoreUnusedVariable(cleanup);
      int pcount = this->pcount();
      const char* ptr = this->Superclass::str();
      return std::string(ptr?ptr:"", pcount);
    }
private:
  OStringStream(const OStringStream&);
  void operator=(const OStringStream&);
};
#endif

}//namespace itk

#if defined(ITK_CPP_FUNCTION)
  #if defined(__BORLANDC__)
    #define ITK_LOCATION __FUNC__
  #elif defined(_WIN32) && !defined(__MINGW32__) && !defined(__CYGWIN__)
    #define ITK_LOCATION __FUNCSIG__
  #elif defined(__GNUC__)
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
#define itkExceptionMacro(x) \
  { \
  ::itk::OStringStream message; \
  message << "itk::ERROR: " << this->GetNameOfClass() \
          << "(" << this << "): " x; \
  ::itk::ExceptionObject e_(__FILE__, __LINE__, message.str().c_str(),ITK_LOCATION); \
  throw e_; /* Explicit naming to work around Intel compiler bug.  */ \
  }

#define itkGenericExceptionMacro(x) \
  { \
  ::itk::OStringStream message; \
  message << "itk::ERROR: " x; \
  ::itk::ExceptionObject e_(__FILE__, __LINE__, message.str().c_str(),ITK_LOCATION); \
  throw e_; /* Explicit naming to work around Intel compiler bug.  */ \
  }

#ifdef ITK_LEAN_AND_MEAN
#define itkGenericOutputMacro(x)
#else
#define itkGenericOutputMacro(x) \
{ if (::itk::Object::GetGlobalWarningDisplay()) \
    { ::itk::OStringStream itkmsg; \
      itkmsg << "WARNING: In " __FILE__ ", line " << __LINE__ << "\n" \
             x << "\n\n"; \
      itk::OutputWindowDisplayGenericOutputText(itkmsg.str().c_str());} \
}
#endif



//----------------------------------------------------------------------------
// Macros for simplifying the use of logging
//
#define itkLogMacro( x, y)  \
{         \
  if (this->GetLogger() ) \
    {  \
    this->GetLogger()->Write(::itk::LoggerBase::x, y); \
    }  \
}


#define itkLogMacroStatic( obj, x, y)  \
{         \
  if (obj->GetLogger() ) \
    {  \
    obj->GetLogger()->Write(::itk::LoggerBase::x, y); \
    }  \
}


//----------------------------------------------------------------------------
// Setup legacy code policy.

// Define itkLegacy macro to mark legacy methods where they are
// declared in their class.  Example usage:
//
//   // @deprecated Replaced by MyOtherMethod() as of ITK 2.0.
//   itkLegacy(void MyMethod());
#if defined(ITK_LEGACY_REMOVE)
  // Remove legacy methods completely.
# define itkLegacy(method)
#elif defined(ITK_LEGACY_SILENT) || defined(ITK_WRAPPING_CXX)
  // Provide legacy methods with no warnings.
# define itkLegacy(method) method
#else
  // Setup compile-time warnings for uses of deprecated methods if
  // possible on this compiler.
# if defined(__GNUC__) && !defined(__INTEL_COMPILER) && (__GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 1))
#  define itkLegacy(method) method __attribute__((deprecated))
# elif defined(_MSC_VER) && _MSC_VER >= 1300
#  define itkLegacy(method) __declspec(deprecated) method
# else
#  define itkLegacy(method) method
# endif
#endif

// Macros to create runtime deprecation warning messages in function
// bodies.  Example usage:
//
//   void itkMyClass::MyOldMethod()
//   {
//     itkLegacyBody(itkMyClass::MyOldMethod, 2.0);
//   }
//
//   void itkMyClass::MyMethod()
//   {
//     itkLegacyReplaceBody(itkMyClass::MyMethod, 2.0,
//                          itkMyClass::MyOtherMethod);
//   }
#if defined(ITK_LEGACY_REMOVE) || defined(ITK_LEGACY_SILENT)
# define itkLegacyBody(method, version)
# define itkLegacyReplaceBody(method, version, replace)
#else
# define itkLegacyBody(method, version) \
  itkWarningMacro(#method " was deprecated for ITK " #version " and will be removed in a future version.")
# define itkLegacyReplaceBody(method, version, replace) \
  itkWarningMacro(#method " was deprecated for ITK " #version " and will be removed in a future version.  Use " #replace " instead.")
#endif

#if defined(__INTEL_COMPILER)
# pragma warning (disable: 193) /* #if testing undefined identifier */
#endif

//=============================================================================
/* Choose a way to prevent template instantiation on this platform.
  - ITK_TEMPLATE_DO_NOT_INSTANTIATE = use #pragma do_not_instantiate to
                                      prevent instantiation
  - ITK_TEMPLATE_EXTERN = use extern template to prevent instantiation

   Note that VS 6 supports extern template instantiation but it is
   hard to block the resulting warning because its stream headers
   re-enable it.  Therefore we just disable support for now.
*/
#if defined(__sgi) && defined(_COMPILER_VERSION)
# define ITK_TEMPLATE_DO_NOT_INSTANTIATE 1
#elif defined(__INTEL_COMPILER) && __INTEL_COMPILER >= 700
# define ITK_TEMPLATE_EXTERN 1
#elif defined(__GNUC__) && __GNUC__ >= 3
# define ITK_TEMPLATE_EXTERN 1
#elif defined(_MSC_VER) && _MSC_VER >= 1300
# define ITK_TEMPLATE_EXTERN 1
#endif
#if !defined(ITK_TEMPLATE_DO_NOT_INSTANTIATE)
# define ITK_TEMPLATE_DO_NOT_INSTANTIATE 0
#endif
#if !defined(ITK_TEMPLATE_EXTERN)
# define ITK_TEMPLATE_EXTERN 0
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
# define ITK_TEMPLATE_IMPORT_DELAY(x) extern template ITK_TEMPLATE_##x;
# if defined(_MSC_VER)
#  pragma warning (disable: 4231) /* extern template extension */
# endif
#elif ITK_TEMPLATE_DO_NOT_INSTANTIATE
# define ITK_TEMPLATE_IMPORT_DELAY(x) \
         ITK_TEMPLATE_IMPORT_IMPL(do_not_instantiate ITK_TEMPLATE_##x)
# define ITK_TEMPLATE_IMPORT_IMPL(x) _Pragma(#x)
#endif
#if defined(ITK_TEMPLATE_IMPORT_DELAY)
# define ITK_TEMPLATE_IMPORT(x) ITK_TEMPLATE_IMPORT_DELAY(x)
# define ITK_TEMPLATE_IMPORT_WORKS 1
#else
# define ITK_TEMPLATE_IMPORT(x)
# define ITK_TEMPLATE_IMPORT_WORKS 0
#endif

/* Define macros to export and import template instantiations.  These
   depend on each class providing a macro defining the instantiations
   given template arguments in X.  The argument X is of the form
   N(a1[,a2...,aN]).  The argument Y is a valid preprocessing token
   unique to the template arguments given in X.  Typical usage is

     ITK_EXPORT_TEMPLATE(itkfoo_EXPORT, Foo, (int), I)
     ITK_EXPORT_TEMPLATE(itkfoo_EXPORT, Bar, (int, char), IC)

   The ITK_TEMPLATE_<name> macro should be defined in itk<name>.h and
   is of the following form:

     #define ITK_TEMPLATE_<name>(_, EXPORT, x, y) namespace itk { \
       _(<n>(class EXPORT <name>< ITK_TEMPLATE_<n> x >)) \
       namespace Templates { typedef <name>< ITK_TEMPLATE_<n> x > <name>##y; }\
     }

   The argument "_" will be replaced by another macro such as
   ITK_TEMPLATE_EXPORT or ITK_TEMPLATE_IMPORT, so it should be used as
   if calling one of these macros.  The argument "EXPORT" will be
   replaced by a dllexport/dllimport macro such as ITKCommon_EXPORT.
   The argument "x" is a paren-enclosed list of template arguments.
   The argument "y" is a preprocessing token corresponding to the
   given template arguments and should be used to construct typedef
   names for the instantiations.

   Note the use of ITK_TEMPLATE_<n>, where <n> is the number of
   template arguments for the class template.  Note also that the
   number of template arguments is usually the length of the list
   nested within the inner parentheses, so the instantiation is listed
   with the form <n>(...).  Example definitions:

     #define ITK_TEMPLATE_Foo(_, EXPORT, x, y) namespace itk { \
       _(1(class EXPORT Foo< ITK_TEMPLATE_1 x >)) \
       _(1(EXPORT std::ostream& operator<<(std::ostream&, \
                                           const Foo< ITK_TEMPLATE_1 x >&))) \
       namespace Templates { typedef Foo< ITK_TEMPLATE_1 x > Foo##y; }\
     }

     #define ITK_TEMPLATE_Bar(_, EXPORT, x, y) namespace itk { \
       _(2(class EXPORT Bar< ITK_TEMPLATE_2 x >)) \
       _(1(EXPORT std::ostream& operator<<(std::ostream&, \
                                           const Bar< ITK_TEMPLATE_2 x >&))) \
       namespace Templates { typedef Bar< ITK_TEMPLATE_2 x > Bar##y; }\
     }

   Note that in the stream operator for template Bar there is a "1" at
   the beginning even though two arguments are taken.  This is because
   the expression "ITK_TEMPLATE_2 x" is contained inside the
   parentheses of the function signature which protects the resulting
   comma from separating macro arguments.  Therefore the nested
   parentheses contain a list of only one macro argument.

   The ITK_EMPTY macro used in these definitions is a hack to work
   around a VS 6.0 preprocessor bug when EXPORT is empty.
*/
#define ITK_EXPORT_TEMPLATE(EXPORT, c, x, y) \
        ITK_TEMPLATE_##c(ITK_TEMPLATE_EXPORT, EXPORT ITK_EMPTY, x, y)
#define ITK_IMPORT_TEMPLATE(EXPORT, c, x, y) \
        ITK_TEMPLATE_##c(ITK_TEMPLATE_IMPORT, EXPORT ITK_EMPTY, x, y)
#define ITK_EMPTY

/* Define macros to support passing a variable number of arguments
   throug other macros.  This is used by ITK_TEMPLATE_EXPORT,
   ITK_TEMPLATE_IMPORT, and by each template's instantiation
   macro.  */
#define ITK_TEMPLATE_1(x1)                         x1
#define ITK_TEMPLATE_2(x1,x2)                      x1,x2
#define ITK_TEMPLATE_3(x1,x2,x3)                   x1,x2,x3
#define ITK_TEMPLATE_4(x1,x2,x3,x4)                x1,x2,x3,x4
#define ITK_TEMPLATE_5(x1,x2,x3,x4,x5)             x1,x2,x3,x4,x5
#define ITK_TEMPLATE_6(x1,x2,x3,x4,x5,x6)          x1,x2,x3,x4,x5,x6
#define ITK_TEMPLATE_7(x1,x2,x3,x4,x5,x6,x7)       x1,x2,x3,x4,x5,x6,x7
#define ITK_TEMPLATE_8(x1,x2,x3,x4,x5,x6,x7,x8)    x1,x2,x3,x4,x5,x6,x7,x8
#define ITK_TEMPLATE_9(x1,x2,x3,x4,x5,x6,x7,x8,x9) x1,x2,x3,x4,x5,x6,x7,x8,x9

/* In order to support both implicit and explicit instantation a .h
   file needs to know whether it should include its .txx file
   containing the template definitions.  Define a macro to tell
   it.  Typical usage in itkFoo.h:
     #if ITK_TEMPLATE_TXX
     # include "itkFoo.txx"
     #endif
*/
#if defined(ITK_MANUAL_INSTANTIATION)
# define ITK_TEMPLATE_TXX 0
#else
# define ITK_TEMPLATE_TXX !(ITK_TEMPLATE_CXX || ITK_TEMPLATE_TYPE)
#endif

/* All explicit instantiation source files define ITK_TEMPLATE_CXX.
   Define ITK_MANUAL_INSTANTIATION to tell .h files that have not been
   converted to this explicit instantiation scheme to not include
   their .txx files.  Also disable warnings that commonly occur in
   these files but are not useful.  */
#if ITK_TEMPLATE_CXX
# undef ITK_MANUAL_INSTANTIATION
# define ITK_MANUAL_INSTANTIATION
# if defined(_MSC_VER)
#  pragma warning (disable: 4275) /* non dll-interface base */
#  pragma warning (disable: 4661) /* no definition available */
# endif
#endif
//=============================================================================

/* Define macros to export and import template instantiations for each
   library in ITK.  */
#define ITK_EXPORT_ITKCommon(c, x, n) \
        ITK_EXPORT_TEMPLATE(ITKCommon_EXPORT, c, x, n)
#define ITK_IMPORT_ITKCommon(c, x, n) \
        ITK_IMPORT_TEMPLATE(ITKCommon_EXPORT, c, x, n)

/* Define a macro to decide whether to block instantiation of ITK
   templates.  They should be blocked only if the platform supports
   blocking template instantiation and the explicit instantiations are
   available.

   - ITK_TEMPLATE_EXPLICIT =
      Whether to include "XXX+-.h" from "XXX.h" to prevent implicit
      instantiations of templates explicitly instantiated elsewhere.
      Typical usage in itkFoo.h:
        #if ITK_TEMPLATE_EXPLICIT
        # include "itkFoo+-.h"
        #endif
*/
#if ITK_TEMPLATE_IMPORT_WORKS && defined(ITK_EXPLICIT_INSTANTIATION)
# define ITK_TEMPLATE_EXPLICIT !ITK_TEMPLATE_CXX
#else
# define ITK_TEMPLATE_EXPLICIT 0
#endif

#endif //end of itkMacro.h
