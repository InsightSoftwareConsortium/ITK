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

//This is probably better, but requires a lot of extra work
//for gettting ExplicitInstantiation to work properly. \#define
// itkStaticConstMacro(name, type, value) static const type name = value
#define itkStaticConstMacro(name, type, value) enum { name = value }

#define itkGetStaticConstMacro(name) (Self::name)

/** Set an input. This defines the Set"name"Input() method */
#define itkSetInputMacro(name, type, number)                                      \
  virtual void Set##name##Input(const type *_arg)                             \
    {                                                                             \
    itkDebugMacro("setting input " #name " to " << _arg);                        \
    if ( _arg != static_cast< type * >( this->ProcessObject::GetInput(number) ) ) \
      {                                                                           \
      this->ProcessObject::SetNthInput( number, const_cast< type * >( _arg ) );   \
      this->Modified();                                                           \
      }                                                                           \
    }                                                                             \
  virtual void SetInput##number(const type * _arg)                              \
    {                                                                             \
    itkDebugMacro("setting input " #number " to " << _arg);                      \
    if ( _arg != static_cast< type * >( this->ProcessObject::GetInput(number) ) ) \
      {                                                                           \
      this->ProcessObject::SetNthInput( number, const_cast< type * >( _arg ) );   \
      this->Modified();                                                           \
      }                                                                           \
    }

/** Macro used to redefine a type from the superclass. */
#define itkSuperclassTraitMacro(traitnameType) \
  typedef typename Superclass::traitnameType traitnameType;

/** Get an input. This defines the Get"name"Input() method */
#define itkGetInputMacro(name, type, number)                                                                     \
  virtual const type * Get##name##Input() const                                                              \
    {                                                                                                            \
    return static_cast< const type * >( this->ProcessObject::GetInput(number) );                                 \
    }                                                                                                            \
  virtual const type *GetInput##number() const                                                                 \
    {                                                                                                            \
    return static_cast< const type * >( this->ProcessObject::GetInput(number) );                                 \
    }

/** Set a decorated input. This defines the Set"name"() and Get"name"() methods,
 * in addition to the Set"name"Input() and Get"name"Input() defined by invoking
 * SetInputMacro() and GetInputMacro() for the decorated object */
#define itkSetDecoratedInputMacro(name, type, number)                \
  itkSetInputMacro(name, SimpleDataObjectDecorator< type >, number); \
  itkGetInputMacro(name, SimpleDataObjectDecorator< type >, number); \
  virtual void Set##name(const type &_arg)                         \
    {                                                                \
    typedef SimpleDataObjectDecorator< type > DecoratorType;         \
    itkDebugMacro("setting input " #name " to " << _arg);           \
    const DecoratorType *oldInput =                                  \
      static_cast< const DecoratorType * >(                          \
        this->ProcessObject::GetInput(number) );                     \
    if ( oldInput && oldInput->Get() == _arg )                       \
      {                                                              \
      return;                                                        \
      }                                                              \
    typename DecoratorType::Pointer newInput = DecoratorType::New(); \
    newInput->Set(_arg);                                             \
    this->Set##name##Input(newInput);                            \
    }                                                                \
  virtual const type & Get##name() const                             \
    {                                                                \
    typedef SimpleDataObjectDecorator< type > DecoratorType;         \
    const DecoratorType *input =                                     \
      static_cast< const DecoratorType * >(                          \
        this->ProcessObject::GetInput(number) );                     \
    if( input == NULL )                                              \
      {                                                              \
      itkExceptionMacro(<<"input" #name " is not set");              \
      }                                                              \
    return input->Get();                                             \
    }


/** Set a decorated input that derives from itk::Object, but not from
 * itk::DataObject. This defines the Set"name"() method.  It invokes
 * SetInputMacro() and GetInputMacro() for the decorated object */
#define itkSetDecoratedObjectInputMacro(name, type, number)          \
  itkSetInputMacro(name, DataObjectDecorator< type >, number);       \
  itkGetInputMacro(name, DataObjectDecorator< type >, number);       \
  virtual void Set##name(const type * _arg)                        \
    {                                                                \
    typedef DataObjectDecorator< type > DecoratorType;               \
    itkDebugMacro("setting input " #name " to " << _arg);           \
    const DecoratorType *oldInput =                                  \
      static_cast< const DecoratorType * >(                          \
        this->ProcessObject::GetInput(number) );                     \
    if ( oldInput && oldInput->Get() == _arg )                       \
      {                                                              \
      return;                                                        \
      }                                                              \
    typename DecoratorType::Pointer newInput = DecoratorType::New(); \
    newInput->Set(_arg);                                             \
    this->Set##name##Input(newInput);                            \
    }

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
 * Get"name"() (e.g., GetPoints()). */
#define itkGetObjectMacro(name, type)                                   \
  virtual type * Get##name ()                                         \
    {                                                                   \
    return this->m_##name.GetPointer();                               \
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

/** Get a smart const pointer to an object.  Creates the member
 * Get"name"() (e.g., GetPoints()). */
#define itkGetConstObjectMacro(name, type)                              \
  virtual const type * Get##name () const                             \
    {                                                                   \
    return this->m_##name.GetPointer();                               \
    }

/** Get a const reference to a smart pointer to an object.
 * Creates the member Get"name"() (e.g., GetPoints()). */
#define itkGetConstReferenceObjectMacro(name, type)                     \
  virtual const typename type::Pointer & Get##name () const             \
    {                                                                   \
    return this->m_##name;                                              \
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
  itkCreateAnotherMacro(x)

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
#if defined( ITK_LEAN_AND_MEAN_TEST_RENAME_TO_INVESTIGATE_REMOVAL_OPTIONS ) || defined( NDEBUG )
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

//The itkDebugStatement is to be used ot protect code that is only
//used in the itkDebugMacro
#define itkDebugStatement(x) x
#endif

/** This macro is used to print warning information (i.e., unusual circumstance
 * but not necessarily fatal.) Example usage looks like:
 * itkWarningMacro(<< "this is warning info" << this->SomeVariable); */
#ifdef ITK_LEAN_AND_MEAN_TEST_RENAME_TO_INVESTIGATE_REMOVAL_OPTIONS
#define itkWarningMacro(x)
#define itkWarningStatement(x)
#else
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
#endif

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
class ITK_EXPORT newexcp : public parentexcp                                            \
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


#ifdef ITK_LEAN_AND_MEAN_TEST_RENAME_TO_INVESTIGATE_REMOVAL_OPTIONS
#define itkGenericOutputMacro(x)
#else
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
#endif

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
// Remove legacy methods completely.  Put a bogus declaration in
// place to avoid stray semicolons because this is an error for some
// compilers.  Using a class forward declaration allows any number
// of repeats in any context without generating unique names.
#define itkLegacyMacro(method) class itkLegacyMethodRemoved /* no ';' */
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

#if defined( __INTEL_COMPILER )
#pragma warning (disable: 193) /* #if testing undefined identifier */
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

   Note that VS 6 supports extern template instantiation but it is
   hard to block the resulting warning because its stream headers
   re-enable it.  Therefore we just disable support for now.
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
#if defined( _MSC_VER )
#pragma warning (disable: 4231) /* extern template extension */
#endif
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

/** Define macros to export and import template instantiations.  These
   depend on each class providing a macro defining the instantiations
   given template arguments in X.  The argument X is of the form
   N(a1[,a2...,aN]).  The argument Y is a valid preprocessing token
   unique to the template arguments given in X.  Typical usage is

     ITK_EXPORT_TEMPLATE(itkfoo_EXPORT, Foo, (int), I)
     ITK_EXPORT_TEMPLATE(itkfoo_EXPORT, Bar, (int, char), IC)

   The ITK_TEMPLATE_\<name\> macro should be defined in itk\<name\>.h and
   is of the following form:

     \#define ITK_TEMPLATE_\<name\>(_, EXPORT, TypeX, TypeY) \
    namespace itk { \
       _(\<n\>(class EXPORT \<name\>< ITK_TEMPLATE_<n> x >)) \
       namespace Templates { \
    typedef \<name\>\< ITK_TEMPLATE_\<n\> x \> \<name\>\#\#TypeY; \
   }\
     }

   The argument "_" will be replaced by another macro such as
   ITK_TEMPLATE_EXPORT or ITK_TEMPLATE_IMPORT, so it should be used as
   if calling one of these macros.  The argument "EXPORT" will be
   replaced by a dllexport/dllimport macro such as ITKCommon_EXPORT.
   The argument "x" is a paren-enclosed list of template arguments.
   The argument "y" is a preprocessing token corresponding to the
   given template arguments and should be used to construct typedef
   names for the instantiations.

   Note the use of ITK_TEMPLATE_\<n\>, where \<n\> is the number of
   template arguments for the class template.  Note also that the
   number of template arguments is usually the length of the list
   nested within the inner parentheses, so the instantiation is listed
   with the form \<n\>(...).  Example definitions:

     \#define ITK_TEMPLATE_Foo(_, EXPORT, TypeX, TypeY) \
    namespace itk { \
       _(1(class EXPORT Foo\< ITK_TEMPLATE_1 TypeX \>)) \
       _(1(EXPORT std::ostream& operator<<(std::ostream&, \
                                           const Foo< ITK_TEMPLATE_1 TypeX >&))) \
       namespace Templates { \
    typedef Foo< ITK_TEMPLATE_1 TypeX > Foo\#\#TypeY; \
   }\
     }

     \#define ITK_TEMPLATE_Bar(_, EXPORT, TypeX, TypeY) \
    namespace itk { \
       _(2(class EXPORT Bar< ITK_TEMPLATE_2 TypeX >)) \
       _(1(EXPORT std::ostream& operator<<(std::ostream&, \
                                           const Bar< ITK_TEMPLATE_2 TypeX >&))) \
       namespace Templates { \
    typedef Bar< ITK_TEMPLATE_2 TypeX > Bar\#\#TypeY; \
   }\
     }

   Note that in the stream operator for template Bar there is a "1" at
   the beginning even though two arguments are taken.  This is because
   the expression "ITK_TEMPLATE_2 TypeX" is contained inside the
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
#define ITK_TEMPLATE_2(x1, x2)                      x1, x2
#define ITK_TEMPLATE_3(x1, x2, x3)                   x1, x2, x3
#define ITK_TEMPLATE_4(x1, x2, x3, x4)                x1, x2, x3, x4
#define ITK_TEMPLATE_5(x1, x2, x3, x4, x5)             x1, x2, x3, x4, x5
#define ITK_TEMPLATE_6(x1, x2, x3, x4, x5, x6)          x1, x2, x3, x4, x5, x6
#define ITK_TEMPLATE_7(x1, x2, x3, x4, x5, x6, x7)       x1, x2, x3, x4, x5, x6, x7
#define ITK_TEMPLATE_8(x1, x2, x3, x4, x5, x6, x7, x8)    x1, x2, x3, x4, x5, x6, x7, x8
#define ITK_TEMPLATE_9(x1, x2, x3, x4, x5, x6, x7, x8, x9) x1, x2, x3, x4, x5, x6, x7, x8, x9

/* In order to support both implicit and explicit instantation a .h
   file needs to know whether it should include its .hxx file
   containing the template definitions.  Define a macro to tell
   it.  Typical usage in itkFoo.h:
     #if ITK_TEMPLATE_TXX
     #include "itkFoo.hxx"
     #endif
*/
#if defined( ITK_MANUAL_INSTANTIATION )
#define ITK_TEMPLATE_TXX 0
#else
#define ITK_TEMPLATE_TXX !( ITK_TEMPLATE_CXX || ITK_TEMPLATE_TYPE )
#endif

/* All explicit instantiation source files define ITK_TEMPLATE_CXX.
   Define ITK_MANUAL_INSTANTIATION to tell .h files that have not been
   converted to this explicit instantiation scheme to not include
   their .hxx files.  Also disable warnings that commonly occur in
   these files but are not useful.  */
#if ITK_TEMPLATE_CXX
#undef ITK_MANUAL_INSTANTIATION
#define ITK_MANUAL_INSTANTIATION
#if defined( _MSC_VER )
#pragma warning (disable: 4275) /* non dll-interface base */
#pragma warning (disable: 4661) /* no definition available */
#endif
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
        #include "itkFoo+-.h"
        #endif
*/
#if ITK_TEMPLATE_IMPORT_WORKS && defined( ITK_EXPLICIT_INSTANTIATION )
#define ITK_TEMPLATE_EXPLICIT !ITK_TEMPLATE_CXX
#else
#define ITK_TEMPLATE_EXPLICIT 0
#endif

//----------------------------------------------------------------------------
// Macro to declare that a function does not return. __attribute__((noreturn))
//    On some compiler, functions that do not return (ex: exit(0)) must
//    have the noreturn attribute. Otherwise, a warning is raised. Use
//    that macro to avoid those warnings. GCC defines the attribute
//    noreturn for versions 2.5 and higher.
#if defined( __GNUC__ )
#define ITK_NO_RETURN  __attribute__ ( ( noreturn ) )
#else
#define ITK_NO_RETURN
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

#define itkAssertInDebugAndIgnoreInReleaseMacro(X) assert(X)

#endif //end of itkMacro.h
