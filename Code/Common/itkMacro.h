/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMacro.h
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
#include <strstream>

/** \namespace itk
 * \brief The "itk" namespace contains all classes defined for Insight.
 */
namespace itk
{
} // end namespace itk

/**
 * Error codes for exceptions
 */
namespace itk
{
  const int BoundsError=10;
  const int InvalidDimension=11;
} // end namespace itk


/**
 * A convenience macro marks variables as not being used by a method,
 * avoiding compile-time errors.
 */
#define itkNotUsed(x)


/**
 * Set built-in type.  Creates member Set"name"() (e.g., SetVisibility());
 */
#define itkSetMacro(name,type) \
  virtual void Set##name (const type _arg) \
  { \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): setting " #name " to " << _arg); \
    if (this->m_##name != _arg) \
      { \
      this->m_##name = _arg; \
      this->Modified(); \
      } \
  } 


/**
 * Get built-in type.  Creates member Get"name"() (e.g., GetVisibility());
 */
#define itkGetMacro(name,type) \
  virtual type Get##name () \
  { \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): returning " << #name " of " << this->m_##name ); \
    return this->m_##name; \
  }


/**
 * Get built-in type.  Creates member Get"name"() (e.g., GetVisibility());
 * This is the "const" form of the itkGetMacro.  It should be used unless
 * the member can be changed through the "Get" access routine.
 */
#define itkGetConstMacro(name,type) \
  virtual type Get##name () const \
  { \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): returning " << #name " of " << this->m_##name ); \
    return this->m_##name; \
  }


/**
 * Get built-in type.  Creates member Get"name"() (e.g., GetVisibility());
 * This is the "const" form of the itkGetMacro.  It should be used unless
 * the member can be changed through the "Get" access routine.
 * This versions returns a const reference to the variable.
 */
#define itkGetConstReferenceMacro(name,type) \
  virtual const type & Get##name () const \
  { \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): returning " << #name " of " << this->m_##name ); \
    return this->m_##name; \
  }


/**
 * Set character string.  Creates member Set"name"() 
 * (e.g., SetFilename(char *)). The macro assumes that
 * the class member (name) is declared a type std::string.
 */
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


/**
 * Get character string.  Creates member Get"name"() 
 * (e.g., SetFilename(char *)). The macro assumes that
 * the class member (name) is declared a type std::string.
 */
#define itkGetStringMacro(name) \
  virtual const char* Get##name () const \
  { \
    return this->m_##name.c_str(); \
  } 


/**
 * Set built-in type where value is constrained between min/max limits.
 * Create member Set"name"() (e.q., SetRadius()). #defines are 
 * convienience for clamping open-ended values.
 */
#define itkSetClampMacro(name,type,min,max) \
  virtual void Set##name (type _arg) \
  { \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): setting " << #name " to " << _arg ); \
    if (this->m_##name != (_arg<min?min:(_arg>max?max:_arg))) \
      { \
      this->m_##name = (_arg<min?min:(_arg>max?max:_arg)); \
      this->Modified(); \
      } \
  } 


/**
 * Set pointer to object; uses Object reference counting methodology.
 * Creates method Set"name"() (e.g., SetPoints()). Note that using
 * smart pointers requires using real pointers when setting input,
 * but returning smart pointers on output.
 */
#define itkSetObjectMacro(name,type) \
  virtual void Set##name (type* _arg) \
  { \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): setting " << #name " to " << _arg ); \
    if (this->m_##name != _arg) \
      { \
      this->m_##name = _arg; \
      this->Modified(); \
      } \
  } 


/**
 * Get a smart pointer to an object.  Creates the member 
 * Get"name"() (e.g., GetPoints()).
 */
#define itkGetObjectMacro(name,type) \
  virtual typename type::Pointer Get##name () \
  { \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): returning " #name " address " << this->m_##name ); \
    return this->m_##name; \
  } 

/**
 * Get a const reference to a smart pointer to an object.  
 * Creates the member Get"name"() (e.g., GetPoints()).
 */
#define itkGetConstReferenceObjectMacro(name,type) \
  virtual const typename type::Pointer & Get##name () const \
  { \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): returning " #name " address " << this->m_##name ); \
    return this->m_##name; \
  } 


/**
 * Create members "name"On() and "name"Off() (e.g., DebugOn() DebugOff()).
 * Set method must be defined to use this macro.
 */
#define itkBooleanMacro(name) \
  virtual void name##On () { this->Set##name(true);} \
  virtual void name##Off () { this->Set##name(false);}

/**
 * Following set macros for vectors define two members for each macro.  The
 * first allows setting of individual components (e.g,
 * SetColor(float,float,float)), the second allows setting from an array
 * (e.g., SetColor(float* rgb[3])).  The macros vary in the size of the
 * vector they deal with.
 */
#define itkSetVector2Macro(name,type) \
  virtual void Set##name (type _arg1, type _arg2) \
  { \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): setting " << #name " to (" << _arg1 \
                  << "," << _arg2 << ")"); \
    if ((this->m_##name[0] != _arg1)||(this->m_##name[1] != _arg2)) \
      { \
      this->Modified(); \
      this->m_##name[0] = _arg1; \
      this->m_##name[1] = _arg2; \
      } \
  }; \
  void Set##name (type _arg[2]) \
  { \
    this->Set##name (_arg[0], _arg[1]); \
  } 

#define itkGetVector2Macro(name,type) \
  virtual type *Get##name () const \
  { \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): returning " << #name " pointer " << this->m_##name); \
    return this->m_##name; \
  } \
  virtual void Get##name (type &_arg1, type &_arg2) const \
  { \
    _arg1 = this->m_##name[0]; \
    _arg2 = this->m_##name[1]; \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): returning " << #name " = (" << _arg1 \
                  << "," << _arg2 << ")"); \
    }; \
  virtual void Get##name (type _arg[2]) const \
  { \
    this->Get##name (_arg[0], _arg[1]);\
  } 

#define itkSetVector3Macro(name,type) \
  virtual void Set##name (type _arg1, type _arg2, type _arg3) \
  { \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): setting " << #name " to (" << _arg1 \
                  << "," << _arg2 << "," << _arg3 << ")"); \
    if ((this->m_##name[0] != _arg1)||(this->m_##name[1] != _arg2) || \
        (this->m_##name[2] != _arg3)) \
      { \
      this->Modified(); \
      this->m_##name[0] = _arg1; \
      this->m_##name[1] = _arg2; \
      this->m_##name[2] = _arg3; \
      } \
  }; \
  virtual void Set##name (type _arg[3]) \
    { \
    this->Set##name (_arg[0], _arg[1], _arg[2]);\
    } 

#define itkGetVector3Macro(name,type) \
virtual type *Get##name () const \
  { \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): returning " << #name " pointer " << this->m_##name); \
    return this->m_##name; \
  } \
  virtual void Get##name (type &_arg1, type &_arg2, type &_arg3) const \
  { \
    _arg1 = this->m_##name[0]; \
    _arg2 = this->m_##name[1]; \
    _arg3 = this->m_##name[2]; \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): returning " << #name " = (" << _arg1 << "," \
                  << _arg2 << "," << _arg3 << ")"); \
    }; \
  virtual void Get##name (type _arg[3]) const \
  { \
    this->Get##name (_arg[0], _arg[1], _arg[2]);\
  } 

#define itkSetVector4Macro(name,type) \
  virtual void Set##name (type _arg1, type _arg2, type _arg3, type _arg4) \
    { \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): setting " << #name " to (" << _arg1 \
                  << "," << _arg2 << "," << _arg3 << "," << _arg4 << ")"); \
    if ((this->m_##name[0] != _arg1) ||(this->m_##name[1] != _arg2) || \
        (this->m_##name[2] != _arg3)||(this->m_##name[3] != _arg4)) \
      { \
      this->Modified(); \
      this->m_##name[0] = _arg1; \
      this->m_##name[1] = _arg2; \
      this->m_##name[2] = _arg3; \
      this->m_##name[3] = _arg4; \
      } \
    }; \
  virtual void Set##name (type _arg[4]) \
  { \
    this->Set##name (_arg[0], _arg[1], _arg[2], _arg[3]);\
  } 

#define itkGetVector4Macro(name,type) \
  virtual type *Get##name () const \
  { \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): returning " << #name " pointer " << this->m_##name); \
    return this->m_##name; \
  } \
  virtual void Get##name (type &_arg1, type &_arg2, \
                          type &_arg3, type &_arg4) const \
  { \
    _arg1 = this->m_##name[0]; \
    _arg2 = this->m_##name[1]; \
    _arg3 = this->m_##name[2]; \
    _arg4 = this->m_##name[3]; \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): returning " << #name " = (" << _arg1 << "," \
                  << _arg2 << "," << _arg3 << "," << _arg4 << ")"); \
  }; \
  virtual void Get##name (type _arg[4]) const \
  { \
    this->Get##name (_arg[0], _arg[1], _arg[2], _arg[3]);\
  } 

#define itkSetVector6Macro(name,type) \
  virtual void Set##name (type _arg1, type _arg2, type _arg3, \
                          type _arg4, type _arg5, type _arg6) \
    { \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): setting " << #name " to (" << _arg1 << "," \
                  << _arg2 << "," << _arg3 << "," << _arg4 << "," \
                  << _arg5 << "," << _arg6 << ")"); \
    if ((this->m_##name[0] != _arg1)||(this->m_##name[1] != _arg2) || \
        (this->m_##name[2] != _arg3)||(this->m_##name[3] != _arg4) || \
        (this->m_##name[4] != _arg5)||(this->m_##name[5] != _arg6)) \
      { \
      this->Modified(); \
      this->m_##name[0] = _arg1; \
      this->m_##name[1] = _arg2; \
      this->m_##name[2] = _arg3; \
      this->m_##name[3] = _arg4; \
      this->m_##name[4] = _arg5; \
      this->m_##name[5] = _arg6; \
      } \
    }; \
  virtual void Set##name (type _arg[6]) \
    { \
    this->Set##name (_arg[0], _arg[1], _arg[2], _arg[3], _arg[4], _arg[5]); \
    } 

#define itkGetVector6Macro(name,type) \
  virtual type *Get##name () const \
  { \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): returning " << #name " pointer " << this->m_##name); \
    return this->m_##name; \
  } \
  virtual void Get##name (type &_arg1, type &_arg2, type &_arg3, \
                          type &_arg4, type &_arg5, type &_arg6) const \
    { \
    _arg1 = this->m_##name[0]; \
    _arg2 = this->m_##name[1]; \
    _arg3 = this->m_##name[2]; \
    _arg4 = this->m_##name[3]; \
    _arg5 = this->m_##name[4]; \
    _arg6 = this->m_##name[5]; \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
    << "): returning " << #name " = (" << _arg1 << "," << _arg2 \
    << "," << _arg3 << "," << _arg4 << "," << _arg5 <<"," << _arg6 << ")"); \
    }; \
  virtual void Get##name (type _arg[6]) const \
    { \
    this->Get##name (_arg[0], _arg[1], _arg[2], _arg[3], _arg[4], _arg[5]); \
    } 

/**
 * General set vector macro creates a single method that copies specified
 * number of values into object.
 * Examples: void SetColor(c,3)
 */
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

/**
 * Get vector macro. Returns pointer to type (i.e., array of type).
 * This is for efficiency. 
 */
#define itkGetVectorMacro(name,type,count) \
  virtual type *Get##name () const \
  { \
    return this->m_##name; \
  } 

namespace itk
{
/**
 * Use a global function which actually calls:
 * OutputWindow::GetInstance()->DisplayText();
 * This is to avoid Object #include of OutputWindow
 * while OutputWindow #includes Object
 */
extern ITK_EXPORT void OutputWindowDisplayText(const char*);
} // end namespace itk

/**
 * This macro is used to print debug (or other information). They are
 * also used to catch errors, etc. Example usage looks like:
 * itkDebugMacro(<< "this is debug info" << this->SomeVariable);
 */
#ifdef ITK_LEAN_AND_MEAN
#define itkDebugMacro(x)
#else
#define itkDebugMacro(x) \
{ if (this->GetDebug() && Object::GetGlobalWarningDisplay()) \
    { char *itkmsgbuff; std::ostrstream itkmsg; \
      itkmsg << "Debug: In " __FILE__ ", line " << __LINE__ << "\n" \
             << this->GetClassName() << " (" << this << "): " x  \
             << "\n\n" << std::ends; \
      itkmsgbuff = itkmsg.str(); \
      OutputWindowDisplayText(itkmsgbuff); \
      itkmsg.rdbuf()->freeze(0);} \
}
#endif


/**
 * Define the standard object factory creation method.  This macro
 * simply takes the type for which the New() method is being defined.
 *
 * This creation method first tries asking the object factory to create
 * an instance, and then defaults to the standard "new" operator if the
 * factory fails.
 *
 * This routine assigns the raw pointer to a smart pointer and then calls
 * UnRegister() on the rawPtr to compensate for LightObject's constructor
 * initializing an object's reference count to 1 (needed for proper
 * initialization of process objects and data objects cycles).
 */
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
}

#define itkWarningMacro(x)

#define itkErrorMacro(x)

#define itkGenericOutputMacro(x)

#define itkTypeMacro(thisClass,superclass) \
    virtual const char *GetClassName() const \
        {return #thisClass;}



#endif
