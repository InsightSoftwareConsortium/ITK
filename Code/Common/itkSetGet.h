/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSetGet.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkSetGet.h defines standard system-wide macros, constants, and other
 * parameters. One of its most important functions is to define macros used
 * to interface to instance variables in a standard fashion. For example,
 * these macros manage modified time, debugging information, and provide a
 * standard interface to set and get instance variables.  SetGet macros are
 * available for built-in types; for string classe; vector arrays;
 * object pointers; and debug, warning, and error printout information. 
 */

#ifndef __itkSetGet_h
#define __itkSetGet_h

#include <string>
#include <strstream>
#include "itkWin32Header.h"
//
// Some constants used throughout code
//
#define ITK_LARGE_FLOAT 1.0e+38F
#define ITK_LARGE_INTEGER 2147483647 // 2^31 - 1

// Some constant required for correct template performance
#define ITK_BIT_MIN 0
#define ITK_BIT_MAX 1
#define ITK_CHAR_MIN -128
#define ITK_CHAR_MAX 127
#define ITK_UNSIGNED_CHAR_MIN 0
#define ITK_UNSIGNED_CHAR_MAX 255
#define ITK_SHORT_MIN -32768
#define ITK_SHORT_MAX 32767
#define ITK_UNSIGNED_SHORT_MIN 0
#define ITK_UNSIGNED_SHORT_MAX 65535
#define ITK_INT_MIN (-ITK_LARGE_INTEGER-1)
#define ITK_INT_MAX ITK_LARGE_INTEGER
#define ITK_UNSIGNED_INT_MIN 0
#define ITK_UNSIGNED_INT_MAX 4294967295UL
#define ITK_LONG_MIN (-ITK_LARGE_INTEGER-1)
#define ITK_LONG_MAX ITK_LARGE_INTEGER
#define ITK_UNSIGNED_LONG_MIN 0
#define ITK_UNSIGNED_LONG_MAX 4294967295UL
#define ITK_FLOAT_MIN -ITK_LARGE_FLOAT
#define ITK_FLOAT_MAX ITK_LARGE_FLOAT
#define ITK_DOUBLE_MIN -1.0e+99L
#define ITK_DOUBLE_MAX  1.0e+99L

// Error codes for exceptions
const int itkBoundsError=10;
const int itkInvalidDimension=11;

// A convenience macro marks variables as not being used by a method,
// avoiding compile-time errors.
#define itkNotUsed(x)

// Set built-in type.  Creates member Set"name"() (e.g., SetVisibility());
//
#define itkSetMacro(name,type) \
  virtual void Set##name (type _arg) \
  { \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): setting " #name " to " << _arg); \
    if (this->m_##name != _arg) \
      { \
      this->m_##name = _arg; \
      this->Modified(); \
      } \
  } 

// Get built-in type.  Creates member Get"name"() (e.g., GetVisibility());
//
#define itkGetMacro(name,type) \
  virtual type Get##name () \
  { \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): returning " << #name " of " << this->m_##name ); \
    return this->m_##name; \
  } 


// Set character string.  Creates member Set"name"() 
// (e.g., SetFilename(char *));
//
#define itkSetStringMacro(name) \
  virtual void Set##name (const char* _arg) \
  { \
    if ( _arg && (_arg != this->m_##name) ) { return;} \
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

// Get character string.  Creates member Get"name"() 
// (e.g., char *GetFilename());
//
#define itkGetStringMacro(name) \
  virtual const char* Get##name () const \
  { \
    return this->m_##name.data(); \
  } 

// Set built-in type where value is constrained between min/max limits.
// Create member Set"name"() (e.q., SetRadius()). #defines are 
// convienience for clamping open-ended values.
//
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

// Set pointer to object; uses itkObject reference counting methodology.
// Creates method Set"name"() (e.g., SetPoints()).
//
#define itkSetObjectMacro(name,type) \
  virtual void Set##name (type* _arg) \
  { \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): setting " << #name " to " << _arg ); \
    if (this->m_##name != _arg) \
      { \
      if (this->m_##name != NULL) { this->m_##name->UnRegister(this); }\
      this->m_##name = _arg; \
      if (this->m_##name != NULL) { this->m_##name->Register(this); } \
      this->Modified(); \
      } \
  } 

// Get pointer to object.  Creates member Get"name" (e.g., GetPoints()).
//
#define itkGetObjectMacro(name,type) \
  virtual type *Get##name () \
  { \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): returning " #name " address " << this->m_##name ); \
    return this->m_##name; \
  } 

// Create members "name"On() and "name"Off() (e.g., DebugOn() DebugOff()).
// Set method must be defined to use this macro.
//
#define itkBooleanMacro(name) \
  virtual void name##On () { this->Set##name(true);}; \
  virtual void name##Off () { this->Set##name(false);}

// Following set macros for vectors define two members for each macro.  The
// first allows setting of individual components (e.g,
// SetColor(float,float,float)), the second allows setting from an array
// (e.g., SetColor(float* rgb[3])).  The macros vary in the size of the
// vector they deal with.
//
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
  virtual void Get##name (type &_arg1, type &_arg2, type &_arg3) \
  { \
    _arg1 = this->m_##name[0]; \
    _arg2 = this->m_##name[1]; \
    _arg3 = this->m_##name[2]; \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): returning " << #name " = (" << _arg1 << "," \
                  << _arg2 << "," << _arg3 << ")"); \
    }; \
  virtual void Get##name (type _arg[3]) \
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

// General set vector macro creates a single method that copies specified
// number of values into object.
// Examples: void SetColor(c,3)
//
#define itkSetVectorMacro(name,type,count) \
  virtual void Set##name(type data[]) \
  { \
    int i; \
    for (i=0; i<count; i++) { if ( data[i] != this->m_##name[i] ) { break; }} \
    if ( i < count ) \
      { \
      this->Modified(); \
      for (i=0; i<count; i++) { this->m_##name[i] = data[i]; }\
      } \
  }

// Get vector macro defines two methods. One returns pointer to type 
// (i.e., array of type). This is for efficiency. The second copies data
// into user provided array. This is more object-oriented.
// Examples: float *GetColor() and void GetColor(float c[count]).
//
#define itkGetVectorMacro(name,type,count) \
  virtual type *Get##name () const \
  { \
    itkDebugMacro(<< this->GetClassName() << " (" << this \
                  << "): returning " << #name " pointer " << this->m_##name); \
    return this->m_##name; \
  } \
  virtual void Get##name (type data[count]) const \
  { \
    for (int i=0; i<count; i++) { data[i] = this->m_##name[i]; }\
  }

// Use a global function which actually calls:
// itkOutputWindow::GetInstance()->DisplayText();
// This is to avoid itkObject #include of itkOutputWindow
// while itkOutputWindow #includes itkObject
extern ITK_EXPORT void itkOutputWindowDisplayText(const char*);

// This macro is used to print debug (or other information). They are
// also used to catch errors, etc. Example usage looks like:
// itkDebugMacro(<< "this is debug info" << this->SomeVariable);
//
#ifdef ITK_LEAN_AND_MEAN
#define itkDebugMacro(x)
#else
#define itkDebugMacro(x) \
{ if (this->GetDebug() && itkObject::GetGlobalWarningDisplay()) \
    { char *itkmsgbuff; std::ostrstream itkmsg; \
      itkmsg << "Debug: In " __FILE__ ", line " << __LINE__ << "\n" \
             << this->GetClassName() << " (" << this << "): " x  \
             << "\n\n" << std::ends; \
      itkmsgbuff = itkmsg.str(); \
      itkOutputWindowDisplayText(itkmsgbuff); \
      itkmsg.rdbuf()->freeze(0);} \
}
#endif

#define itkWarningMacro(x)

#define itkErrorMacro(x)

#define itkGenericOutputMacro(x)

#define itkTypeMacro(thisClass,superclass) \
    virtual const char *GetClassName() const \
        {return #thisClass;}



#endif
