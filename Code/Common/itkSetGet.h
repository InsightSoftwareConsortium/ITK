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

// Use this method to set instance variable values.
#define itkSetMacro(ivarValue,value) \
{itkDebugMacro(<< this->GetClassName() << " (" << this \
               << "): setting " #ivarValue " to " << value); \
if ( ivarValue != value ) \
  {\
  this->Modified();\
  ivarValue = value;\
  }\
}

// Use this method to get instance variable values.
#define itkGetMacro(ivarValue) \
{\
  return ivarValue;\
}

// Set built-in type where value is constrained between min/max limits.
#define itkSetClampMacro(ivarValue,value,min,max) \
{\
  value = (value < min ? min : (value > max ? max : value));\
  if ( ivarValue != value ) \
    {\
    this->Modified();\
    ivarValue = value;\
    }\
}

// Use this method to get a pointer to an object, where the pointer
// is a member of a class.
#define itkGetObjectMacro(ivarObjectPtr) \
{\
  return ivarObjectPtr;\
}

// Macros for setting strings. These macros depend on using
// the standard "string" class.
#define itkSetStringMacro(ivarValue,str) \
{\
  if ( str != ivarValue ) \
    { \
    ivarValue = str;\
    this->Modified();\
    } \
}

// Macros for getting strings. These macros depend on using
// the standard "string" class (i.e., the class must respond
// to data() invocation).
#define itkGetStringMacro(ivarValue) \
{\
  return ivarValue.data();\
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
    { char *itkmsgbuff; ostrstream itkmsg; \
      itkmsg << "Debug: In " __FILE__ ", line " << __LINE__ << "\n" \
             << this->GetClassName() << " (" << this << "): " x  \
             << "\n\n" << ends; \
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
