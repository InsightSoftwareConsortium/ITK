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
///standard system-wide macros for setting/getting instance variables,
///and defining other important ITK variables.
/**
 * The SetGet macros are used to interface to instance variables
 * in a standard fashion. This includes properly treating modified time
 * and printing out debug information.
 * 
 * Macros are available for built-in types; for character strings; 
 * vector arrays of built-in types size 2,3,4; for setting objects; and
 * debug, warning, and error printout information. */

#ifndef __itkSetGet_h
#define __itkSetGet_h
#include <string.h>
#include "itkWin32Header.h"
//
// Some constants used throughout code
//
#define ITK_LARGE_FLOAT 1.0e+38F
#define ITK_LARGE_INTEGER 2147483647 // 2^31 - 1

// These types are returned by GetDataType to indicate pixel type.
#define ITK_VOID            0
#define ITK_BIT             1 
#define ITK_CHAR            2
#define ITK_UNSIGNED_CHAR   3
#define ITK_SHORT           4
#define ITK_UNSIGNED_SHORT  5
#define ITK_INT             6
#define ITK_UNSIGNED_INT    7
#define ITK_LONG            8
#define ITK_UNSIGNED_LONG   9
#define ITK_FLOAT          10
#define ITK_DOUBLE         11 

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

// Set pointer to object; uses itkObject reference counting methodology.
// Creates method Set"name"() (e.g., SetPoints()).
//
#define itkSetObjectMacro(name,type) \
virtual void Set##name (type* _arg) \
  { \
  if (this->name != _arg) \
    { \
    if (this->name != NULL) { this->name->UnRegister(this); }\
    this->name = _arg; \
    if (this->name != NULL) { this->name->Register(this); } \
    this->Modified(); \
    } \
  } 

// Get pointer to object.  Creates member Get"name" (e.g., GetPoints()).
//
#define itkGetObjectMacro(name,type) \
virtual type *Get##name () \
  { \
  return this->name; \
  } 

// Use a global function which actually calls:
// itkOutputWindow::GetInstance()->DisplayText();
// This is to avoid itkObject #include of itkOutputWindow
// while itkOutputWindow #includes itkObject
extern ITK_EXPORT void itkOutputWindowDisplayText(const char*);

// This macro is used for  debug statements in instance methods
// itkDebugMacro(<< "this is debug info" << this->SomeVariable);
//
#define itkDebugMacro(x)
#define itkWarningMacro(x)
#define itkErrorMacro(x)

#define itkNotUsed(x)

#define itkSetMacro(ivarValue,value) \
{if ( ivarValue != value ) \
  {\
  this->Modified();\
  ivarValue = value;\
  }\
}

#define itkGetMacro(ivarValue) \
{
  return ivarValue;
}

#define itkGetObjectMacro(ivarObjectPtr) \
{
  return ivarObjectPtr;
}

// Set built-in type where value is constrained between min/max limits.
// Create member Set"name"() (e.q., SetRadius()). #defines are 
// convienience for clamping open-ended values.
//
#define itkSetClampMacro(ivarValue,value,min,max) \
{\
  value = (value < min ? min : (value > max ? max : value));\
  if ( ivarValue != value ) \
    {\
    this->Modified();\
    ivarValue = value;\
    }\
}

#endif
