/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapUtils.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "wrapUtils.h"

#include <string.h>

namespace _wrap_
{

/**
 * Check whether the given object's type pointer is NULL.
 */
bool TclObjectTypeIsNULL(Tcl_Obj* o)
{
  return ((o)->typePtr == NULL);
}


/**
 * Check whether the given object's type is "boolean".
 */
bool TclObjectTypeIsBoolean(Tcl_Obj* o)
{
  return (!TclObjectTypeIsNULL(o)
          && (strcmp("boolean", (o)->typePtr->name)==0));
}


/**
 * Check whether the given object's type is "int".
 */
bool TclObjectTypeIsInt(Tcl_Obj* o)
{
  return (!TclObjectTypeIsNULL(o)
          && (strcmp("int", (o)->typePtr->name)==0));
}
  

/**
 * Check whether the given object's type is "long".
 */
bool TclObjectTypeIsLong(Tcl_Obj* o)
{
  return (!TclObjectTypeIsNULL(o)
          && (strcmp("long", (o)->typePtr->name)==0));
}
  

/**
 * Check whether the given object's type is "double".
 */
bool TclObjectTypeIsDouble(Tcl_Obj* o)
{
  return (!TclObjectTypeIsNULL(o)
          && (strcmp("double", (o)->typePtr->name)==0));
}


/**
 * Check whether the given object's type is "string".
 */
bool TclObjectTypeIsString(Tcl_Obj* o)
{
  return (!TclObjectTypeIsNULL(o)
          && (strcmp("string", (o)->typePtr->name)==0));
}


/**
 * Check whether the given object is a command name object.
 */
bool TclObjectTypeIsCmdName(Tcl_Obj* o)
{
  return (!TclObjectTypeIsNULL(o)
          && (strcmp("cmdName", (o)->typePtr->name)==0));
}


} // namespace _wrap_
