/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapInit.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "wrapUtils.h"
#include "wrapTypeInfo.h"

namespace _wrap_
{
extern Tcl_ObjType TclPointerType;
extern Tcl_ObjType TclReferenceType;
} // namespace _wrap_


// Make sure wrapper initialization function has external C-style linkage.
extern "C" { _wrap_EXPORT int Wrap_Init(Tcl_Interp*); }

/**
 * When the WrapTclFacilitator library is loaded by a tcl interpreter,
 * this is called to initialize it.
 *
 * This just registers the Pointer and Reference Tcl object types needed
 * by the wrappers with the interpreter.
 */
_wrap_EXPORT int Wrap_Init(Tcl_Interp* interp)
{
  Tcl_RegisterObjType(&_wrap_::TclPointerType);
  Tcl_RegisterObjType(&_wrap_::TclReferenceType);
  
  _wrap_::TypeInfo::Initialize();
  
  return TCL_OK;
}
