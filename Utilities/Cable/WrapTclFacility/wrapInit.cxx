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

#include <set>

namespace _wrap_
{
extern Tcl_ObjType TclPointerType;
extern Tcl_ObjType TclReferenceType;


/**
 * Initialization class to setup the facility for a Tcl interpreter.
 */
class TclWrapperFacility
{
public:
  static int InitializeInterpreter(Tcl_Interp*);
private:
  static std::set<Tcl_Interp*> initializedInterpreters;
};


/**
 * The set of interpreters that have been initialized.
 */
std::set<Tcl_Interp*> TclWrapperFacility::initializedInterpreters;


/**
 * Initialization function for a Tcl interpreter.   This will only execute
 * exactly once for a given interpreter.
 *
 * This just registers the Pointer and Reference Tcl object types needed
 * by the wrappers with the interpreter.
 */
int TclWrapperFacility::InitializeInterpreter(Tcl_Interp* interp)
{
  // Initialize predefined type information.  This only executes once,
  // no matter how many interpreters are using it.
  _wrap_::TypeInfo::Initialize();

  // Only register our object types if the interpreter has not been
  // initialized.
  if(initializedInterpreters.find(interp) != initializedInterpreters.end())
    {
      Tcl_RegisterObjType(&_wrap_::TclPointerType);
      Tcl_RegisterObjType(&_wrap_::TclReferenceType);
    }
  
  initializedInterpreters.insert(interp);

  return TCL_OK;
}


} // namespace _wrap_


// Make sure wrapper initialization function has external C-style linkage.
extern "C" { _wrap_EXPORT int Wrap_Init(Tcl_Interp*); }

/**
 * When the WrapTclFacilitator library is loaded by a Tcl interpreter,
 * this is called to initialize it.  This just passes the call to
 * TclWrapperFacility::InitializeInterpreter.
 */
_wrap_EXPORT int Wrap_Init(Tcl_Interp* interp)
{
  return _wrap_::TclWrapperFacility::InitializeInterpreter(interp);
}
