/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapTclShell.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "wrapUtils.h"

namespace _wrap_
{

// We need to call the wrapping facilitator initialization function.
extern "C" { _wrap_EXPORT int Wrap_Init(Tcl_Interp*); }

/**
 * Initialize the given Tcl interpreter for use with the wrapping
 * facilitator.
 */
_wrap_EXPORT int Main_Wrap_Init(Tcl_Interp* interp)
{
  // Initialize the Tcl interpreter.
  if(Tcl_Init(interp) == TCL_ERROR)
    {
    return TCL_ERROR;
    }
  
  // Initialize the wrapping facilitator package.
  return Wrap_Init(interp);
}
  
} // namespace _wrap_


/**
 * Entry point to program.  Just initializes the Tcl interpreter
 * and tells it to call Main_Wrap_Init to initialize the wrapper facility.
 */
int main(int argc, char* argv[])
{
  Tcl_Main(argc, argv, _wrap_::Main_Wrap_Init);
  return 0;
}
