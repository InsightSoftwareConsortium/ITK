/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTclAppInit.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkLightObject.h"
#include "itkTclConfigure.h"

#ifndef ITK_TCL_NO_TK
#  include <tk.h>
#else
#  include <tcl.h>
#endif

// Define the location of the pkgIndex.tcl file for ITK.
#ifdef CMAKE_INTDIR
#define ITK_TCL_LIB_DIR ITK_BINARY_DIR "/Wrapping/Tcl/" CMAKE_INTDIR
#else
#define ITK_TCL_LIB_DIR ITK_BINARY_DIR "/Wrapping/Tcl"
#endif

int itkTclAppInit(Tcl_Interp* interp);

/** Program entry point.  */
int main(int argc, char** argv)
{
  std::ios::sync_with_stdio();
#ifndef ITK_TCL_NO_TK
  Tk_Main(argc, argv, &itkTclAppInit);
#else
  Tcl_Main(argc, argv, &itkTclAppInit);
#endif
  return 0;
}

// Get the Tcl package initialization functions to call directly.
extern "C"
{
  int Vxlnumericstcl_Init(Tcl_Interp*);
  int Itknumericstcl_Init(Tcl_Interp*);
  int Itkcommontcl_Init(Tcl_Interp*);
  int Itkiotcl_Init(Tcl_Interp*);
  int Itkbasicfilterstcl_Init(Tcl_Interp*);
  int Itkalgorithmstcl_Init(Tcl_Interp*);
}

/** Main application initialization function.  */
int itkTclAppInit(Tcl_Interp* interp)
{
  // Initialize Tcl.
  if(Tcl_Init(interp) != TCL_OK) { return TCL_ERROR; }
  
#ifndef ITK_TCL_NO_TK
  // Initialize Tk.
  if(Tk_Init(interp) != TCL_OK) { return TCL_ERROR; }
#endif
  
  // Prefix tcl package directory onto the load path.
  static char pathScript[] =
    "foreach dir [list \"" ITK_TCL_LIB_DIR "\"] {\n"
    "  if {[file isdirectory $dir]} {\n"
    "    set auto_path [linsert $auto_path 0 $dir]\n"
    "  }\n"
    "}\n";
  if(Tcl_GlobalEval(interp, pathScript) != TCL_OK) { return TCL_ERROR; }
  
  // Initialize the built-in packages.
  if(Vxlnumericstcl_Init(interp) != TCL_OK) { return TCL_ERROR; }
  if(Itknumericstcl_Init(interp) != TCL_OK) { return TCL_ERROR; }
  if(Itkcommontcl_Init(interp) != TCL_OK) { return TCL_ERROR; }
  if(Itkiotcl_Init(interp) != TCL_OK) { return TCL_ERROR; }
  if(Itkbasicfilterstcl_Init(interp) != TCL_OK) { return TCL_ERROR; }
  if(Itkalgorithmstcl_Init(interp) != TCL_OK) { return TCL_ERROR; }
  
  // Initialize all ITK Tcl packages.
  static char initScript[] = "package require InsightToolkit 0.7";
  if(Tcl_GlobalEval(interp, initScript) != TCL_OK) { return TCL_ERROR; }
  
  // Allow users to have an initialization file for interactive mode.
  static char rcFileNameVariable[] = "tcl_rcFileName";
  static char rcFileNameValue[] = "~/.itktclrc";
  Tcl_SetVar(interp, rcFileNameVariable, rcFileNameValue, TCL_GLOBAL_ONLY);
  
  return TCL_OK;
}
