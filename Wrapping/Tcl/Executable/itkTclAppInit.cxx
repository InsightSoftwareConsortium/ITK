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
#include <tcl.h>

#ifdef CMAKE_INTDIR
#define ITK_TCL_LIB_DIR ITK_BINARY_DIR "/Wrapping/Tcl/" CMAKE_INTDIR
#else
#define ITK_TCL_LIB_DIR ITK_BINARY_DIR "/Wrapping/Tcl"
#endif

int itkTclAppInit(Tcl_Interp* interp);

int main(int argc, char** argv)
{
  std::ios::sync_with_stdio();
  Tcl_Main(argc, argv, &itkTclAppInit);
  return 0;
}

extern "C"
{
  int Vxlnumericstcl_Init(Tcl_Interp*);
  int Itkcommontcl_Init(Tcl_Interp*);
}

int itkTclAppInit(Tcl_Interp* interp)
{
  // Initialize Tcl.
  if(Tcl_Init(interp) != TCL_OK) { return TCL_ERROR; }
  
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
  if(Itkcommontcl_Init(interp) != TCL_OK) { return TCL_ERROR; }
  
  // Initialize all ITK Tcl packages.
  static char initScript[] = "package require itk 0.7";
  if(Tcl_GlobalEval(interp, initScript) != TCL_OK) { return TCL_ERROR; }
  
  return TCL_OK;
}
