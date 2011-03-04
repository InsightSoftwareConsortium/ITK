/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkLightObject.h"
#include "itkTclConfigure.h"

#include <sys/stat.h>
#include <string.h>

#ifndef ITK_TCL_NO_TK
#  include <tk.h>
#else
#  include <tcl.h>
#endif

// Include the list of modules that the user has selected to build
#include "itkTclModules.h"

//----------------------------------------------------------------------------
int itkTclAppInit(Tcl_Interp* interp);

//----------------------------------------------------------------------------
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

//----------------------------------------------------------------------------
/** Main application initialization function.  */
int itkTclAppInit(Tcl_Interp* interp)
{
  // Initialize Tcl.
  if(Tcl_Init(interp) != TCL_OK) { return TCL_ERROR; }

#ifndef ITK_TCL_NO_TK
  // Initialize Tk.
  if(Tk_Init(interp) != TCL_OK) { return TCL_ERROR; }
#endif

  // Initialize the built-in packages.
  for (unsigned int i=0; i < NumITKModules; i++)
    {
    if( ModuleInitializers[i](interp) != TCL_OK )
      {
      return TCL_ERROR;
      }
    }

  // Initialize all ITK Tcl packages.
  static char initScript[] = "package require InsightToolkit " ITK_VERSION_STRING;
  if(Tcl_GlobalEval(interp, initScript) != TCL_OK) { return TCL_ERROR; }

  // Allow users to have an initialization file for interactive mode.
  static char rcFileNameVariable[] = "tcl_rcFileName";
  static char rcFileNameValue[] = "~/.itktclrc";
  Tcl_SetVar(interp, rcFileNameVariable, rcFileNameValue, TCL_GLOBAL_ONLY);

  return TCL_OK;
}
