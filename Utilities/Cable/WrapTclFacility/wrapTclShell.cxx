/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapTclShell.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
int Main_Wrap_Init(Tcl_Interp* interp)
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
