/*=========================================================================

  Program:   Visualization Toolkit
  Module:    itkDynamicLoader.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
  Thanks:    Thanks to William A. Hoffman who developed this class


Copyright (c) 1993-2000 Ken Martin, Will Schroeder, Bill Lorensen 
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * Neither name of Ken Martin, Will Schroeder, or Bill Lorensen nor the names
   of any contributors may be used to endorse or promote products derived
   from this software without specific prior written permission.

 * Modified source versions must be plainly marked as such, and must not be
   misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
// .NAME itkDynamicLoader - class interface to system dynamic libraries
// .SECTION Description
// itkDynamicLoader provides a portable interface to loading dynamic 
// libraries into a process.  


#ifndef __itkDynamicLoader_h
#define __itkDynamicLoader_h

//BTX
// Ugly stuff for library handles
// They are different on several different OS's
#if defined(__hpux)
# include <dl.h>
  typedef shl_t itkLibHandle;
#elif defined(_WIN32)
# include "windows.h"
  typedef HMODULE itkLibHandle;
#elif defined(__powerpc)
  typedef ConnectionID itkLibHandle;
#else
  typedef void* itkLibHandle;
#endif
//ETX

#include "itkObject.h"


class ITK_EXPORT itkDynamicLoader : public itkObject
{
public:
  static itkDynamicLoader *New() {return new itkDynamicLoader;};
  itkTypeMacro(itkDynamicLoader,itkObject);

  //BTX
  // Description:
  // Load a dynamic library into the current process.
  // The returned itkLibHandle can be used to access the symbols in the 
  // library.
  static itkLibHandle OpenLibrary(const char*);

  // Description:
  // Attempt to detach a dynamic library from the
  // process.  A value of true is returned if it is sucessful.
  static int CloseLibrary(itkLibHandle);
  //ETX
  
  // Description:
  // Find the address of the symbol in the given library
  static void* GetSymbolAddress(itkLibHandle, const char*);

  // Description:
  // Return the library prefix for the given architecture
  static const char* LibPrefix();

  // Description:
  // Return the library extension for the given architecture
  static const char* LibExtension();

  // Description:
  // Return the last error produced from a calls made on this class.
  static const char* LastError();
  
protected:
  itkDynamicLoader() {};
  ~itkDynamicLoader() {};
  itkDynamicLoader(const itkDynamicLoader&) {};
  void operator=(const itkDynamicLoader&) {};

  
};

#endif
