/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDynamicLoader.cxx
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
#include "itkDynamicLoader.h"

// This file is actually 3 different implementations.
// 1. HP machines which uses shl_load
// 2. Power PC MAC which uses GetSharedLibrary
// 3. Windows which uses LoadLibrary
// 4. Most unix systems which use dlopen (default )
// Each part of the ifdef contains a complete implementation for
// the static methods of DynamicLoader.  


// ---------------------------------------------------------------
// 1. Implementation for HPUX  machines
#ifdef __hpux
#define ITKDYNAMICLOADER_DEFINED 1
#include <dl.h>

namespace itk
{

//----------------------------------------------------------------------------
LibHandle 
DynamicLoader
::OpenLibrary(const char* libname )
{
  return shl_load(libname, BIND_DEFERRED | DYNAMIC_PATH, 0L);
}

int 
DynamicLoader::
CloseLibrary(LibHandle lib)
{
  return 0;
}

//----------------------------------------------------------------------------
void* 
DynamicLoader
::GetSymbolAddress(LibHandle lib, const char* sym)
{ 
  void* addr;
  int status;
  
  status = shl_findsym (&lib, sym, TYPE_PROCEDURE, &addr);
  return (status < 0) ? (void*)0 : addr;
}

//----------------------------------------------------------------------------
const char* 
DynamicLoader
::LibPrefix()
{ 
  return "lib";
}

//----------------------------------------------------------------------------
const char* 
DynamicLoader
::LibExtension()
{
  return ".sl";
}

//----------------------------------------------------------------------------
const char* 
DynamicLoader
::LastError()
{
  return 0;
}

} // end namespace itk

#endif


// ---------------------------------------------------------------
// 2. Implementation for the Power PC (MAC)
#ifdef __powerc
#define ITKDYNAMICLOADER_DEFINED 1
#include <CodeFragments.h>
#include <Errors.h>
#include <Threads.h>
#include <Timer.h>

namespace itk
{

//----------------------------------------------------------------------------
LibHandle 
DynamicLoader
::OpenLibrary(const char* libname )
{
    Str63 libName;
    libName[0] = strlen(libname);
    strcpy((char*) &libName[1], libname);
    ConnectionID connID;
    Ptr mainAddr;
    Str255 errName;
    OSErr err = GetSharedLibrary(
        libName, kPowerPCArch, kLoadLib, &connID, &mainAddr, errName
    );
    return err == fragNoErr ? connID : 0;
}

//----------------------------------------------------------------------------
int 
DynamicLoader
::CloseLibrary(LibHandle lib)
{
  return 0;
}

//----------------------------------------------------------------------------
void* 
DynamicLoader
::GetSymbolAddress(LibHandle lib, const char* sym)
{ 
  Str255 symName;
  symName[0] = strlen(sym);
  strcpy((char*) &symName[1], sym);
  Ptr symAddr;
  SymClass symClass;
  OSErr err = FindSymbol(lib, symName, &symAddr, &symClass);
  return (void*) symAddr; 
}

//----------------------------------------------------------------------------
const char* 
DynamicLoader
::LibPrefix()
{ 
  return "";
}

//----------------------------------------------------------------------------
const char* 
DynamicLoader
::LibExtension()
{
  return ".lib";
}

//----------------------------------------------------------------------------
const char* 
DynamicLoader
::LastError()
{
  return 0;
}

} // end namespace itk

#endif

// ---------------------------------------------------------------
// 3. Implementation for Windows win32 code
#ifdef _WIN32
#include "itkWindows.h"
#define ITKDYNAMICLOADER_DEFINED 1

namespace itk
{
  
//----------------------------------------------------------------------------
LibHandle 
DynamicLoader
::OpenLibrary(const char* libname )
{
  return LoadLibrary(libname);
}

//----------------------------------------------------------------------------
int 
DynamicLoader
::CloseLibrary(LibHandle lib)
{
  return (int)FreeLibrary(lib);
}

//----------------------------------------------------------------------------
void* 
DynamicLoader
::GetSymbolAddress(LibHandle lib, const char* sym)
{ 
  return (void *)GetProcAddress(lib, sym);
}

//----------------------------------------------------------------------------
const char* 
DynamicLoader
::LibPrefix()
{ 
  return "";
}

//----------------------------------------------------------------------------
const char* 
DynamicLoader
::LibExtension()
{
  return ".dll";
}

//----------------------------------------------------------------------------
const char* 
DynamicLoader
::LastError()
{
  LPVOID lpMsgBuf;

  FormatMessage( 
		FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
		NULL,
		GetLastError(),
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
		(LPTSTR) &lpMsgBuf,
		0,
		NULL 
		);
  
  static char* str = 0;
  delete [] str;
  str = strcpy(new char[strlen((char*)lpMsgBuf)+1], (char*)lpMsgBuf);
  // Free the buffer.
  LocalFree( lpMsgBuf );
  return str;
}

} // end namespace itk

#endif

// ---------------------------------------------------------------
// 4. Implementation for default UNIX machines.
// if nothing has been defined then use this
#ifndef ITKDYNAMICLOADER_DEFINED
#define ITKDYNAMICLOADER_DEFINED
// Setup for most unix machines
#include <dlfcn.h>

namespace itk
{
  
//----------------------------------------------------------------------------
LibHandle 
DynamicLoader
::OpenLibrary(const char* libname )
{
  return dlopen(libname, RTLD_LAZY);
}

//----------------------------------------------------------------------------
int 
DynamicLoader
::CloseLibrary(LibHandle lib)
{
  return (int)dlclose(lib);
}

//----------------------------------------------------------------------------
void* 
DynamicLoader
::GetSymbolAddress(LibHandle lib, const char* sym)
{ 
  return dlsym(lib, sym);
}

//----------------------------------------------------------------------------
const char* 
DynamicLoader
::LibPrefix()
{ 
  return "lib";
}

//----------------------------------------------------------------------------
const char* 
DynamicLoader
::LibExtension()
{
  return ".so";
}

//----------------------------------------------------------------------------
const char* 
DynamicLoader
::LastError()
{
  return dlerror(); 
}

} // end namespace itk

#endif
