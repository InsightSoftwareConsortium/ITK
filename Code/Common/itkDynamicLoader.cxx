/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDynamicLoader.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
#ifdef __APPLE__
#define ITKDYNAMICLOADER_DEFINED 
#include <mach-o/dyld.h>

namespace itk
{

//----------------------------------------------------------------------------
LibHandle 
DynamicLoader
::OpenLibrary(const char* libname )
{
  NSObjectFileImageReturnCode rc;
  NSObjectFileImage image;
  
  rc = NSCreateObjectFileImageFromFile(libname, &image);
  return NSLinkModule(image, libname, TRUE);
}

//----------------------------------------------------------------------------
int 
DynamicLoader
::CloseLibrary( LibHandle ) // argument expected (LibHandle lib)
{
  return 0;
}

//----------------------------------------------------------------------------
void* 
DynamicLoader
::GetSymbolAddress(LibHandle , const char* sym) // (LibHandle lib)
{ 
  void *result=0;
  if(NSIsSymbolNameDefined(sym))
    {
    NSSymbol symbol= NSLookupAndBindSymbol(sym);
    if(symbol)
      {
      result = NSAddressOfSymbol(symbol);
      }
    }
  return result;
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
  return ".dylib";
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
  if (lib)
    {
    return (int)dlclose(lib);
    }
  else
    {
    return 0;
    }
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
