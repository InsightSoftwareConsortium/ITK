/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDynamicLoader.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkDynamicLoader.h"

// This file is actually 3 different implementations.
// 1. HP machines which uses shl_load
// 2. Power PC MAC which uses GetSharedLibrary
// 3. Windows which uses LoadLibrary
// 4. Most unix systems which use dlopen (default )
// Each part of the ifdef contains a complete implementation for
// the static methods of itkDynamicLoader.  


// ---------------------------------------------------------------
// 1. Implementation for HPUX  machines
#ifdef __hpux
#define ITKDYNAMICLOADER_DEFINED 1
#include <dl.h>

//----------------------------------------------------------------------------
itkLibHandle 
itkDynamicLoader
::OpenLibrary(const char* libname )
{
  return shl_load(libname, BIND_DEFERRED | DYNAMIC_PATH, 0L);
}

int 
itkDynamicLoader::
CloseLibrary(itkLibHandle lib)
{
  return 0;
}

//----------------------------------------------------------------------------
void* 
itkDynamicLoader
::GetSymbolAddress(itkLibHandle lib, const char* sym)
{ 
  void* addr;
  int status;
  
  status = shl_findsym (&lib, sym, TYPE_PROCEDURE, &addr);
  return (status < 0) ? (void*)0 : addr;
}

//----------------------------------------------------------------------------
const char* 
itkDynamicLoader
::LibPrefix()
{ 
  return "lib";
}

//----------------------------------------------------------------------------
const char* 
itkDynamicLoader
::LibExtension()
{
  return ".sl";
}

//----------------------------------------------------------------------------
const char* 
itkDynamicLoader
::LastError()
{
  return 0;
}
#endif


// ---------------------------------------------------------------
// 2. Implementation for the Power PC (MAC)
#ifdef __powerc
#define ITKDYNAMICLOADER_DEFINED 1
#include <CodeFragments.h>
#include <Errors.h>
#include <Threads.h>
#include <Timer.h>

//----------------------------------------------------------------------------
itkLibHandle 
itkDynamicLoader
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
itkDynamicLoader
::CloseLibrary(itkLibHandle lib)
{
  return 0;
}

//----------------------------------------------------------------------------
void* 
itkDynamicLoader
::GetSymbolAddress(itkLibHandle lib, const char* sym)
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
itkDynamicLoader
::LibPrefix()
{ 
  return "";
}

//----------------------------------------------------------------------------
const char* 
itkDynamicLoader
::LibExtension()
{
  return ".lib";
}

//----------------------------------------------------------------------------
const char* 
itkDynamicLoader
::LastError()
{
  return 0;
}
#endif

// ---------------------------------------------------------------
// 3. Implementation for Windows win32 code
#ifdef _WIN32
#include <windows.h>
#define ITKDYNAMICLOADER_DEFINED 1

//----------------------------------------------------------------------------
itkLibHandle 
itkDynamicLoader
::OpenLibrary(const char* libname )
{
  return LoadLibrary(libname);
}

//----------------------------------------------------------------------------
int 
itkDynamicLoader
::CloseLibrary(itkLibHandle lib)
{
  return (int)FreeLibrary(lib);
}

//----------------------------------------------------------------------------
void* 
itkDynamicLoader
::GetSymbolAddress(itkLibHandle lib, const char* sym)
{ 
  return GetProcAddress(lib, sym);
}

//----------------------------------------------------------------------------
const char* 
itkDynamicLoader
::LibPrefix()
{ 
  return "";
}

//----------------------------------------------------------------------------
const char* 
itkDynamicLoader
::LibExtension()
{
  return ".dll";
}

//----------------------------------------------------------------------------
const char* 
itkDynamicLoader
::LastError()
{
   DWORD errcode = GetLastError();
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
  
  // Free the buffer.
  LocalFree( lpMsgBuf );
  static char* str = 0;
  delete [] str;
  str = strcpy(new char[strlen((char*)lpMsgBuf)+1], (char*)lpMsgBuf);
  return str;
}

#endif

// ---------------------------------------------------------------
// 4. Implementation for default UNIX machines.
// if nothing has been defined then use this
#ifndef ITKDYNAMICLOADER_DEFINED
#define ITKDYNAMICLOADER_DEFINED
// Setup for most unix machines
#include <dlfcn.h>

//----------------------------------------------------------------------------
itkLibHandle 
itkDynamicLoader
::OpenLibrary(const char* libname )
{
  return dlopen(libname, RTLD_LAZY);
}

//----------------------------------------------------------------------------
int 
itkDynamicLoader
::CloseLibrary(itkLibHandle lib)
{
  return (int)dlclose(lib);
}

//----------------------------------------------------------------------------
void* 
itkDynamicLoader
::GetSymbolAddress(itkLibHandle lib, const char* sym)
{ 
  return dlsym(lib, sym);
}

//----------------------------------------------------------------------------
const char* 
itkDynamicLoader
::LibPrefix()
{ 
  return "lib";
}

//----------------------------------------------------------------------------
const char* 
itkDynamicLoader
::LibExtension()
{
  return ".so";
}

//----------------------------------------------------------------------------
const char* 
itkDynamicLoader
::LastError()
{
  return dlerror(); 
}
#endif
