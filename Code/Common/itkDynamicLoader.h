/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDynamicLoader.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkDynamicLoader provides a portable interface to loading dynamic 
 * libraries or dll's into a process. 
 */

#ifndef __itkDynamicLoader_h
#define __itkDynamicLoader_h

// Ugly stuff for library handles.
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

#include "itkObject.h"

class ITK_EXPORT itkDynamicLoader : public itkObject
{
public:
  static itkDynamicLoader *New() 
    {return new itkDynamicLoader;}
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(itkDynamicLoader,itkObject);

  /**
   * Load a dynamic library into the current process.
   * The returned itkLibHandle can be used to access the symbols in the 
   * library.
   */
  static itkLibHandle OpenLibrary(const char*);

  /**
   * Attempt to detach a dynamic library from the
   * process.  A value of true is returned if it is sucessful.
   */
  static int CloseLibrary(itkLibHandle);
  
  /**
   * Find the address of the symbol in the given library.
   */
  static void* GetSymbolAddress(itkLibHandle, const char*);

  /**
   * Return the library prefix for the given architecture
   */
  static const char* LibPrefix();

  /**
   * Return the library extension for the given architecture.
   */
  static const char* LibExtension();

  /**
   * Return the last error produced from a calls made on this class.
   */
  static const char* LastError();
  
protected:
  itkDynamicLoader() {};
  ~itkDynamicLoader() {};
  itkDynamicLoader(const itkDynamicLoader&) {};
  void operator=(const itkDynamicLoader&) {};

};

#endif
