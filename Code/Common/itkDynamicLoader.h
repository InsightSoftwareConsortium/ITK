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
#ifndef __itkDynamicLoader_h
#define __itkDynamicLoader_h

// Ugly stuff for library handles.
// They are different on several different OS's
#if defined(__hpux)
# include <dl.h>
ITK_NAMESPACE_BEGIN
typedef shl_t LibHandle;
ITK_NAMESPACE_END
#elif defined(_WIN32)
# include "windows.h"
ITK_NAMESPACE_BEGIN
typedef HMODULE LibHandle;
ITK_NAMESPACE_END
#elif defined(__powerpc)
ITK_NAMESPACE_BEGIN
typedef ConnectionID LibHandle;
ITK_NAMESPACE_END
#else
ITK_NAMESPACE_BEGIN
typedef void* LibHandle;
ITK_NAMESPAC_END
#endif

#include "itkObject.h"
#include "itkObjectFactory.h"

ITK_NAMESPACE_BEGIN

/** \class DynamicLoader
 * \brief Portable loading of dynamic libraries or dll's.
 *
 * DynamicLoader provides a portable interface to loading dynamic 
 * libraries or dll's into a process. 
 */


class ITK_EXPORT DynamicLoader : public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef DynamicLoader       Self;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(DynamicLoader,Object);

  /**
   * Load a dynamic library into the current process.
   * The returned LibHandle can be used to access the symbols in the 
   * library.
   */
  static LibHandle OpenLibrary(const char*);

  /**
   * Attempt to detach a dynamic library from the
   * process.  A value of true is returned if it is sucessful.
   */
  static int CloseLibrary(LibHandle);
  
  /**
   * Find the address of the symbol in the given library.
   */
  static void* GetSymbolAddress(LibHandle, const char*);

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
  DynamicLoader() {};
  ~DynamicLoader() {};
  DynamicLoader(const Self&) {}
  void operator=(const Self&) {}

};

ITK_NAMESPACE_END
  
#endif
