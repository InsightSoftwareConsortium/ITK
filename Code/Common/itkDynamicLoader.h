/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDynamicLoader.h
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
#ifndef __itkDynamicLoader_h
#define __itkDynamicLoader_h

#include "itkMacro.h"

// Ugly stuff for library handles.
// They are different on several different OS's
#if defined(__hpux)
# include <dl.h>
namespace itk
{
typedef shl_t LibHandle;
} // end namespace itk
#elif defined(_WIN32)
#include "itkWindows.h"

namespace itk
{
typedef HMODULE LibHandle;
} // end namespace itk
#elif defined(__powerpc)
namespace itk
{
typedef ConnectionID LibHandle;
} // end namespace itk
#else
namespace itk
{
typedef void* LibHandle;
} // end namespace itk
#endif

#include "itkObject.h"
#include "itkObjectFactory.h"

namespace itk
{

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
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
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

} // end namespace itk
  
#endif
