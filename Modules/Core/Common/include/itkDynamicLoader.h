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
#ifndef itkDynamicLoader_h
#define itkDynamicLoader_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itksys/DynamicLoader.hxx"

namespace itk
{
/** \class DynamicLoader
 * \brief Portable loading of dynamic libraries or dll's.
 *
 * DynamicLoader provides a portable interface to loading dynamic
 * libraries or dll's into a process.
 *
 * \ingroup OSSystemObjects
 * \ingroup ITKCommon
 */

//BTX
typedef itksys::DynamicLoader::LibraryHandle LibHandle;
// Cannot use this as this is a void (*)() but ITK old API used to be void*
typedef itksys::DynamicLoader::SymbolPointer SymbolPointer;
//ETX

class ITKCommon_EXPORT DynamicLoader:public Object
{
public:
  /** Standard class typedefs. */
  typedef DynamicLoader              Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DynamicLoader, Object);

  /** Load a dynamic library into the current process.
   * The returned LibHandle can be used to access the symbols in the
   * library. */
  static LibHandle OpenLibrary(const char *);

  /** Attempt to detach a dynamic library from the
   * process.  A value of true is returned if it is successful. */
  static int CloseLibrary(LibHandle);

  /** Find the address of the symbol in the given library. */
  static void *GetSymbolAddress(LibHandle, const char *);

  /** Return the library prefix for the given architecture */
  static const char * LibPrefix();

  /** Return the library extension for the given architecture. */
  static const char * LibExtension();

  /** Return the last error produced from a calls made on this class. */
  static const char * LastError();

protected:
  DynamicLoader();
  ~DynamicLoader() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DynamicLoader);
};
} // end namespace itk

#endif
