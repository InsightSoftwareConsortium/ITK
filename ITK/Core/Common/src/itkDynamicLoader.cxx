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
#include "itkDynamicLoader.h"

namespace itk
{
DynamicLoader::DynamicLoader()
{}

//----------------------------------------------------------------------------
DynamicLoader::~DynamicLoader()
{}

//----------------------------------------------------------------------------
LibHandle DynamicLoader::OpenLibrary(const char *libname)
{
  return itksys::DynamicLoader::OpenLibrary(libname);
}

//----------------------------------------------------------------------------
int DynamicLoader::CloseLibrary(LibHandle lib)
{
  return itksys::DynamicLoader::CloseLibrary(lib);
}

//----------------------------------------------------------------------------
//itkSymbolPointer
void * DynamicLoader::GetSymbolAddress(LibHandle lib, const char *sym)
{
  return (void *)itksys::DynamicLoader::GetSymbolAddress(lib, sym);
}

//----------------------------------------------------------------------------
const char * DynamicLoader::LibPrefix()
{
  return itksys::DynamicLoader::LibPrefix();
}

//----------------------------------------------------------------------------
const char * DynamicLoader::LibExtension()
{
  return itksys::DynamicLoader::LibExtension();
}

//----------------------------------------------------------------------------
const char * DynamicLoader::LastError()
{
  return itksys::DynamicLoader::LastError();
}
} // end namespace itk
