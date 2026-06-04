/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include "IOMeshSWCExport.h"

#include "itkSWCMeshIOFactory.h"
#include "itkSWCMeshIO.h"
#include "itkVersion.h"

namespace itk
{
void
SWCMeshIOFactory::PrintSelf(std::ostream &, Indent) const
{}


SWCMeshIOFactory ::SWCMeshIOFactory()
{
  this->RegisterOverride("itkMeshIOBase", "itkSWCMeshIO", "SWC Mesh IO", true, CreateObjectFunction<SWCMeshIO>::New());
}


SWCMeshIOFactory::~SWCMeshIOFactory() = default;


const char *
SWCMeshIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}


const char *
SWCMeshIOFactory::GetDescription() const
{
  return "SWC Mesh IO Factory, allows the loading of SWC mesh into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
void IOMeshSWC_EXPORT
SWCMeshIOFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<SWCMeshIOFactory>();
}

} // end namespace itk
