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
#include "ITKIOMeshOBJExport.h"

#include "itkOBJMeshIO.h"
#include "itkOBJMeshIOFactory.h"
#include "itkVersion.h"

namespace itk
{
void
OBJMeshIOFactory::PrintSelf(std::ostream &, Indent) const
{}

OBJMeshIOFactory::OBJMeshIOFactory()
{
  this->RegisterOverride("itkMeshIOBase", "itkOBJMeshIO", "OBJ Mesh IO", true, CreateObjectFunction<OBJMeshIO>::New());
}

OBJMeshIOFactory::~OBJMeshIOFactory() = default;

const char *
OBJMeshIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
OBJMeshIOFactory::GetDescription() const
{
  return "OBJ Mesh IO Factory, allows the loading of OBJ mesh into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
void ITKIOMeshOBJ_EXPORT
     OBJMeshIOFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<OBJMeshIOFactory>();
}

} // end namespace itk
