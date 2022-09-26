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
#include "ITKIOMeshOFFExport.h"

#include "itkOFFMeshIO.h"
#include "itkOFFMeshIOFactory.h"
#include "itkVersion.h"

namespace itk
{
void
OFFMeshIOFactory::PrintSelf(std::ostream &, Indent) const
{}

OFFMeshIOFactory::OFFMeshIOFactory()
{
  this->RegisterOverride("itkMeshIOBase", "itkOFFMeshIO", "OFF Mesh IO", true, CreateObjectFunction<OFFMeshIO>::New());
}

OFFMeshIOFactory::~OFFMeshIOFactory() = default;

const char *
OFFMeshIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
OFFMeshIOFactory::GetDescription() const
{
  return "OFF Mesh IO Factory, allows the loading of OFF mesh into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
void ITKIOMeshOFF_EXPORT
     OFFMeshIOFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<OFFMeshIOFactory>();
}

} // end namespace itk
