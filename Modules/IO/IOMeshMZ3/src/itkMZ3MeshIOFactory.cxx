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
#include "IOMeshMZ3Export.h"

#include "itkMZ3MeshIOFactory.h"
#include "itkMZ3MeshIO.h"
#include "itkVersion.h"

namespace itk
{
void
MZ3MeshIOFactory::PrintSelf(std::ostream &, Indent) const
{}


MZ3MeshIOFactory::MZ3MeshIOFactory()
{
  this->RegisterOverride("itkMeshIOBase", "itkMZ3MeshIO", "MZ3 Mesh IO", true, CreateObjectFunction<MZ3MeshIO>::New());
}


MZ3MeshIOFactory::~MZ3MeshIOFactory() = default;


const char *
MZ3MeshIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}


const char *
MZ3MeshIOFactory::GetDescription() const
{
  return "MZ3 Mesh IO Factory, allows the loading of MZ3 mesh into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
void IOMeshMZ3_EXPORT
MZ3MeshIOFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<MZ3MeshIOFactory>();
}

} // end namespace itk
