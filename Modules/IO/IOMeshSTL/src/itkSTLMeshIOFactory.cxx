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

#include "IOMeshSTLExport.h"
#include "itkSTLMeshIO.h"
#include "itkSTLMeshIOFactory.h"
#include "itkVersion.h"

namespace itk
{
void
STLMeshIOFactory ::PrintSelf(std::ostream &, Indent) const
{}

STLMeshIOFactory ::STLMeshIOFactory()
{
  this->RegisterOverride("itkMeshIOBase", "itkSTLMeshIO", "STL IO", true, CreateObjectFunction<STLMeshIO>::New());
}

STLMeshIOFactory ::~STLMeshIOFactory() = default;

const char *
STLMeshIOFactory ::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
STLMeshIOFactory ::GetDescription() const
{
  return "STL MeshIO Factory, allows the loading of STL QuadEdgeMesh data into ITK";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
static bool STLMeshIOFactoryHasBeenRegistered;

void IOMeshSTL_EXPORT
STLMeshIOFactoryRegister__Private()
{
  if (!STLMeshIOFactoryHasBeenRegistered)
  {
    STLMeshIOFactoryHasBeenRegistered = true;
    STLMeshIOFactory::RegisterOneFactory();
  }
}
} // end namespace itk
