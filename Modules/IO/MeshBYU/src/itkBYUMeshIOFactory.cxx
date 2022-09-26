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
#include "ITKIOMeshBYUExport.h"

#include "itkBYUMeshIOFactory.h"
#include "itkBYUMeshIO.h"
#include "itkVersion.h"

namespace itk
{
void
BYUMeshIOFactory::PrintSelf(std::ostream &, Indent) const
{}


BYUMeshIOFactory::BYUMeshIOFactory()
{
  this->RegisterOverride("itkMeshIOBase", "itkBYUMeshIO", "BYU Mesh IO", true, CreateObjectFunction<BYUMeshIO>::New());
}


BYUMeshIOFactory::~BYUMeshIOFactory() = default;


const char *
BYUMeshIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}


const char *
BYUMeshIOFactory::GetDescription() const
{
  return "BYU Mesh IO Factory, allows the loading of BYU mesh into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
void ITKIOMeshBYU_EXPORT
     BYUMeshIOFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<BYUMeshIOFactory>();
}

} // end namespace itk
