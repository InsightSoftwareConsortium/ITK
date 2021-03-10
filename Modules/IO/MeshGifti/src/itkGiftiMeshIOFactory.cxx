/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "ITKIOMeshGiftiExport.h"

#include "itkGiftiMeshIO.h"
#include "itkGiftiMeshIOFactory.h"
#include "itkVersion.h"

namespace itk
{
void
GiftiMeshIOFactory::PrintSelf(std::ostream &, Indent) const
{}

GiftiMeshIOFactory::GiftiMeshIOFactory()
{
  this->RegisterOverride(
    "itkMeshIOBase", "itkGiftiMeshIO", "Gifti Mesh IO", true, CreateObjectFunction<GiftiMeshIO>::New());
}

GiftiMeshIOFactory::~GiftiMeshIOFactory() = default;

const char *
GiftiMeshIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
GiftiMeshIOFactory::GetDescription() const
{
  return "Gifti MeshIO Factory, allows the loading of Gifti meshs into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
static bool GiftiMeshIOFactoryHasBeenRegistered;

void ITKIOMeshGifti_EXPORT
     GiftiMeshIOFactoryRegister__Private()
{
  if (!GiftiMeshIOFactoryHasBeenRegistered)
  {
    GiftiMeshIOFactoryHasBeenRegistered = true;
    GiftiMeshIOFactory::RegisterOneFactory();
  }
}

} // end namespace itk
