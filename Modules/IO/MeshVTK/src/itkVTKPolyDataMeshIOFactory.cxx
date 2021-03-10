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
#include "ITKIOMeshVTKExport.h"

#include "itkVTKPolyDataMeshIO.h"
#include "itkVTKPolyDataMeshIOFactory.h"
#include "itkVersion.h"

namespace itk
{

void
VTKPolyDataMeshIOFactory::PrintSelf(std::ostream &, Indent) const
{}


VTKPolyDataMeshIOFactory ::VTKPolyDataMeshIOFactory()
{
  this->RegisterOverride(
    "itkMeshIOBase", "itkVTKPolyDataMeshIO", "VTK Polydata IO", true, CreateObjectFunction<VTKPolyDataMeshIO>::New());
}

VTKPolyDataMeshIOFactory::~VTKPolyDataMeshIOFactory() = default;


const char *
VTKPolyDataMeshIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}


const char *
VTKPolyDataMeshIOFactory::GetDescription() const
{
  return "VTK MeshIO Factory, allows the loading of VTK polydata into insight";
}


// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
static bool VTKPolyDataMeshIOFactoryHasBeenRegistered;

void ITKIOMeshVTK_EXPORT
     VTKPolyDataMeshIOFactoryRegister__Private()
{
  if (!VTKPolyDataMeshIOFactoryHasBeenRegistered)
  {
    VTKPolyDataMeshIOFactoryHasBeenRegistered = true;
    VTKPolyDataMeshIOFactory::RegisterOneFactory();
  }
}

} // end namespace itk
