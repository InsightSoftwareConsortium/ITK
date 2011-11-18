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

#include "itkOFFMeshIO.h"
#include "itkOFFMeshIOFactory.h"
#include "itkVersion.h"

namespace itk
{
void
OFFMeshIOFactory
::PrintSelf(std::ostream &, Indent) const
{}

OFFMeshIOFactory
::OFFMeshIOFactory()
{
  this->RegisterOverride( "itkMeshIOBase",
                         "itkOFFMeshIO",
                         "OFF Mesh IO",
                         1,
                         CreateObjectFunction< OFFMeshIO >::New() );
}

OFFMeshIOFactory
::~OFFMeshIOFactory()
{}

const char *
OFFMeshIOFactory
::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
OFFMeshIOFactory
::GetDescription() const
{
  return "OFF Mesh IO Factory, allows the loading of OFF mesh into insight";
}
} // end namespace itk
