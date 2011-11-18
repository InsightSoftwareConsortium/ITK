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

#include "itkOBJMeshIO.h"
#include "itkOBJMeshIOFactory.h"
#include "itkVersion.h"

namespace itk
{
void
OBJMeshIOFactory
::PrintSelf(std::ostream &, Indent) const
{}

OBJMeshIOFactory
::OBJMeshIOFactory()
{
  this->RegisterOverride( "itkMeshIOBase",
                         "itkOBJMeshIO",
                         "OBJ Mesh IO",
                         1,
                         CreateObjectFunction< OBJMeshIO >::New() );
}

OBJMeshIOFactory
::~OBJMeshIOFactory()
{}

const char *
OBJMeshIOFactory
::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
OBJMeshIOFactory
::GetDescription() const
{
  return "OBJ Mesh IO Factory, allows the loading of OBJ mesh into insight";
}
} // end namespace itk
