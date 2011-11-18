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

#include "itkFreeSurferAsciiMeshIO.h"
#include "itkFreeSurferAsciiMeshIOFactory.h"
#include "itkVersion.h"

namespace itk
{
void
FreeSurferAsciiMeshIOFactory
::PrintSelf(std::ostream &, Indent) const
{}

FreeSurferAsciiMeshIOFactory
::FreeSurferAsciiMeshIOFactory()
{
  this->RegisterOverride( "itkMeshIOBase",
                         "itkFreeSurferAsciiMeshIO",
                         "Freesurfer Mesh IO",
                         1,
                         CreateObjectFunction< FreeSurferAsciiMeshIO >::New() );
}

FreeSurferAsciiMeshIOFactory
::~FreeSurferAsciiMeshIOFactory()
{}

const char *
FreeSurferAsciiMeshIOFactory
::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
FreeSurferAsciiMeshIOFactory
::GetDescription() const
{
  return "FreeSurfer ASCII Mesh IO Factory, allows the loading of FreeSurfer Ascii mesh into insight";
}
} // end namespace itk
