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
#include "itkMGHImageIOFactory.h"
#include "itkMGHImageIO.h"
#include "itkVersion.h"

namespace itk
{
void MGHImageIOFactory::PrintSelf(std::ostream &, Indent) const
{}

MGHImageIOFactory::MGHImageIOFactory()
{
  this->RegisterOverride("itkImageIOBase",
                         "itkMGHImageIO",
                         "MGH Image IO",
                         1,
                         CreateObjectFunction<MGHImageIO>::New() );
}

MGHImageIOFactory::~MGHImageIOFactory()
{}

const char *
MGHImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
MGHImageIOFactory::GetDescription() const
{
  return "MGH ImageIO Factory, allows the loading of MGH/MGZ images into Insight";
}
// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool MGHImageIOFactoryHasBeenRegistered=false;

void MGHImageIOFactoryRegister__Private(void)
{
  if( ! MGHImageIOFactoryHasBeenRegistered )
    {
    MGHImageIOFactoryHasBeenRegistered = true;
    MGHImageIOFactory::RegisterOneFactory();
    }
}

} // end namespace itk
