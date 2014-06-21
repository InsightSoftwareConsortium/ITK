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
#include "itkNiftiImageIOFactory.h"
#include "itkNiftiImageIO.h"
#include "itkVersion.h"

namespace itk
{
void NiftiImageIOFactory::PrintSelf(std::ostream &, Indent) const
{}

NiftiImageIOFactory::NiftiImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkNiftiImageIO",
                          "Nifti Image IO",
                          1,
                          CreateObjectFunction< NiftiImageIO >::New() );
}

NiftiImageIOFactory::~NiftiImageIOFactory()
{}

const char *
NiftiImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
NiftiImageIOFactory::GetDescription() const
{
  return "Nifti ImageIO Factory, allows the loading of Nifti images into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool NiftiImageIOFactoryHasBeenRegistered;

void ITKIONIFTI_EXPORT NiftiImageIOFactoryRegister__Private(void)
{
  if( ! NiftiImageIOFactoryHasBeenRegistered )
    {
    NiftiImageIOFactoryHasBeenRegistered = true;
    NiftiImageIOFactory::RegisterOneFactory();
    }
}

} // end namespace itk
