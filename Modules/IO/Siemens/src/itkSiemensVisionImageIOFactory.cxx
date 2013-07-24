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
#include "itkSiemensVisionImageIOFactory.h"
#include "itkSiemensVisionImageIO.h"
#include "itkVersion.h"

namespace itk
{
void SiemensVisionImageIOFactory::PrintSelf(std::ostream &, Indent) const
{}

SiemensVisionImageIOFactory::SiemensVisionImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkSiemensVisionImageIO",
                          "SiemensVision Image IO",
                          1,
                          CreateObjectFunction< SiemensVisionImageIO >::New() );
}

SiemensVisionImageIOFactory::~SiemensVisionImageIOFactory()
{}

const char *
SiemensVisionImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
SiemensVisionImageIOFactory::GetDescription() const
{
  return "SiemensVision ImageIO Factory, allows the loading of SiemensVision images into insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool SiemensVisionImageIOFactoryHasBeenRegistered;

void ITKIOSiemens_EXPORT SiemensVisionImageIOFactoryRegister__Private(void)
{
  if( ! SiemensVisionImageIOFactoryHasBeenRegistered )
    {
    SiemensVisionImageIOFactoryHasBeenRegistered = true;
    SiemensVisionImageIOFactory::RegisterOneFactory();
    }
}

} // end namespace itk
