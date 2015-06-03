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
#include "itkGDCMImageIOFactory.h"
#include "itkGDCMImageIO.h"
#include "itkVersion.h"

namespace itk
{
GDCMImageIOFactory::GDCMImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkGDCMImageIO",
                          "GDCM Image IO",
                          1,
                          CreateObjectFunction< GDCMImageIO >::New() );
}

GDCMImageIOFactory::~GDCMImageIOFactory()
{}

const char * GDCMImageIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char * GDCMImageIOFactory::GetDescription() const
{
  return "GDCM ImageIO Factory, allows the loading of DICOM images into Insight";
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.

static bool GDCMImageIOFactoryHasBeenRegistered;

void ITKIOGDCM_EXPORT GDCMImageIOFactoryRegister__Private(void)
{
  if( ! GDCMImageIOFactoryHasBeenRegistered )
    {
    GDCMImageIOFactoryHasBeenRegistered = true;
    GDCMImageIOFactory::RegisterOneFactory();
    }
}

} // end namespace itk
